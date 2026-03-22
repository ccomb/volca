"""Server lifecycle management for VoLCA."""

import shutil
import subprocess
import time
from pathlib import Path

import requests

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:
    import tomli as tomllib  # type: ignore[no-redef]


class Server:
    """Manages the VoLCA server process.

    Usage::

        with Server(config="volca.toml") as srv:
            client = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
            activities = client.search_activities(name="at plant")
    """

    def __init__(self, config: str = "volca.toml", port: int = 0, binary: str = "volca"):
        self.config = config
        self.binary = binary
        self._process: subprocess.Popen | None = None

        # Read port and password from config
        cfg = self._read_config()
        server_cfg = cfg.get("server", {})
        self.port = port or server_cfg.get("port", 8081)
        self.password = server_cfg.get("password", "")

    @property
    def base_url(self) -> str:
        return f"http://localhost:{self.port}"

    def _read_config(self) -> dict:
        """Read the TOML config file."""
        try:
            with open(self.config, "rb") as f:
                return tomllib.load(f)
        except FileNotFoundError:
            return {}

    def _auth_headers(self) -> dict:
        if self.password:
            return {"Authorization": f"Bearer {self.password}"}
        return {}

    def _find_binary(self) -> str:
        """Find the volca binary: explicit path, package bin/, or PATH."""
        if Path(self.binary).exists():
            return self.binary
        found = shutil.which(self.binary)
        if found:
            return found
        # Try common locations
        for candidate in ["./volca", "./dist/volca"]:
            if Path(candidate).exists():
                return candidate
        raise FileNotFoundError(
            f"Cannot find '{self.binary}' binary. "
            "Set binary= parameter or add volca to PATH."
        )

    def is_alive(self) -> bool:
        """Health check — GET /api/v1/db, return True if 200."""
        try:
            r = requests.get(
                f"{self.base_url}/api/v1/db",
                headers=self._auth_headers(),
                timeout=2,
            )
            return r.status_code == 200
        except requests.ConnectionError:
            return False

    def start(self, idle_timeout: int = 300, wait_timeout: int = 120) -> None:
        """Start server if not running. Wait until ready."""
        if self.is_alive():
            return

        binary = self._find_binary()
        cmd = [
            binary,
            "--config", self.config,
            "server",
            "--port", str(self.port),
            "--idle-timeout", str(idle_timeout),
        ]
        self._process = subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

        # Poll until server is ready
        deadline = time.monotonic() + wait_timeout
        while time.monotonic() < deadline:
            if self.is_alive():
                return
            time.sleep(0.5)

        raise TimeoutError(
            f"Server did not become ready within {wait_timeout}s"
        )

    def stop(self) -> None:
        """Stop the server via shutdown endpoint, then terminate process."""
        try:
            requests.post(
                f"{self.base_url}/api/v1/shutdown",
                headers=self._auth_headers(),
                timeout=5,
            )
        except requests.ConnectionError:
            pass
        if self._process:
            self._process.terminate()
            self._process.wait(timeout=10)
            self._process = None

    def __enter__(self) -> "Server":
        self.start()
        return self

    def __exit__(self, *_) -> None:
        self.stop()
