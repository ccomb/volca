"""Server lifecycle management for VoLCA."""

import os
import shutil
import subprocess
import time
from pathlib import Path

import requests

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:
    import tomli as tomllib  # type: ignore[no-redef]

from . import _download


class Server:
    """Manages the VoLCA server process.

    Usage::

        with Server(config="volca.toml") as srv:
            client = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
            activities = client.search_activities(name="at plant")
    """

    def __init__(self, config: str = "volca.toml", port: int = 0, binary: str = "volca"):
        """Configure (but don't start) a managed VoLCA server.

        Args:
            config: Path to the engine TOML. Read for ``server.port`` and
                ``server.password``. Missing file is tolerated — defaults
                are used.
            port: Override the port. ``0`` means "read from config (or 8080)".
            binary: Name or path of the volca binary. Looked up on PATH if
                not absolute.
        """
        self.config = config
        self.binary = binary
        self._process: subprocess.Popen | None = None

        # Read port and password from config
        cfg = self._read_config()
        server_cfg = cfg.get("server", {})
        self.port = port or server_cfg.get("port", 8080)
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
        """Find the volca binary.

        Resolution order:
          1. ``self.binary`` if it is an existing path.
          2. The shared install root (``platformdirs.user_data_dir``) —
             populated by :func:`volca.download`, ``install.sh``, or
             ``install.ps1`` interchangeably.
          3. ``shutil.which(self.binary)`` — PATH lookup, including the
             ``~/.local/bin/volca`` shim that ``install.sh`` drops.
          4. ``./volca`` / ``./dist/volca`` for ad-hoc dev trees.
        """
        if Path(self.binary).exists():
            return self.binary
        installed = _download.installed_binary()
        if installed is not None:
            return str(installed)
        found = shutil.which(self.binary)
        if found:
            return found
        for candidate in ["./volca", "./dist/volca"]:
            if Path(candidate).exists():
                return candidate
        raise FileNotFoundError(
            f"Cannot find '{self.binary}' binary. "
            "Run volca.download(), set binary= parameter, or add volca to PATH."
        )

    def _subprocess_env(self) -> dict:
        """Subprocess env for the spawned engine.

        When the data bundle has been installed into the shared install root,
        export VOLCA_DATA_DIR so the engine resolves "data/flows.csv" and
        friends against the bundle instead of the engine's CWD.
        """
        env = os.environ.copy()
        if "VOLCA_DATA_DIR" not in env:
            installed = _download.installed_data_dir()
            if installed is not None:
                env["VOLCA_DATA_DIR"] = str(installed)
        return env

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
        """Spawn the engine process if it is not already serving, and wait until ready.

        Args:
            idle_timeout: Seconds without an HTTP request before the engine
                shuts itself down. Default 5 min.
            wait_timeout: How long to poll for the server to become healthy
                before raising :class:`TimeoutError`.

        No-op if a healthy server is already reachable on ``base_url``.
        """
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
            env=self._subprocess_env(),
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
