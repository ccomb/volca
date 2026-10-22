"""Server lifecycle management for VoLCA."""

import json
import os
import shutil
import subprocess
import tempfile
import threading
import time
from collections.abc import Mapping
from pathlib import Path
from typing import Literal

import requests

try:
    import tomllib  # Python 3.11+
except ModuleNotFoundError:
    import tomli as tomllib  # type: ignore[no-redef]

from . import _compat, _download


def declared_config(databases: Mapping[str, str | Path], methods: Mapping[str, str | Path]) -> str:
    """The engine TOML declaring each database and method collection by name and path.

    Paths are resolved and written as JSON strings, which TOML reads as its own
    (Windows backslashes included). A database is declared unloaded, the
    engine's default, so a start does not wait on it.
    """

    def entry(table: str, name: str, path: str | Path) -> str:
        return f"[[{table}]]\nname = {quoted(name)}\npath = {quoted(str(Path(path).resolve()))}\n"

    def quoted(text: str) -> str:
        # Unescaped beyond ASCII: an escaped emoji is a surrogate pair, which TOML refuses.
        return json.dumps(text, ensure_ascii=False)

    return "\n".join(
        [entry("databases", n, p) for n, p in databases.items()]
        + [entry("methods", n, p) for n, p in methods.items()]
    )


class Server:
    """Manages the VoLCA server process.

    Usage::

        with Server(config="volca.toml") as srv:
            client = Client(base_url=srv.base_url, db="agribalyse-3.2", password=srv.password)
            activities = client.search_activities(name="at plant")

    Or with no TOML of one's own, the databases and methods named by path::

        with Server(databases={"agb": "agribalyse.zip"}, methods={"EF 3.1": "ef31.zip"}) as srv:
            client = Client(base_url=srv.base_url, db="agb")
            client.load_database("agb")
    """

    def __init__(
        self,
        config: str | None = "volca.toml",
        port: int | Literal["auto"] = 0,
        binary: str = "volca",
        databases: Mapping[str, str | Path] | None = None,
        methods: Mapping[str, str | Path] | None = None,
    ):
        """Configure (but don't start) a managed VoLCA server.

        Args:
            config: Path to the engine TOML, read for ``server.port`` and
                ``server.password``. ``None`` starts the engine without any
                config file: built-in defaults, no databases (needs an
                engine >= v0.9.3). A path that does not exist makes
                :meth:`start` fail loudly: a typo must never silently become
                "all defaults".
            port: Override the configured port. ``"auto"`` asks the engine to
                bind an OS-assigned free port atomically. ``0`` reads the
                config (or uses 8080), preserving the original API.
            binary: Name or path of the volca binary. Looked up on PATH if
                not absolute. Left at ``"volca"``, ``VOLCA_BINARY`` names a
                local build, which then needs ``VOLCA_DATA_DIR``, the data
                directory of the same build.
            databases: Databases to declare, name to path, in place of a
                ``config``. They start unloaded: load each with
                :meth:`Client.load_database`. The engine then takes a free
                port, and its TOML lives in a temporary directory.
            methods: Method collections to declare, name to path, as
                ``databases``.
        """
        if databases is not None or methods is not None:
            if config not in ("volca.toml", None):
                raise ValueError("pass either config or databases/methods, not both")
            if port not in (0, "auto"):
                raise ValueError("an engine started on databases/methods takes a free port: leave port out")
            self._declared = tempfile.TemporaryDirectory(ignore_cleanup_errors=True)
            config = str(Path(self._declared.name) / "volca.toml")
            Path(config).write_text(declared_config(databases or {}, methods or {}), encoding="utf-8")
            port = "auto"
        self.config = config
        self.binary = binary
        self._process: subprocess.Popen | None = None

        # Read port and password from config
        cfg = self._read_config()
        server_cfg = cfg.get("server", {})
        self.port = 0 if port == "auto" else port or server_cfg.get("port", 8080)
        self.password = server_cfg.get("password", "")

    @property
    def base_url(self) -> str:
        """``http://localhost:<port>``, pass to :class:`Client(base_url=…)`.

        Always loopback: the managed server only listens locally.
        """
        return f"http://localhost:{self.port}"

    def _read_config(self) -> dict:
        """Read the TOML config file; ``{}`` when running config-less.

        A missing file also reads as ``{}`` here (port and password get
        their defaults), but :meth:`start` still refuses to spawn against a
        path that does not exist.
        """
        if self.config is None:
            return {}
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
          0. ``VOLCA_BINARY`` when ``self.binary`` is left at ``"volca"``,
             refused without ``VOLCA_DATA_DIR`` (the released data would not
             match a local build).
          1. ``self.binary`` if it is an existing file.
          2. The shared install root (``platformdirs.user_data_dir``),
             populated by :func:`volca.download`, ``install.sh``, or
             ``install.ps1`` interchangeably.
          3. ``shutil.which(self.binary)``: PATH lookup, including the
             ``~/.local/bin/volca`` shim that ``install.sh`` drops.
          4. ``./volca`` / ``./dist/volca`` for ad-hoc dev trees.

        The check is ``is_file``, not ``exists``: a directory named ``volca``
        in the working tree (e.g. a source checkout) must not shadow the real
        binary and get handed to ``Popen`` as an unexecutable path.
        """
        if self.binary == "volca" and "VOLCA_BINARY" in os.environ:
            if "VOLCA_DATA_DIR" not in os.environ:
                raise RuntimeError("VOLCA_BINARY needs VOLCA_DATA_DIR, the data directory of the same build")
            return os.environ["VOLCA_BINARY"]
        if Path(self.binary).is_file():
            return self.binary
        installed = _download.installed_binary()
        if installed is not None:
            return str(installed)
        found = shutil.which(self.binary)
        if found:
            return found
        for candidate in ["./volca", "./dist/volca"]:
            if Path(candidate).is_file():
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
        """Health check: GET /api/v1/db, return True if 200."""
        try:
            r = requests.get(
                f"{self.base_url}/api/v1/db",
                headers=self._auth_headers(),
                timeout=2,
            )
            return r.status_code == 200
        except requests.ConnectionError:
            return False

    def _check_wire(self) -> None:
        """Verify the running engine speaks a wire pyvolca understands.

        Local import of :class:`Client` keeps the server/client modules free of
        an import cycle. ``get_version`` is ungated, so this works even against
        an incompatible engine (the point is to fail with a clear message).
        """
        from .client import Client

        _compat.check(Client(self.base_url, password=self.password).get_version())

    def _early_exit(self, code: int, doing: str) -> RuntimeError:
        """Fail-fast error for an engine that died before ``doing``.

        The likeliest config-less cause is an engine too old to run without
        ``--config``, hence the version hint.
        """
        hint = (
            " Running without a config file needs an engine >= v0.9.3."
            if self.config is None
            else ""
        )
        return RuntimeError(f"Engine exited with code {code} before {doing}.{hint}")

    def _await_bound_port(self, wait_timeout: int) -> None:
        """Read the engine's machine-readable port after it has bound port 0."""
        process = self._process
        if process is None or process.stdout is None:
            raise RuntimeError("dynamic-port server has no stdout pipe")

        announced: list[str] = []

        def read_stdout() -> None:
            assert process.stdout is not None
            for line in process.stdout:
                if line.startswith("VOLCA_PORT="):
                    announced.append(line.removeprefix("VOLCA_PORT=").strip())
                    return

        # The engine's death closes stdout, which ends the thread; the join
        # carries the timeout. No need to watch the process separately.
        reader = threading.Thread(target=read_stdout, daemon=True)
        reader.start()
        reader.join(wait_timeout)
        if reader.is_alive():
            raise TimeoutError(f"Server did not report its bound port within {wait_timeout}s")
        if not announced:
            # EOF on stdout without an announcement means the engine died.
            raise self._early_exit(process.wait(timeout=5), "reporting its bound port")
        raw = announced[0]
        port = int(raw) if raw.isdecimal() else 0
        if not 1 <= port <= 65535:
            raise RuntimeError(f"VoLCA reported an invalid port: {raw}")
        self.port = port

    def start(self, idle_timeout: int = 1200, wait_timeout: int = 120) -> None:
        """Spawn the engine process if it is not already serving, and wait until ready.

        Args:
            idle_timeout: Seconds without use before the engine shuts itself
                down. Default 20 min: a script's own work between two
                requests (reading a workbook, drawing its results) counts as
                idleness, and outlasted 5. An API request or a matrix solve counts
                as use; an MCP client merely staying connected does not.
            wait_timeout: How long to poll for the server to become healthy
                before raising :class:`TimeoutError`.

        No-op if a healthy server is already reachable on ``base_url``.
        """
        dynamic_port = self.port == 0
        if not dynamic_port and self.is_alive():
            # A server is already up; we didn't spawn it, so verify the wire
            # but leave it running on a mismatch; it isn't ours to stop.
            self._check_wire()
            return

        binary = self._find_binary()
        if self.config is not None and not Path(self.config).is_file():
            raise FileNotFoundError(
                f"Config file not found: {self.config!r}. Pass config=None to "
                "run on the engine's built-in defaults (engine >= v0.9.3)."
            )
        cmd = [binary]
        if self.config is not None:
            cmd += ["--config", self.config]
        cmd += [
            "server",
            "--port", str(self.port),
            "--idle-timeout", str(idle_timeout),
        ]
        if dynamic_port:
            cmd.append("--desktop")
        self._process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE if dynamic_port else subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            env=self._subprocess_env(),
            text=True,
        )

        if dynamic_port:
            try:
                self._await_bound_port(wait_timeout)
            except Exception:
                self.stop()
                raise

        # Poll until server is ready
        deadline = time.monotonic() + wait_timeout
        while time.monotonic() < deadline:
            if self.is_alive():
                # We spawned this engine: if it speaks an incompatible wire,
                # tear it down rather than leave an unusable process running.
                try:
                    self._check_wire()
                except Exception:
                    self.stop()
                    raise
                return
            code = self._process.poll()
            if code is not None:
                # Fail now, not at the readiness timeout.
                self._process = None
                raise self._early_exit(code, "becoming ready")
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
        except requests.RequestException:
            # an engine of ours that does not answer in time is stopped below all the
            # same, and raising would throw away the caller's finished work; one we
            # did not start would keep running, which the caller must hear
            if self._process is None:
                raise
        if self._process:
            self._process.terminate()
            try:
                self._process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                self._process.kill()
                self._process.wait(timeout=10)
            self._process = None

    def __enter__(self) -> "Server":
        self.start()
        return self

    def __exit__(self, *_) -> None:
        self.stop()
