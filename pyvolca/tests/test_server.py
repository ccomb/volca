"""Offline tests for volca.server.Server binary resolution.

No engine is spawned: these exercise _find_binary's resolution order,
which must not be fooled by a directory that happens to share the binary
name (the common case: running a script from a source checkout that has a
``volca/`` package directory in the working tree).
"""

from __future__ import annotations

import io
import subprocess
from pathlib import Path
from unittest import mock

import pytest
import requests

from volca import _download
from volca.server import Server


def test_find_binary_skips_same_named_directory(tmp_path: Path, monkeypatch):
    """A ``volca`` *directory* in cwd must not be returned as the binary."""
    (tmp_path / "volca").mkdir()
    monkeypatch.chdir(tmp_path)
    installed = tmp_path / "install" / "volca"
    installed.parent.mkdir()
    installed.write_bytes(b"")
    with mock.patch.object(_download, "installed_binary", return_value=installed):
        srv = Server(config="absent.toml", binary="volca")
        assert srv._find_binary() == str(installed)


def test_find_binary_devtree_skips_same_named_directory(tmp_path: Path, monkeypatch):
    """Dev-tree fallback: a ``./volca`` directory must not shadow ``./dist/volca``."""
    (tmp_path / "volca").mkdir()
    (tmp_path / "dist").mkdir()
    (tmp_path / "dist" / "volca").write_bytes(b"")
    monkeypatch.chdir(tmp_path)
    with mock.patch.object(_download, "installed_binary", return_value=None), \
         mock.patch("volca.server.shutil.which", return_value=None):
        srv = Server(config="absent.toml", binary="volca")
        assert srv._find_binary() == "./dist/volca"


def test_find_binary_accepts_explicit_file(tmp_path: Path):
    """An explicit path to a real file is returned as-is."""
    binary = tmp_path / "volca"
    binary.write_bytes(b"")
    srv = Server(config="absent.toml", binary=str(binary))
    assert srv._find_binary() == str(binary)


def test_find_binary_raises_when_nothing_found(tmp_path: Path, monkeypatch):
    """No file, no install, no PATH, no dev tree → clear FileNotFoundError."""
    monkeypatch.chdir(tmp_path)
    with mock.patch.object(_download, "installed_binary", return_value=None), \
         mock.patch("volca.server.shutil.which", return_value=None):
        srv = Server(config="absent.toml", binary="volca")
        with pytest.raises(FileNotFoundError):
            srv._find_binary()


def test_auto_port_preserves_explicit_zero_compatibility(tmp_path: Path):
    config = tmp_path / "volca.toml"
    config.write_text("[server]\nport = 8123\n")

    assert Server(config=str(config)).port == 8123
    assert Server(config=str(config), port=0).port == 8123
    assert Server(config=str(config), port="auto").port == 0


def test_await_bound_port_accepts_engine_announcement():
    process = mock.Mock()
    process.stdout = io.StringIO("VOLCA_PORT=43123\n")
    srv = Server(config="absent.toml", port="auto")
    srv._process = process

    srv._await_bound_port(wait_timeout=1)

    assert srv.port == 43123


@pytest.mark.parametrize(
    "line", ["VOLCA_PORT=0\n", "VOLCA_PORT=70000\n", "VOLCA_PORT=garbage\n", "VOLCA_PORT=²\n"]
)
def test_await_bound_port_rejects_invalid_port(line: str):
    process = mock.Mock()
    process.stdout = io.StringIO(line)
    srv = Server(config="absent.toml", port="auto")
    srv._process = process

    with pytest.raises(RuntimeError, match="invalid port"):
        srv._await_bound_port(wait_timeout=1)


def test_await_bound_port_reports_early_exit():
    process = mock.Mock()
    process.stdout = io.StringIO("some unrelated log line\n")
    process.wait.return_value = 1
    srv = Server(config="absent.toml", port="auto")
    srv._process = process

    with pytest.raises(RuntimeError, match="code 1 before reporting"):
        srv._await_bound_port(wait_timeout=1)


def test_await_bound_port_early_exit_hints_config_less():
    # port="auto" is the canonical config-less invocation; an engine too old
    # to run without --config must get the version hint on this path too.
    process = mock.Mock()
    process.stdout = io.StringIO("")
    process.wait.return_value = 1
    srv = Server(config=None, port="auto")
    srv._process = process

    with pytest.raises(RuntimeError, match="engine >= v0.9.3"):
        srv._await_bound_port(wait_timeout=1)


def test_start_dynamic_port_uses_desktop_announcement(monkeypatch, tmp_path: Path):
    config = tmp_path / "volca.toml"
    config.write_text("")
    process = mock.Mock()
    process.stdout = io.StringIO("VOLCA_PORT=43123\n")
    srv = Server(config=str(config), port="auto")
    monkeypatch.setattr(srv, "_find_binary", lambda: "/tmp/volca")
    monkeypatch.setattr(srv, "is_alive", lambda: True)
    check_wire = mock.Mock()
    monkeypatch.setattr(srv, "_check_wire", check_wire)

    with mock.patch("volca.server.subprocess.Popen", return_value=process) as popen:
        srv.start(wait_timeout=1)

    command = popen.call_args.args[0]
    assert "--desktop" in command
    assert command[command.index("--port") + 1] == "0"
    assert srv.port == 43123
    check_wire.assert_called_once_with()


def test_start_config_none_omits_the_flag(monkeypatch):
    process = mock.Mock()
    process.stdout = io.StringIO("VOLCA_PORT=43123\n")
    srv = Server(config=None, port="auto")
    monkeypatch.setattr(srv, "_find_binary", lambda: "/tmp/volca")
    monkeypatch.setattr(srv, "is_alive", lambda: True)
    monkeypatch.setattr(srv, "_check_wire", mock.Mock())

    with mock.patch("volca.server.subprocess.Popen", return_value=process) as popen:
        srv.start(wait_timeout=1)

    command = popen.call_args.args[0]
    assert "--config" not in command


def test_start_refuses_a_missing_config_path(monkeypatch):
    # A typo'd path must fail loudly, never silently become "all defaults".
    srv = Server(config="absent.toml")
    monkeypatch.setattr(srv, "_find_binary", lambda: "/tmp/volca")
    monkeypatch.setattr(srv, "is_alive", lambda: False)

    with mock.patch("volca.server.subprocess.Popen") as popen:
        with pytest.raises(FileNotFoundError, match="config=None"):
            srv.start(wait_timeout=1)
    popen.assert_not_called()


def test_start_reports_early_engine_exit_with_config_hint(monkeypatch):
    # Fixed-port path: the engine dies at once (e.g. an engine too old for
    # config-less startup); start() must fail now with the hint, not hang
    # until the readiness timeout.
    process = mock.Mock()
    process.poll.return_value = 1
    srv = Server(config=None, port=8199)
    monkeypatch.setattr(srv, "_find_binary", lambda: "/tmp/volca")
    monkeypatch.setattr(srv, "is_alive", lambda: False)

    with mock.patch("volca.server.subprocess.Popen", return_value=process):
        with pytest.raises(RuntimeError, match="engine >= v0.9.3"):
            srv.start(wait_timeout=5)


def test_stop_survives_an_engine_slow_to_answer_shutdown():
    """A shutdown request that times out still stops the process, without raising."""
    process = mock.Mock()
    process.wait.side_effect = [subprocess.TimeoutExpired("volca", 10), 0]
    srv = Server(config="absent.toml")
    srv._process = process
    with mock.patch("volca.server.requests.post", side_effect=requests.ReadTimeout):
        srv.stop()

    process.terminate.assert_called_once()
    process.kill.assert_called_once()
    assert srv._process is None


def test_stop_reports_an_engine_it_did_not_start_still_running():
    """A slow shutdown of an engine someone else started is not hidden: it keeps running."""
    srv = Server(config="absent.toml")
    with mock.patch("volca.server.requests.post", side_effect=requests.ReadTimeout), \
         pytest.raises(requests.ReadTimeout):
        srv.stop()


def test_declared_config_names_each_database_and_method_by_its_resolved_path(tmp_path: Path):
    """The TOML a ``databases=`` server writes reads back as what was declared."""
    from volca.server import declared_config, tomllib

    db = tmp_path / "my db.zip"
    text = declared_config({"agb": db}, {"EF 3.1": "ef31.zip"})
    assert tomllib.loads(text) == {
        "databases": [{"name": "agb", "path": str(db.resolve())}],
        "methods": [{"name": "EF 3.1", "path": str(Path("ef31.zip").resolve())}],
    }


def test_declared_databases_write_their_own_config_on_a_free_port(tmp_path: Path):
    srv = Server(databases={"agb": tmp_path / "agb.zip"})
    assert Path(srv.config).is_file()
    assert srv.port == 0  # "auto": the engine picks a free port


def test_declared_databases_refuse_a_config_beside_them():
    with pytest.raises(ValueError, match="not both"):
        Server(config="mine.toml", databases={"agb": "agb.zip"})


def test_volca_binary_names_a_local_build(tmp_path: Path, monkeypatch):
    monkeypatch.setenv("VOLCA_BINARY", "/build/volca")
    monkeypatch.setenv("VOLCA_DATA_DIR", str(tmp_path))
    assert Server(config=None)._find_binary() == "/build/volca"
    mine = tmp_path / "mine"
    mine.write_bytes(b"")
    assert Server(config=None, binary=str(mine))._find_binary() == str(mine)


def test_volca_binary_without_its_data_is_refused(monkeypatch):
    monkeypatch.setenv("VOLCA_BINARY", "/build/volca")
    monkeypatch.delenv("VOLCA_DATA_DIR", raising=False)
    with pytest.raises(RuntimeError, match="VOLCA_DATA_DIR"):
        Server(config=None)._find_binary()
