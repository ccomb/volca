"""Engine wire-compatibility gate: volca._compat plus its client hooks.

All offline, with no engine binary. The gate's logic is exercised directly on
synthetic :class:`ServerVersion` values, and the client integration through a
mocked session.
"""

from __future__ import annotations

import re
import warnings
from pathlib import Path
from unittest.mock import MagicMock

import pytest

from volca import _compat
from volca.client import Client, VoLCAError
from volca.types import ServerVersion


def _sv(wire: int | None) -> ServerVersion:
    """A ServerVersion advertising wire ``wire`` (None = pre-wireVersion engine)."""
    return ServerVersion(
        version="9.9.9",
        git_hash="deadbee",
        git_tag=None,
        build_target="x86_64-linux",
        wire_version=wire,
    )


def _version_body(wire: int | None) -> dict:
    """The /api/v1/version JSON an engine at wire ``wire`` would return."""
    body = {"version": "9.9.9", "gitHash": "deadbee", "buildTarget": "x86_64-linux"}
    if wire is not None:
        body["wireVersion"] = wire
    return body


# -- check(): the pure policy -------------------------------------------------


@pytest.mark.parametrize("wire", [None, 0])
def test_check_rejects_too_old_wire(wire: int | None) -> None:
    with pytest.raises(VoLCAError) as exc:
        _compat.check(_sv(wire))
    # names the engine to upgrade to, and an older-pyvolca fallback
    assert f"v{_compat.MIN_ENGINE_HINT}" in str(exc.value)
    assert "pyvolca<0.7.2" in str(exc.value)


def test_check_message_distinguishes_absent_from_zero() -> None:
    """wire 0 is a real value, not 'absent': the messages must differ."""
    with pytest.raises(VoLCAError) as none_exc:
        _compat.check(_sv(None))
    with pytest.raises(VoLCAError) as zero_exc:
        _compat.check(_sv(0))
    assert "pre-1" in str(none_exc.value)
    assert "wire 0" in str(zero_exc.value)


@pytest.mark.parametrize("wire", [_compat.REQUIRED_WIRE, _compat.KNOWN_WIRE])
def test_check_is_silent_on_known_wires(wire: int) -> None:
    with warnings.catch_warnings():
        warnings.simplefilter("error")  # any warning would fail the test
        _compat.check(_sv(wire))  # neither raises nor warns


def test_check_warns_on_newer_wire() -> None:
    with pytest.warns(UserWarning, match="upgrade pyvolca"):
        _compat.check(_sv(_compat.KNOWN_WIRE + 1))


@pytest.mark.parametrize("wire", [None, 0])
def test_skip_env_downgrades_error_to_warning(
    wire: int | None, monkeypatch: pytest.MonkeyPatch
) -> None:
    monkeypatch.setenv("VOLCA_SKIP_COMPAT_CHECK", "1")
    with pytest.warns(UserWarning, match="VOLCA_SKIP_COMPAT_CHECK"):
        _compat.check(_sv(wire))  # opt-out: warns instead of raising


# -- client integration -------------------------------------------------------


def _client_with_version(make_response, wire: int | None) -> tuple[Client, MagicMock]:
    c = Client(base_url="http://test.local")
    session = MagicMock()
    session.get.return_value = make_response(_version_body(wire))
    c._session = session
    return c, session


def test_get_version_stays_ungated(make_response) -> None:
    """A bad engine must remain inspectable: get_version never gates."""
    c, _ = _client_with_version(make_response, wire=None)
    sv = c.get_version()  # must not raise despite the missing wireVersion
    assert sv.wire_version is None


def test_load_operations_gate_rejects_old_engine(make_response) -> None:
    c, _ = _client_with_version(make_response, wire=None)
    with pytest.raises(VoLCAError):
        c._load_operations()


def test_refresh_stubs_is_gated(make_response) -> None:
    """The explicit engine-upgrade path honours the wire gate, and refuses
    before fetching the spec it would otherwise fail to decode."""
    c, session = _client_with_version(make_response, wire=None)
    with pytest.raises(VoLCAError):
        c.refresh_stubs()
    assert session.get.call_count == 1  # version checked; openapi.json never fetched


def test_refresh_stubs_rechecks_the_wire(make_response, monkeypatch) -> None:
    """The documented "engine was upgraded" path must re-read the live wire:
    a client that first met an older engine would otherwise keep refusing
    wire-gated capabilities from a stale cache after an in-place upgrade."""
    monkeypatch.setattr("volca._stub_gen.write_stubs_for_spec", lambda spec: None)
    c, session = _client_with_version(make_response, wire=_compat.REQUIRED_WIRE)
    c._ensure_compatible()
    assert c._server_wire == _compat.REQUIRED_WIRE
    session.get.return_value = make_response(_version_body(_compat.KNOWN_WIRE))
    c.refresh_stubs()
    assert c._server_wire == _compat.KNOWN_WIRE


def test_ensure_compatible_is_one_shot(make_response) -> None:
    c, session = _client_with_version(make_response, wire=_compat.REQUIRED_WIRE)
    c._ensure_compatible()
    c._ensure_compatible()
    assert c._checked is True
    assert session.get.call_count == 1  # version fetched once, then cached


def test_preloaded_operations_skip_the_gate(mocked_client) -> None:
    """Clients handed a preloaded operation table (the offline fixtures) must
    never trigger a version fetch; that is what keeps the dispatch tests
    engine-free."""
    client, session = mocked_client
    client._load_operations()
    assert session.get.call_count == 0
    assert client._checked is False


def test_search_flows_kind_refuses_an_engine_that_would_drop_it(make_response) -> None:
    """An engine before wire 9 ignores the unknown query key and answers with
    every kind, which reads as "no flow of that kind exists". Refuse instead."""
    c, session = _client_with_version(make_response, wire=8)
    with pytest.raises(VoLCAError) as exc:
        c.search_flows("water", kind="biosphere")
    assert "wire revision >= 9" in str(exc.value)
    assert session.get.call_count == 1  # version checked; the search never went out


def test_known_wire_follows_the_engine() -> None:
    """The engine's own wire revision and the one this client claims to know.

    They drifted four revisions apart because nothing tied them together: an
    engine change bumps `currentWireVersion`, the client keeps its own number,
    and every call against a current engine starts warning that the engine is
    newer than the client. Read from the source tree when it is there, which is
    where a pull request runs; skipped when pyvolca is installed on its own.
    """
    routes = Path(__file__).resolve().parents[2] / "src" / "API" / "Routes.hs"
    if not routes.exists():
        pytest.skip("engine source not alongside; nothing to compare against")
    m = re.search(r"^currentWireVersion\s*=\s*(\d+)", routes.read_text(), re.M)
    assert m, f"no currentWireVersion in {routes}"
    engine_wire = int(m.group(1))
    assert _compat.KNOWN_WIRE == engine_wire, (
        f"the engine speaks wire {engine_wire} and this pyvolca knows up to "
        f"{_compat.KNOWN_WIRE}: read what that revision added, decode it, and "
        "move KNOWN_WIRE with its docstring."
    )
