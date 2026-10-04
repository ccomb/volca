"""A failed load or unload raises, whatever engine answers it.

Engines now answer a failure with an HTTP error, which ``_json`` raises on.
Older ones answered 200 with the failure in the body; read as a result, a
load that never happened looked like one that did.
"""

from __future__ import annotations

from unittest import mock

import pytest

from volca.client import Client, VoLCAError


@pytest.fixture()
def client() -> Client:
    return Client(base_url="http://test.local")


def test_load_database_raises_on_an_in_band_failure(client):
    client._call = mock.Mock(return_value={"tag": "LoadFailed", "error": "Database not found: nope"})  # type: ignore[method-assign]
    with pytest.raises(VoLCAError, match="Database not found: nope"):
        client.load_database("nope")


def test_unload_database_raises_on_an_in_band_failure(client):
    client._call = mock.Mock(return_value={"success": False, "message": "Database not loaded: nope"})  # type: ignore[method-assign]
    with pytest.raises(VoLCAError, match="Database not loaded: nope"):
        client.unload_database("nope")


def test_derive_database_raises_on_an_in_band_failure(client):
    client._require_wire = mock.Mock()  # type: ignore[method-assign]
    client._call = mock.Mock(return_value={"tag": "LoadFailed", "error": "the key divides no block"})  # type: ignore[method-assign]
    with pytest.raises(VoLCAError, match="divides no block"):
        client.derive_database("x-wet", "wet mass", db_name="x")


def test_a_successful_load_is_returned(client):
    answer = {"tag": "LoadSucceeded", "database": {"name": "db"}, "deps": []}
    client._call = mock.Mock(return_value=answer)  # type: ignore[method-assign]
    assert client.load_database("db") == answer
