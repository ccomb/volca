"""Offline tests for method collection management (direct HTTP, no operationId)."""

from __future__ import annotations

import pytest

from volca.client import Client, VoLCAError


def _resp(session_attr, json_body: dict) -> None:
    from tests.conftest import _make_response

    session_attr.return_value = _make_response(json_body)


class TestMethodCollections:
    def test_list_unwraps_methods_envelope(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"methods": [{"name": "ef31", "status": "loaded", "methodCount": 16}]})
        out = client.list_method_collections()
        assert session.get.call_args[0][0] == "http://test.local/api/v1/method-collections"
        assert out == [{"name": "ef31", "status": "loaded", "methodCount": 16}]

    def test_load_posts_and_checks_success(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "loaded"})
        client.load_method_collection("ef31")
        assert session.post.call_args[0][0] == "http://test.local/api/v1/method-collections/ef31/load"

    def test_unload_posts(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "unloaded"})
        client.unload_method_collection("ef31")
        assert session.post.call_args[0][0] == "http://test.local/api/v1/method-collections/ef31/unload"

    def test_delete_uses_http_delete(self, mocked_client):
        client, session = mocked_client
        _resp(session.delete, {"success": True, "message": "gone"})
        client.delete_method_collection("ef31")
        assert session.delete.call_args[0][0] == "http://test.local/api/v1/method-collections/ef31"

    def test_load_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": False, "message": "no such collection"})
        with pytest.raises(VoLCAError, match="no such collection"):
            client.load_method_collection("nope")

    def test_upload_streams_to_collection_endpoint(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "ok", "slug": "ef31", "format": "ILCD"})
        client.upload_method_collection(b"method bytes", "EF 3.1", description="PEF")
        assert session.post.call_args[0][0] == "http://test.local/api/v1/method-collections/upload"
        assert session.post.call_args[1]["params"] == {"name": "EF 3.1", "description": "PEF"}
        assert session.post.call_args[1]["data"] == b"method bytes"

    def test_upload_names_the_database_its_flows_come_from(self, mocked_client):
        client, session = mocked_client
        client._server_wire = 101
        client._checked = True
        _resp(session.post, {"success": True, "message": "ok", "families": [], "dropped": {"count": 0, "examples": []}})
        client.upload_method_collection(b"workbook", "LCIA", flows_from="ei-3.12")
        assert session.post.call_args[1]["params"] == {"name": "LCIA", "flows_from": "ei-3.12"}

    def test_upload_flows_from_refused_by_an_older_engine(self, mocked_client):
        client, session = mocked_client
        client._server_wire = 98
        client._checked = True
        with pytest.raises(VoLCAError, match="flows_from"):
            client.upload_method_collection(b"workbook", "LCIA", flows_from="ei-3.12")
        session.post.assert_not_called()

    def test_available_lists_the_catalogue(self, mocked_client):
        client, session = mocked_client
        client._server_wire = 103
        client._checked = True
        _resp(session.get, [{"name": "IMPACT World+ 2.2.1 Expert", "installed": False}])
        out = client.available_method_collections()
        assert session.get.call_args[0][0] == "http://test.local/api/v1/method-collections/available"
        assert out == [{"name": "IMPACT World+ 2.2.1 Expert", "installed": False}]

    def test_install_quotes_the_name_and_checks_success(self, mocked_client):
        client, session = mocked_client
        client._server_wire = 103
        client._checked = True
        _resp(session.post, {"success": True, "message": "Installed and loaded method: IMPACT World+ 2.2.1 Expert"})
        client.install_method_collection("IMPACT World+ 2.2.1 Expert")
        assert session.post.call_args[0][0] == (
            "http://test.local/api/v1/method-collections/available/IMPACT%20World%2B%202.2.1%20Expert/install"
        )

    def test_install_refused_by_an_older_engine(self, mocked_client):
        client, session = mocked_client
        client._server_wire = 102
        client._checked = True
        with pytest.raises(VoLCAError, match="install_method_collection"):
            client.install_method_collection("IMPACT World+ 2.2.1 Expert")
        session.post.assert_not_called()
