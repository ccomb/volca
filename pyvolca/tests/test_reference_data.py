"""Offline tests for reference-data management and flow-synonym extras."""

from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from volca import UnitDefinition
from volca.client import Client, VoLCAError


def _resp(session_attr, json_body: dict) -> None:
    from tests.conftest import _make_response

    session_attr.return_value = _make_response(json_body)


class TestKindValidation:
    def test_unknown_kind_raises_before_any_request(self, mocked_client):
        client, session = mocked_client
        with pytest.raises(VoLCAError, match="unknown reference data kind"):
            client.list_reference_data("colours")
        session.get.assert_not_called()

    def test_upload_unknown_kind_raises_before_request(self, mocked_client):
        client, session = mocked_client
        with pytest.raises(VoLCAError, match="unknown reference data kind"):
            client.upload_reference_data("colours", b"x", "y")
        session.post.assert_not_called()


class TestReferenceData:
    def test_list_unwraps_items_envelope(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"items": [{"name": "si", "status": "loaded", "isAuto": True, "entryCount": 42}]})
        out = client.list_reference_data("units")
        assert session.get.call_args[0][0] == "http://test.local/api/v1/units"
        assert out[0]["entryCount"] == 42

    def test_load_targets_kind_path(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "loaded"})
        client.load_reference_data("compartment-mappings", "ei")
        assert session.post.call_args[0][0] == "http://test.local/api/v1/compartment-mappings/ei/load"

    def test_delete_targets_kind_path(self, mocked_client):
        client, session = mocked_client
        _resp(session.delete, {"success": True, "message": "gone"})
        client.delete_reference_data("units", "custom")
        assert session.delete.call_args[0][0] == "http://test.local/api/v1/units/custom"

    def test_upload_streams_to_kind_endpoint(self, mocked_client):
        client, session = mocked_client
        _resp(session.post, {"success": True, "message": "ok", "slug": "syn", "format": None})
        client.upload_reference_data("flow-synonyms", b"csv bytes", "My synonyms")
        assert session.post.call_args[0][0] == "http://test.local/api/v1/flow-synonyms/upload"
        assert session.post.call_args[1]["params"] == {"name": "My synonyms"}


class TestSynonymExtras:
    def test_get_synonym_groups_unwraps(self, mocked_client):
        client, session = mocked_client
        _resp(session.get, {"groups": [["water", "H2O"], ["CO2", "carbon dioxide"]]})
        out = client.get_synonym_groups("pubchem")
        assert session.get.call_args[0][0] == "http://test.local/api/v1/flow-synonyms/pubchem/groups"
        assert out == [["water", "H2O"], ["CO2", "carbon dioxide"]]

    def test_download_returns_raw_bytes(self, mocked_client):
        client, session = mocked_client
        resp = MagicMock()
        resp.status_code = 200
        resp.content = b"source,target\nwater,H2O\n"
        session.get.return_value = resp
        out = client.download_flow_synonyms("pubchem")
        assert out == b"source,target\nwater,H2O\n"
        assert session.get.call_args[0][0] == "http://test.local/api/v1/flow-synonyms/pubchem/download"
        assert session.get.call_args[1]["headers"] == {"Accept": "application/octet-stream"}

    def test_unit_table_reads_each_row(self, mocked_client):
        client, session = mocked_client
        client._checked = True
        client._server_wire = 81
        resp = MagicMock()
        resp.status_code = 200
        resp.content = b"unit,measure,ratio\nkg,mass,1.0\n g ,mass,0.001\nm3,volume,1.0\n"
        session.get.return_value = resp
        table = client.unit_table()
        assert session.get.call_args[0][0] == "http://test.local/api/v1/units/Default%20units/download"
        assert session.get.call_args[1]["headers"] == {"Accept": "application/octet-stream"}
        assert table["g"] == UnitDefinition("mass", 0.001)
        assert table["m3"].dimension == "volume"
        assert len(table) == 3

    def test_unit_table_asks_for_an_engine_that_serves_it(self, mocked_client):
        client, session = mocked_client
        client._checked = True
        client._server_wire = 79
        with pytest.raises(VoLCAError, match="0.16.0"):
            client.unit_table()
        session.get.assert_not_called()

    def test_download_http_error_raises(self, mocked_client):
        client, session = mocked_client
        resp = MagicMock()
        resp.status_code = 404
        resp.text = "not found"
        session.get.return_value = resp
        with pytest.raises(VoLCAError, match="HTTP 404"):
            client.download_flow_synonyms("missing")
