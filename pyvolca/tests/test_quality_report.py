"""Offline tests for the quality report helpers: the JSON one goes through the
dispatcher, the CSV ones fetch the file the web page offers."""

from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from volca import VoLCAError


def _file(status: int, content: bytes) -> MagicMock:
    resp = MagicMock()
    resp.status_code = status
    resp.content = content
    resp.text = content.decode()
    return resp


def test_quality_report_reads_the_database_asked(mocked_client, make_response):
    client, session = mocked_client
    session.get.return_value = make_response({"checks": []})
    assert client.quality_report("other", limit=5) == {"checks": []}
    assert session.get.call_args[0][0] == "http://test.local/api/v1/db/other/quality-report"
    assert dict(session.get.call_args[1]["params"]) == {"limit": "5"}


def test_quality_report_csv_fetches_the_file(mocked_client):
    client, session = mocked_client
    session.get.return_value = _file(200, b"check,activity\n")
    assert client.quality_report_csv() == b"check,activity\n"
    assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/quality-report.csv"
    assert session.get.call_args[1]["params"] == {}


def test_computed_quality_report_csv_names_its_collection(mocked_client):
    client, session = mocked_client
    session.get.return_value = _file(200, b"check\n")
    client.computed_quality_report_csv(collection="EF 3.1", limit=10)
    assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/computed-quality-report.csv"
    assert session.get.call_args[1]["params"] == {"collection": "EF 3.1", "limit": 10}


def test_quality_report_csv_raises_on_an_http_error(mocked_client):
    client, session = mocked_client
    session.get.return_value = _file(404, b"no such database")
    with pytest.raises(VoLCAError, match="HTTP 404"):
        client.quality_report_csv("missing")
