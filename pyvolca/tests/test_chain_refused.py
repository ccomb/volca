"""A chain reaching inputs only a database not loaded would supply is refused.

A single calculation raises ``ChainRefused`` with the engine's sentence; a
batch lists such processes in ``refused`` and scores the others.
"""

from __future__ import annotations

from unittest import mock

import pytest
import requests

from volca import ChainRefused, RefusedActivity, VoLCAError
from volca.client import Client
from volca.types import BatchScores

SENTENCE = "Refused: the chain of bread reaches 2 inputs that a database would supply once loaded and linked (flour, salt); its upstream names them."


def response(status: int, text: str) -> requests.Response:
    r = requests.Response()
    r.status_code = status
    r._content = text.encode()
    r.reason = "Unprocessable Entity" if status == 422 else "OK"
    r.request = requests.Request("GET", "http://test.local/x").prepare()
    return r


def test_a_refused_chain_raises_chain_refused_with_the_whole_sentence():
    with pytest.raises(ChainRefused) as caught:
        Client._json(response(422, SENTENCE))
    assert str(caught.value) == SENTENCE
    assert caught.value.status_code == 422


def test_another_unprocessable_request_stays_a_plain_error():
    with pytest.raises(VoLCAError) as caught:
        Client._json(response(422, "Cannot convert 1 m3 to kg"))
    assert not isinstance(caught.value, ChainRefused)


def test_a_batch_lists_its_refused_processes_beside_its_scores():
    scores = BatchScores.from_json(
        {
            "results": [],
            "notFound": [],
            "invalid": [],
            "unscorable": [],
            "unsolvable": [],
            "refused": [{"processId": "bread", "count": 2, "reason": SENTENCE}],
        }
    )
    assert scores.refused == [RefusedActivity(process_id="bread", count=2, reason=SENTENCE)]


def test_score_table_names_a_refused_process_among_those_not_scored():
    client = Client(base_url="http://test.local")
    refused = BatchScores.from_json(
        {"results": [], "notFound": [], "invalid": [], "unscorable": [], "unsolvable": [], "refused": [{"processId": "bread", "count": 2, "reason": SENTENCE}]}
    )
    client._resolve_collection = mock.Mock(return_value="ef")  # type: ignore[method-assign]
    client.score_activities = mock.Mock(return_value=refused)  # type: ignore[method-assign]
    with pytest.raises(VoLCAError, match="bread"):
        client.score_table(["bread"], collection="ef")
