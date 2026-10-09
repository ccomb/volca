"""Offline request-shaping tests for the database write operations.

These endpoints (copy / delete / relink / export / add-/remove-dependency)
carry no operationId: they bypass the OpenAPI dispatcher and build their
URLs directly. They also do not exist in any released engine binary, so these
tests never touch a live engine: they mock ``Client._session`` and assert on
the wire shape (URL, JSON body, raw-bytes handling, format validation, error
surfacing).
"""

from __future__ import annotations

import urllib.parse

import pytest

from volca.client import Client, VoLCAError
from volca.types import (
    ActivityInput,
    BioDirection,
    BioExchange,
    ExchangeSelector,
    SetAmount,
    TechInput,
)


def _ok(session, json_body: dict) -> None:
    """Wire the mocked session's POST to return a 200 with ``json_body``."""
    from tests.conftest import _make_response

    session.post.return_value = _make_response(json_body)


# ---------------------------------------------------------------------------
# delete
# ---------------------------------------------------------------------------


class TestDelete:
    def test_body_shape_with_dict_classifications(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 3})
        result = client.delete_activities(
            name="wheat",
            location="FR",
            classifications=[{"system": "ISIC", "value": "01", "exact": True}],
            exact=True,
            keep=["a_b"],
            extra=["c_d"],
        )
        assert result["deleted"] == 3
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/delete"
        body = session.post.call_args[1]["json"]
        # Blank filters are omitted entirely: product was not supplied, so its
        # key is absent rather than sent as "".
        assert body == {
            "name": "wheat",
            "location": "FR",
            "classifications": [{"system": "ISIC", "value": "01", "exact": True}],
            "exact": True,
            "keep": ["a_b"],
            "extra": ["c_d"],
        }

    def test_tuple_classifications_coerced(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 0})
        client.delete_activities(classifications=[("CPC", "012"), ("ISIC", "01", True)])
        body = session.post.call_args[1]["json"]
        assert body["classifications"] == [
            {"system": "CPC", "value": "012", "exact": False},
            {"system": "ISIC", "value": "01", "exact": True},
        ]

    def test_defaults_are_empty(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 0})
        client.delete_activities(product="x")
        body = session.post.call_args[1]["json"]
        assert body["classifications"] == []
        assert body["keep"] == []
        assert body["extra"] == []
        assert body["exact"] is False
        # An unsupplied name is omitted, never sent as "": a regression to the
        # always-send-empty-string body would make the engine read "name":""
        # as a real (unsatisfiable) filter and silently delete nothing.
        assert "name" not in body

    def test_blank_filters_are_omitted(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok", "deleted": 0})
        client.delete_activities(product="milk")
        body = session.post.call_args[1]["json"]
        assert "name" not in body
        assert "location" not in body
        assert body["product"] == "milk"

    def test_malformed_classification_dict_raises(self, mocked_client):
        client, _ = mocked_client
        with pytest.raises(VoLCAError, match="missing keys"):
            client.delete_activities(classifications=[{"system": "ISIC"}])

    def test_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": False, "message": "nothing matched"})
        with pytest.raises(VoLCAError, match="nothing matched"):
            client.delete_activities(name="x")


def _version_ok(session, wire: int) -> None:
    """Wire the mocked session's GET (used by get_version) to a wire response."""
    from tests.conftest import _make_response

    session.get.return_value = _make_response(
        {
            "version": "0.9.3",
            "gitHash": "abc1234",
            "gitTag": "v0.9.3",
            "buildTarget": "x86_64-linux",
            "wireVersion": wire,
        }
    )


class TestDeleteByIds:
    def test_ids_body_shape_behind_the_wire_gate(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=3)
        _ok(session, {"success": True, "message": "ok", "deleted": 2})
        result = client.delete_activities(ids=["a_b", "c_d"], keep=["a_b"])
        assert result["deleted"] == 2
        body = session.post.call_args[1]["json"]
        assert body["ids"] == ["a_b", "c_d"]
        assert body["keep"] == ["a_b"]
        # ids names the selection verbatim: no filter keys ride along.
        assert "name" not in body and "product" not in body

    def test_ids_refused_with_filter_arguments(self, mocked_client):
        client, session = mocked_client
        with pytest.raises(VoLCAError, match="cannot be combined"):
            client.delete_activities(ids=["a_b"], name="wheat")
        with pytest.raises(VoLCAError, match="cannot be combined"):
            client.delete_activities(ids=["a_b"], exact=True)
        session.post.assert_not_called()

    def test_ids_never_sent_to_a_wire2_engine(self, mocked_client):
        # A wire-2 engine would drop the unknown "ids" key and read the request
        # as an empty filter ("everything"); the client must refuse to send it.
        client, session = mocked_client
        _version_ok(session, wire=2)
        with pytest.raises(VoLCAError, match="wire revision >= 3"):
            client.delete_activities(ids=["a_b"])
        session.post.assert_not_called()


# ---------------------------------------------------------------------------
# copy
# ---------------------------------------------------------------------------


class TestCopy:
    def test_url_and_default_db(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "copied", "database": None})
        client.copy_database("clone")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/copy/clone"

    def test_explicit_db_override(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": True, "message": "ok"})
        client.copy_database("clone", db_name="other")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/other/copy/clone"

    def test_in_band_failure_raises(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"success": False, "message": "already exists"})
        with pytest.raises(VoLCAError, match="already exists"):
            client.copy_database("clone")


# ---------------------------------------------------------------------------
# relink
# ---------------------------------------------------------------------------


class TestRelink:
    def test_body_and_url(self, mocked_client):
        client, session = mocked_client
        _ok(
            session,
            {
                "dbName": "testdb",
                "unresolvedBefore": 10,
                "unresolvedAfter": 2,
                "crossDBLinks": 8,
                "dependsOn": ["ecoinvent-3.9"],
            },
        )
        result = client.relink("ecoinvent-3.9", "src,dst\nfoo,bar\n")
        assert result["unresolvedAfter"] == 2
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/relink"
        body = session.post.call_args[1]["json"]
        assert body == {"depDb": "ecoinvent-3.9", "mappingCsv": "src,dst\nfoo,bar\n"}

    def test_from_file_reads_text(self, mocked_client, tmp_path):
        client, session = mocked_client
        _ok(session, {"dbName": "testdb", "unresolvedBefore": 0,
                      "unresolvedAfter": 0, "crossDBLinks": 0, "dependsOn": []})
        csv = tmp_path / "map.csv"
        csv.write_text("src,dst\nfoo,bar\n", encoding="utf-8")
        client.relink_from_file("dep", str(csv))
        body = session.post.call_args[1]["json"]
        assert body == {"depDb": "dep", "mappingCsv": "src,dst\nfoo,bar\n"}


# ---------------------------------------------------------------------------
# dependencies
# ---------------------------------------------------------------------------


class TestDependencies:
    def test_add_dependency_url(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"dependsOn": ["dep"]})
        client.add_dependency("dep")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/add-dependency/dep"

    def test_remove_dependency_url(self, mocked_client):
        client, session = mocked_client
        _ok(session, {"dependsOn": []})
        client.remove_dependency("dep", db_name="other")
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/other/remove-dependency/dep"


# ---------------------------------------------------------------------------
# export
# ---------------------------------------------------------------------------


def _raw(session, body: bytes, warnings_header: str | None = None) -> None:
    """Wire the mocked session's POST to return raw export bytes."""
    from tests.conftest import _make_response

    r = _make_response({})
    r.content = body
    if warnings_header is not None:
        r.headers = {"X-Volca-Export-Warnings": warnings_header}
    session.post.return_value = r


class TestExport:
    def test_returns_raw_bytes(self, mocked_client):
        client, session = mocked_client
        raw = b"PK\x03\x04 zipped db bytes"
        _raw(session, raw)
        out = client.export_database("ecospold2")
        assert out == raw
        url = session.post.call_args[0][0]
        assert url == "http://test.local/api/v1/db/testdb/export"
        assert session.post.call_args[1]["json"] == {"format": "ecospold2"}
        assert session.post.call_args[1]["headers"] == {
            "Accept": "application/octet-stream"
        }

    def test_format_normalized_before_send(self, mocked_client):
        client, session = mocked_client
        _raw(session, b"x")
        client.export_database("  SimaPro  ")
        assert session.post.call_args[1]["json"] == {"format": "simapro"}

    def test_unknown_format_raises_before_request(self, mocked_client):
        client, session = mocked_client
        with pytest.raises(VoLCAError, match="unknown export format"):
            client.export_database("parquet")
        session.post.assert_not_called()

    def test_http_error_raises(self, mocked_client):
        client, session = mocked_client
        from tests.conftest import _make_response

        session.post.return_value = _make_response(
            {"error": "Database not loaded: testdb"}, status=404
        )
        with pytest.raises(VoLCAError, match="not loaded"):
            client.export_database("simapro")

    def test_warnings_header_surfaced(self, mocked_client):
        client, session = mocked_client
        header = urllib.parse.quote("orphan waste in Café crème\nsecond warning")
        _raw(session, b"x", warnings_header=header)
        with pytest.warns(UserWarning) as caught:
            client.export_database("brightway")
        assert [str(w.message) for w in caught] == [
            "orphan waste in Café crème",
            "second warning",
        ]

    def test_to_file_writes_bytes(self, mocked_client, tmp_path):
        client, session = mocked_client
        raw = b"hello bytes"
        _raw(session, raw)
        out = tmp_path / "export.csv"
        client.export_to_file("simapro", str(out))
        assert out.read_bytes() == raw


# ---------------------------------------------------------------------------
# default-db guard
# ---------------------------------------------------------------------------


class TestNoDefaultDb:
    def _client(self):
        return Client(base_url="http://test.local", db="")

    def test_copy_without_db_raises(self):
        with pytest.raises(VoLCAError, match="No database specified"):
            self._client().copy_database("clone")

    def test_export_without_db_raises(self):
        with pytest.raises(VoLCAError, match="No database specified"):
            self._client().export_database("simapro")


# ---------------------------------------------------------------------------
# authoring activities
# ---------------------------------------------------------------------------


def _cheese(**overrides) -> ActivityInput:
    fields = {
        "name": "cheese, at dairy",
        "location": "FR",
        "product_name": "cheese",
        "product_amount": 1.0,
        "product_unit": "kg",
        "inputs": [TechInput(provider="act_prod", amount=8.0)],
    }
    fields.update(overrides)
    return ActivityInput(**fields)


class TestCreateActivities:
    def test_body_and_url_shape(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=5)
        _ok(session, {"written": ["a_b"], "transient": False, "warnings": []})
        result = client.create_activities([_cheese()])
        assert result["written"] == ["a_b"]
        url = session.post.call_args[0][0]
        assert url.endswith("/api/v1/db/testdb/activities")
        body = session.post.call_args[1]["json"]
        [activity] = body["activities"]
        # The wire speaks camelCase; the client speaks snake_case.
        assert activity["productName"] == "cheese"
        assert activity["productUnit"] == "kg"
        assert activity["inputs"] == [{"provider": "act_prod", "amount": 8.0}]
        # Identity is the engine's to mint, so nothing here names one.
        assert "processId" not in activity and "id" not in activity

    def test_accepts_a_single_activity_as_well_as_a_batch(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=5)
        _ok(session, {"written": ["a_b"], "transient": False, "warnings": []})
        client.create_activities(_cheese())
        assert len(session.post.call_args[1]["json"]["activities"]) == 1

    def test_omits_optional_fields_rather_than_sending_nulls(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=5)
        _ok(session, {"written": ["a_b"], "transient": False, "warnings": []})
        client.create_activities([_cheese()])
        [activity] = session.post.call_args[1]["json"]["activities"]
        assert "unit" not in activity["inputs"][0]
        assert "comment" not in activity["inputs"][0]

    def test_biosphere_line_carries_its_compartment(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=5)
        _ok(session, {"written": ["a_b"], "transient": False, "warnings": ["new flow"]})
        activity = _cheese(
            biosphere=[
                BioExchange.from_name("Nitrous oxide", "air", "Emission", 0.5, "kg"),
                BioExchange.from_id("11111111-2222-3333-4444-555555555555", "Emission", 1.2),
            ]
        )
        client.create_activities([activity])
        [sent] = session.post.call_args[1]["json"]["activities"]
        introduced, existing = sent["biosphere"]
        assert introduced["name"] == "Nitrous oxide"
        assert introduced["compartment"] == "air"
        assert "flow" not in introduced
        assert existing["flow"] == "11111111-2222-3333-4444-555555555555"
        assert "name" not in existing

    def test_never_sent_to_an_engine_that_has_no_such_route(self, mocked_client):
        # An absent route answers 404, which reads exactly like a misspelled
        # database name; refuse before sending rather than let the caller guess.
        client, session = mocked_client
        _version_ok(session, wire=4)
        with pytest.raises(VoLCAError, match="wire revision >= 5"):
            client.create_activities([_cheese()])
        session.post.assert_not_called()


class TestReplaceActivity:
    def test_puts_to_the_addressed_process(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=5)
        from tests.conftest import _make_response

        session.put.return_value = _make_response(
            {"written": ["a_b"], "transient": False, "warnings": []}
        )
        client.replace_activity("a_b", _cheese(product_amount=2.0))
        url = session.put.call_args[0][0]
        assert url.endswith("/api/v1/db/testdb/activity/a_b")
        assert session.put.call_args[1]["json"]["productAmount"] == 2.0

    def test_never_sent_to_an_engine_that_has_no_such_route(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=4)
        with pytest.raises(VoLCAError, match="wire revision >= 5"):
            client.replace_activity("a_b", _cheese())
        session.put.assert_not_called()


class TestAuthoringInputTypes:
    def test_a_biosphere_line_must_name_its_flow_exactly_one_way(self):
        # Caught here rather than after a round trip: the engine refuses the
        # same two shapes, but the caller finds out sooner.
        with pytest.raises(ValueError, match="not both and not neither"):
            BioExchange(direction=BioDirection.EMISSION, amount=1.0)
        with pytest.raises(ValueError, match="not both and not neither"):
            BioExchange(
                direction=BioDirection.EMISSION, amount=1.0, flow="f", name="n", compartment="air"
            )

    def test_a_new_flow_needs_a_compartment(self):
        with pytest.raises(ValueError, match="needs a compartment"):
            BioExchange(direction=BioDirection.EMISSION, amount=1.0, name="Nitrous oxide")

    def test_a_new_flow_needs_a_unit(self):
        # The engine refuses this too: a named flow's unit is half its
        # identity, so it cannot be defaulted.
        with pytest.raises(ValueError, match="needs a unit"):
            BioExchange(
                direction=BioDirection.EMISSION, amount=1.0, name="Nitrous oxide", compartment="air"
            )

    def test_direction_is_read_the_way_the_engine_reads_it(self):
        # The engine lowercases the wire value before matching, so the
        # client accepts any casing but always sends the canonical one.
        exchange = BioExchange.from_name("Nitrous oxide", "air", "emission", 0.5, "kg")
        assert exchange.direction is BioDirection.EMISSION
        assert exchange.to_wire()["direction"] == "Emission"

    @pytest.mark.parametrize(
        "retired,current,args",
        [
            ("existing", "from_id", ("11111111-2222-3333-4444-555555555555", "Emission", 1.2)),
            ("introducing", "from_name", ("Nitrous oxide", "air", "Emission", 0.5, "kg")),
        ],
    )
    def test_a_retired_constructor_still_builds_and_says_what_replaced_it(
        self, retired, current, args
    ):
        with pytest.warns(DeprecationWarning, match=f"BioExchange.{current}"):
            built = getattr(BioExchange, retired)(*args)
        assert built == getattr(BioExchange, current)(*args)


class TestEditExchanges:
    def test_body_and_url_shape(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=7)
        _ok(
            session,
            {"removed": [2], "amountsSet": [], "added": 1, "transient": False, "warnings": []},
        )
        result = client.edit_exchanges(
            "a_b",
            remove=[ExchangeSelector.biosphere_flow("f-1")],
            add_inputs=[TechInput(provider="c_d", amount=2.5)],
        )
        assert result["removed"] == [2]
        url = session.post.call_args[0][0]
        assert url.endswith("/api/v1/db/testdb/activity/a_b/exchanges")
        body = session.post.call_args[1]["json"]
        assert body["remove"] == [{"kind": "biosphere", "flow": "f-1"}]
        assert body["addInputs"] == [{"provider": "c_d", "amount": 2.5}]
        # All five lists travel even when empty. The engine reads an absent
        # list as empty; sending them all keeps the body one canonical shape.
        assert body["setAmounts"] == []
        assert body["addBiosphere"] == []
        assert body["addWasteOutputs"] == []

    def test_an_amount_change_nests_its_selector(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=7)
        _ok(
            session,
            {"removed": [], "amountsSet": [1], "added": 0, "transient": True, "warnings": []},
        )
        client.edit_exchanges(
            "a_b",
            set_amounts=[SetAmount(ExchangeSelector.input_from("c_d"), 4.0)],
        )
        body = session.post.call_args[1]["json"]
        assert body["setAmounts"] == [
            {"select": {"kind": "input", "provider": "c_d"}, "amount": 4.0}
        ]

    def test_texts_travel_only_when_stated(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=56)
        _ok(session, {"removed": [], "amountsSet": [], "added": 0, "transient": False, "warnings": []})
        client.edit_exchanges("a_b", name="milk, corrected", description=[])
        body = session.post.call_args[1]["json"]
        assert body["setName"] == "milk, corrected"
        # An empty description clears it, so it is sent, not dropped.
        assert body["setDescription"] == []
        assert "setLocation" not in body

    def test_texts_never_sent_to_an_engine_that_cannot_restate_them(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=55)
        with pytest.raises(VoLCAError, match="wire revision >= 56"):
            client.edit_exchanges("a_b", location="CH")
        session.post.assert_not_called()

    def test_never_sent_to_an_engine_that_has_no_such_route(self, mocked_client):
        client, session = mocked_client
        _version_ok(session, wire=6)
        with pytest.raises(VoLCAError, match="wire revision >= 7"):
            client.edit_exchanges("a_b", remove=[ExchangeSelector.biosphere_flow("f-1")])
        session.post.assert_not_called()

    def test_a_selector_names_the_key_its_kind_calls_for(self):
        # Caught before the round trip: the engine refuses the same shapes.
        with pytest.raises(ValueError, match="names its provider"):
            ExchangeSelector(kind="input", flow="f-1")
        with pytest.raises(ValueError, match="names its flow"):
            ExchangeSelector(kind="biosphere", provider="c_d")
        with pytest.raises(ValueError, match="unknown selector kind"):
            ExchangeSelector(kind="product", provider="c_d")
