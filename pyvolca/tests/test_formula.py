"""The formula language and the formula check, as pyvolca asks for them."""

import pytest

from volca import ParameterValue, Scenario, SetParameter, Uncertainty, VoLCAError


def test_formula_language_dispatch(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 72
    session.get.return_value = make_response(
        {
            "functions": [{"name": "sqrt", "form": "function", "usage": "sqrt(x)", "meaning": "the square root of x", "example": "sqrt(16)", "value": 4}],
            "operators": [["a + b", "a - b"], ["-x"]],
            "rules": ["Names are read without case."],
        }
    )
    language = client.formula_language()
    assert session.get.call_args[0][0] == "http://test.local/api/v1/formula-language"
    assert language.functions[0].name == "sqrt" and language.functions[0].value == 4
    assert language.operators == [["a + b", "a - b"], ["-x"]]


def test_check_formula_dispatch(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 72
    session.post.return_value = make_response({"value": 2.5, "reads": ["yield"], "warnings": [], "problem": None})
    check = client.check_formula("2.5 * yield", process_id="bread", exchange=1)
    assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/formula-check"
    assert session.post.call_args[1]["json"] == {"formula": "2.5 * yield", "process": "bread", "exchange": 1}
    assert check.value == 2.5 and check.reads == ["yield"] and check.problem is None


def test_check_formula_gives_the_problem_as_an_answer(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 72
    session.post.return_value = make_response({"value": None, "reads": [], "warnings": [], "problem": "no parameter or named line 'nope'"})
    check = client.check_formula("nope")
    assert session.post.call_args[1]["json"] == {"formula": "nope"}
    assert check.value is None and check.problem == "no parameter or named line 'nope'"


def test_formula_routes_need_revision_72(mocked_client):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 71
    with pytest.raises(VoLCAError, match="wire revision >= 72"):
        client.check_formula("1")
    with pytest.raises(VoLCAError, match="wire revision >= 72"):
        client.formula_language()
    session.post.assert_not_called()
    session.get.assert_not_called()


def test_database_parameters_dispatch(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 74
    session.get.return_value = make_response(
        [{"name": "distance", "value": 120, "formula": None, "unit": "km", "description": "Truck route", "uncertainty": None}]
    )
    parameters = client.database_parameters()
    assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/parameters"
    assert parameters[0].name == "distance" and parameters[0].value == 120


def test_edit_database_parameters_dispatch(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 74
    session.post.return_value = make_response(
        {"parametersSet": ["distance"], "parametersRemoved": ["old"], "restatedProcesses": ["a_b"], "transient": False, "warnings": []}
    )
    answer = client.edit_database_parameters(
        set_parameters=[SetParameter("distance", value=150, uncertainty=Uncertainty("uniform", minimum=100, maximum=200))],
        remove_parameters=["old"],
    )
    assert session.post.call_args[0][0] == "http://test.local/api/v1/db/testdb/parameters"
    assert session.post.call_args[1]["json"] == {
        "setParameters": [{"name": "distance", "value": 150, "uncertainty": {"distribution": "uniform", "minimum": 100, "maximum": 200}}],
        "removeParameters": ["old"],
    }
    assert answer["restatedProcesses"] == ["a_b"]


def test_database_parameters_need_revision_74(mocked_client):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 73
    with pytest.raises(VoLCAError, match="wire revision >= 74"):
        client.database_parameters()
    with pytest.raises(VoLCAError, match="wire revision >= 74"):
        client.edit_database_parameters(remove_parameters=["old"])
    session.post.assert_not_called()
    session.get.assert_not_called()


def test_scenarios_dispatch(mocked_client, make_response):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 75
    saved = [{"name": "high salt", "parameters": [{"process": None, "name": "salt_kg", "value": 0.02}], "problem": None}]
    session.get.return_value = make_response(saved)
    assert client.scenarios() == [Scenario("high salt", [ParameterValue("salt_kg", 0.02)])]
    assert session.get.call_args[0][0] == "http://test.local/api/v1/db/testdb/scenarios"

    session.put.return_value = make_response(saved)
    client.save_scenario("high salt", [ParameterValue("salt_kg", 0.02)])
    assert session.put.call_args[0][0] == "http://test.local/api/v1/db/testdb/scenarios/high salt"
    assert session.put.call_args[1]["json"] == {"parameters": [{"name": "salt_kg", "value": 0.02}]}

    session.delete.return_value = make_response([])
    assert client.drop_scenario("high salt") == []
    assert session.delete.call_args[0][0] == "http://test.local/api/v1/db/testdb/scenarios/high salt"


def test_scenarios_need_revision_75(mocked_client):
    client, session = mocked_client
    client._checked = True
    client._server_wire = 74
    with pytest.raises(VoLCAError, match="wire revision >= 75"):
        client.scenarios()
    with pytest.raises(VoLCAError, match="wire revision >= 75"):
        client.save_scenario("x", [ParameterValue("salt_kg", 1)])
    session.get.assert_not_called()
    session.put.assert_not_called()
