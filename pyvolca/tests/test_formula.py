"""The formula language and the formula check, as pyvolca asks for them."""

import pytest

from volca import VoLCAError


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
