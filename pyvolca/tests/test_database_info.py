from volca.types import DatabaseInfo


def _entry(**extra):
    return {"name": "db", "displayName": "Db", "status": "loaded", "path": "/data/db", **extra}


def test_reads_the_terms_a_database_is_served_under():
    terms = {"licence": "Members only", "downloads": "refused"}
    assert DatabaseInfo.from_json(_entry(terms=terms)).terms == terms


def test_an_engine_without_terms_leaves_them_unknown():
    assert DatabaseInfo.from_json(_entry()).terms is None
