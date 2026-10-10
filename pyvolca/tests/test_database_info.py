from volca.types import DatabaseInfo


def _entry(**extra):
    return {"name": "db", "displayName": "Db", "status": "loaded", "path": "/data/db", **extra}


def test_reads_the_licence_a_database_is_served_under():
    licence = {"kind": "standard", "id": "CC-BY-NC-4.0", "permissions": []}
    assert DatabaseInfo.from_json(_entry(licence=licence)).licence == licence


def test_an_engine_without_a_licence_leaves_it_unknown():
    assert DatabaseInfo.from_json(_entry()).licence is None


def test_reads_which_published_database_it_is():
    release = {"name": "base", "version": "3.12", "systemModel": "cut-off"}
    assert DatabaseInfo.from_json(_entry(release=release)).release == release
    assert DatabaseInfo.from_json(_entry()).release is None
