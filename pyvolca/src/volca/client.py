"""HTTP client for all VoLCA API endpoints."""

import requests

from .types import Activity, SupplyChain


def _substitution_body(substitutions: list[dict]) -> dict:
    """Build request body for substitution endpoints."""
    return {
        "srSubstitutions": [
            {"subFrom": s["from"], "subTo": s["to"], "subConsumer": s["consumer"]}
            for s in substitutions
        ]
    }


class Client:
    """HTTP client for the VoLCA REST API.

    Usage::

        c = Client(db="agribalyse-3.2", password="1234")
        plants = c.search_activities(name="at plant")
        chain = c.get_supply_chain(plants[0].process_id, name="at farm")

    Substitutions can be passed to get_supply_chain, get_inventory, get_lcia,
    and get_lcia_batch to compute results with Sherman-Morrison rank-1 updates::

        subs = [{"from": old_pid, "to": new_pid, "consumer": consumer_pid}]
        result = c.get_lcia(pid, method_id, substitutions=subs)
    """

    def __init__(self, base_url: str = "http://localhost:8081", db: str = "", password: str = ""):
        self.base_url = base_url.rstrip("/")
        self.db = db
        self._session = requests.Session()
        if password:
            self._session.headers["Authorization"] = f"Bearer {password}"

    def _db_url(self, path: str) -> str:
        return f"{self.base_url}/api/v1/db/{self.db}/{path}"

    def _api_url(self, path: str) -> str:
        return f"{self.base_url}/api/v1/{path}"

    def use(self, db_name: str) -> "Client":
        """Return a new client targeting a different database (shares session)."""
        c = Client.__new__(Client)
        c.base_url = self.base_url
        c.db = db_name
        c._session = self._session
        return c

    # -- Database management --

    def list_databases(self) -> list[dict]:
        r = self._session.get(self._api_url("db"))
        r.raise_for_status()
        return r.json()["dlrDatabases"]

    def load_database(self, db_name: str) -> dict:
        r = self._session.post(self._api_url(f"db/{db_name}/load"))
        r.raise_for_status()
        return r.json()

    def unload_database(self, db_name: str) -> dict:
        r = self._session.post(self._api_url(f"db/{db_name}/unload"))
        r.raise_for_status()
        return r.json()

    # -- Search --

    def search_activities(
        self,
        name: str | None = None,
        geo: str | None = None,
        product: str | None = None,
        classification: str | None = None,
        classification_value: str | None = None,
        limit: int | None = None,
        offset: int = 0,
    ) -> list[Activity]:
        params: dict = {"offset": offset}
        if limit is not None:
            params["limit"] = limit
        if name:
            params["name"] = name
        if geo:
            params["geo"] = geo
        if product:
            params["product"] = product
        if classification:
            params["classification"] = classification
        if classification_value:
            params["classification-value"] = classification_value
        r = self._session.get(self._db_url("activities"), params=params)
        r.raise_for_status()
        return [Activity.from_json(a) for a in r.json()["srResults"]]

    def get_classifications(self) -> list[dict]:
        """List all classification systems and their values for the current database."""
        r = self._session.get(self._db_url("classifications"))
        r.raise_for_status()
        return r.json()

    def search_flows(self, query: str | None = None, limit: int | None = None) -> list[dict]:
        params: dict = {}
        if limit is not None:
            params["limit"] = limit
        if query:
            params["q"] = query
        r = self._session.get(self._db_url("flows"), params=params)
        r.raise_for_status()
        return r.json()["srResults"]

    # -- Activity details --

    def get_activity(self, process_id: str) -> dict:
        r = self._session.get(self._db_url(f"activity/{process_id}"))
        r.raise_for_status()
        return r.json()

    def get_inputs(self, process_id: str) -> list[dict]:
        r = self._session.get(self._db_url(f"activity/{process_id}/inputs"))
        r.raise_for_status()
        return r.json()

    def get_outputs(self, process_id: str) -> list[dict]:
        r = self._session.get(self._db_url(f"activity/{process_id}/outputs"))
        r.raise_for_status()
        return r.json()

    # -- Supply chain (scaling vector based) --

    def get_supply_chain(
        self,
        process_id: str,
        name: str | None = None,
        limit: int | None = None,
        min_quantity: float = 0,
        substitutions: list[dict] | None = None,
    ) -> SupplyChain:
        params: dict = {}
        if limit is not None:
            params["limit"] = limit
        if name:
            params["name"] = name
        if min_quantity > 0:
            params["min-quantity"] = min_quantity
        url = self._db_url(f"activity/{process_id}/supply-chain")
        if substitutions:
            r = self._session.post(url, params=params, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url, params=params)
        r.raise_for_status()
        return SupplyChain.from_json(r.json())

    # -- Consumers (reverse supply chain) --

    def get_consumers(
        self,
        process_id: str,
        name: str | None = None,
        limit: int | None = None,
    ) -> list[Activity]:
        """Find all activities that transitively depend on this supplier."""
        params: dict = {}
        if name:
            params["name"] = name
        if limit is not None:
            params["limit"] = limit
        r = self._session.get(self._db_url(f"activity/{process_id}/consumers"), params=params)
        r.raise_for_status()
        return [Activity.from_json(a) for a in r.json()]

    # -- Tree --

    def get_tree(self, process_id: str) -> dict:
        r = self._session.get(self._db_url(f"activity/{process_id}/tree"))
        r.raise_for_status()
        return r.json()

    # -- Inventory & LCIA --

    def get_inventory(self, process_id: str, substitutions: list[dict] | None = None) -> dict:
        url = self._db_url(f"activity/{process_id}/inventory")
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        r.raise_for_status()
        return r.json()

    def get_lcia(self, process_id: str, method_id: str, substitutions: list[dict] | None = None) -> dict:
        url = self._db_url(f"activity/{process_id}/lcia/{method_id}")
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        r.raise_for_status()
        return r.json()

    def get_lcia_batch(self, process_id: str, collection: str, substitutions: list[dict] | None = None) -> dict:
        url = self._db_url(f"activity/{process_id}/lcia-batch/{collection}")
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        r.raise_for_status()
        return r.json()
