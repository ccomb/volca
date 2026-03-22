"""HTTP client for all VoLCA API endpoints."""

import requests

from .types import Activity, SupplyChain, Variant


class Client:
    """HTTP client for the VoLCA REST API.

    Usage::

        c = Client(db="agribalyse-3.2", password="1234")
        plants = c.search_activities(name="at plant")
        chain = c.get_supply_chain(plants[0].process_id, name="at farm")
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
        r = self._session.get(self._api_url("database"))
        r.raise_for_status()
        return r.json()["dlrDatabases"]

    def load_database(self, db_name: str) -> dict:
        r = self._session.post(self._api_url(f"database/{db_name}/load"))
        r.raise_for_status()
        return r.json()

    def unload_database(self, db_name: str) -> dict:
        r = self._session.post(self._api_url(f"database/{db_name}/unload"))
        r.raise_for_status()
        return r.json()

    # -- Search --

    def search_activities(
        self,
        name: str | None = None,
        geo: str | None = None,
        product: str | None = None,
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
        r = self._session.get(self._db_url("activities"), params=params)
        r.raise_for_status()
        return [Activity.from_json(a) for a in r.json()["srResults"]]

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
    ) -> SupplyChain:
        params: dict = {}
        if limit is not None:
            params["limit"] = limit
        if name:
            params["name"] = name
        if min_quantity > 0:
            params["min-quantity"] = min_quantity
        r = self._session.get(
            self._db_url(f"activity/{process_id}/supply-chain"),
            params=params,
        )
        r.raise_for_status()
        return SupplyChain.from_json(r.json())

    # -- Variants (Sherman-Morrison substitution) --

    def create_variant(
        self,
        process_id: str,
        substitutions: list[dict],
    ) -> Variant:
        """Create a variant by substituting suppliers.

        Args:
            process_id: Base activity ProcessId
            substitutions: List of {"from": old_supplier_pid, "to": new_supplier_pid}

        Returns:
            Variant with modified supply chain
        """
        body = {
            "vrSubstitutions": [
                {"subFrom": s["from"], "subTo": s["to"]}
                for s in substitutions
            ]
        }
        r = self._session.post(
            self._db_url(f"activity/{process_id}/variant"),
            json=body,
        )
        r.raise_for_status()
        return Variant.from_json(r.json())

    # -- Tree --

    def get_tree(self, process_id: str) -> dict:
        r = self._session.get(self._db_url(f"activity/{process_id}/tree"))
        r.raise_for_status()
        return r.json()

    # -- Inventory & LCIA --

    def get_inventory(self, process_id: str) -> dict:
        r = self._session.get(self._db_url(f"activity/{process_id}/inventory"))
        r.raise_for_status()
        return r.json()

    def get_lcia(self, process_id: str, method_id: str) -> dict:
        r = self._session.get(self._db_url(f"activity/{process_id}/lcia/{method_id}"))
        r.raise_for_status()
        return r.json()

    def get_lcia_batch(self, process_id: str, collection: str) -> list[dict]:
        r = self._session.get(
            self._db_url(f"activity/{process_id}/lcia-batch/{collection}")
        )
        r.raise_for_status()
        return r.json()
