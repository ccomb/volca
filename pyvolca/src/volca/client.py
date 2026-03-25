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


class VoLCAError(Exception):
    """Error from the VoLCA API."""

    def __init__(self, message: str, status_code: int | None = None, body: str = ""):
        self.status_code = status_code
        self.body = body
        super().__init__(message)


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
        self._session.headers["Accept"] = "application/json"
        if password:
            self._session.headers["Authorization"] = f"Bearer {password}"

    def _db_url(self, path: str) -> str:
        return f"{self.base_url}/api/v1/db/{self.db}/{path}"

    def _api_url(self, path: str) -> str:
        return f"{self.base_url}/api/v1/{path}"

    @staticmethod
    def _json(r: requests.Response):
        """Parse JSON response, raising a clear error on failure."""
        try:
            r.raise_for_status()
        except requests.HTTPError:
            body = r.text[:200]
            raise VoLCAError(
                f"{r.status_code} {r.reason} for {r.request.method} {r.url}: {body}",
                status_code=r.status_code, body=r.text,
            ) from None
        if not r.content:
            raise VoLCAError(
                f"Empty response for {r.request.method} {r.url} (status {r.status_code})",
                status_code=r.status_code, body="",
            )
        try:
            return r.json()
        except requests.exceptions.JSONDecodeError:
            hint = ""
            if "<!DOCTYPE" in r.text[:50] or "<html" in r.text[:50]:
                if r.history:
                    hint = (f" (redirected from {r.history[0].url} — "
                            "auth headers are dropped on redirect, try using https://)")
                else:
                    hint = " (got HTML — is the URL correct?)"
            raise VoLCAError(
                f"Non-JSON response for {r.request.method} {r.url} "
                f"(status {r.status_code}){hint}",
                status_code=r.status_code, body=r.text,
            ) from None

    def use(self, db_name: str) -> "Client":
        """Return a new client targeting a different database (shares session)."""
        c = Client.__new__(Client)
        c.base_url = self.base_url
        c.db = db_name
        c._session = self._session
        return c

    # -- Server info --

    def get_version(self) -> dict:
        """Return server version info (version, gitHash, gitTag, buildTarget)."""
        return self._json(self._session.get(self._api_url("version")))

    # -- Database management --

    def list_databases(self) -> list[dict]:
        return self._json(self._session.get(self._api_url("db")))["dlrDatabases"]

    def load_database(self, db_name: str) -> dict:
        return self._json(self._session.post(self._api_url(f"db/{db_name}/load")))

    def unload_database(self, db_name: str) -> dict:
        return self._json(self._session.post(self._api_url(f"db/{db_name}/unload")))

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
        return [Activity.from_json(a) for a in self._json(r)["srResults"]]

    def get_classifications(self) -> list[dict]:
        """List all classification systems and their values for the current database."""
        return self._json(self._session.get(self._db_url("classifications")))

    def search_flows(self, query: str | None = None, limit: int | None = None) -> list[dict]:
        params: dict = {}
        if limit is not None:
            params["limit"] = limit
        if query:
            params["q"] = query
        return self._json(self._session.get(self._db_url("flows"), params=params))["srResults"]

    # -- Activity details --

    def get_activity(self, process_id: str) -> dict:
        return self._json(self._session.get(self._db_url(f"activity/{process_id}")))

    def get_inputs(self, process_id: str) -> list[dict]:
        return self._json(self._session.get(self._db_url(f"activity/{process_id}/inputs")))

    def get_outputs(self, process_id: str) -> list[dict]:
        return self._json(self._session.get(self._db_url(f"activity/{process_id}/outputs")))

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
        return SupplyChain.from_json(self._json(r))

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
        return [Activity.from_json(a) for a in self._json(r)]

    # -- Tree --

    def get_tree(self, process_id: str) -> dict:
        return self._json(self._session.get(self._db_url(f"activity/{process_id}/tree")))

    # -- Inventory & LCIA --

    def get_inventory(self, process_id: str, substitutions: list[dict] | None = None) -> dict:
        url = self._db_url(f"activity/{process_id}/inventory")
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        return self._json(r)

    def get_lcia(self, process_id: str, method_id: str, substitutions: list[dict] | None = None) -> dict:
        url = self._db_url(f"activity/{process_id}/lcia/{method_id}")
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        return self._json(r)

    def get_lcia_batch(self, process_id: str, collection: str, substitutions: list[dict] | None = None) -> dict:
        url = self._db_url(f"activity/{process_id}/lcia-batch/{collection}")
        if substitutions:
            r = self._session.post(url, json=_substitution_body(substitutions))
        else:
            r = self._session.get(url)
        return self._json(r)
