"""The scores a reduced model gives at many sets of parameter values, with numpy.

A model holds the processes its parameters move, each column as formula trees,
and, for each supplier of those columns, its scores and the scaling it gives the
moving processes, all at the database's own values. At other values the moving
columns change by D (suppliers × moving processes); their scalings become
``w = (I - M·D)⁻¹ x0`` and every score ``s0 + u·(D·w) + Σ Δg·w``, which is the
engine's own score, loops included. Each set solves a system of the size of the
moving processes; sets are taken a few at a time so memory stays bounded.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Callable

if TYPE_CHECKING:
    from .types import ReducedModel


# Bytes of the per-set arrays one chunk may hold (differences and systems).
_CHUNK_BYTES = 256 * 2**20


def _numpy() -> Any:
    try:
        import numpy
    except ImportError as exc:  # pragma: no cover - the message is the point
        raise ImportError("Evaluating a reduced model needs numpy: pip install 'pyvolca[model]'") from exc
    return numpy


def _functions(np: Any) -> dict[tuple[str, int], Callable[..., Any]]:
    """VoLCA's functions, by the name or operator a tree spells them with and their number of arguments."""

    def truth(holds: Any) -> Any:
        return np.where(holds, 1.0, 0.0)

    def half_up(x: Any) -> Any:
        return np.floor(x + 0.5)

    def quotient(a: Any, b: Any) -> Any:
        d = half_up(b)
        return np.where(d == 0, np.nan, np.trunc(half_up(a) / np.where(d == 0, 1.0, d)))

    one = {
        "-": np.negative,
        "not": lambda x: truth(x == 0),
        "abs": np.abs,
        "sqrt": np.sqrt,
        "sqr": lambda x: x * x,
        "exp": np.exp,
        "ln": np.log,
        "log10": np.log10,
        "trunc": np.trunc,
        "frac": lambda x: x - np.trunc(x),
        "round": half_up,
        "roundeven": np.rint,
        "ceil": np.ceil,
        "floor": np.floor,
        "sin": np.sin,
        "cos": np.cos,
        "tan": np.tan,
        "cotan": lambda x: 1.0 / np.tan(x),
        "asin": np.arcsin,
        "acos": np.arccos,
        "atan": np.arctan,
        "sinh": np.sinh,
        "cosh": np.cosh,
        "tanh": np.tanh,
    }
    two = {
        "+": np.add,
        "-": np.subtract,
        "*": np.multiply,
        "/": np.divide,
        "^": np.power,
        "=": lambda a, b: truth(a == b),
        "<>": lambda a, b: truth(a != b),
        "<": lambda a, b: truth(a < b),
        "<=": lambda a, b: truth(a <= b),
        ">": lambda a, b: truth(a > b),
        ">=": lambda a, b: truth(a >= b),
        "log": lambda x, base: np.log(x) / np.log(base),
        "ipower": lambda x, n: np.power(x, np.trunc(n)),
        "xor": lambda a, b: truth((a != 0) != (b != 0)),
        "div": quotient,
        "mod": lambda a, b: a - b * np.trunc(a / b),
    }
    table: dict[tuple[str, int], Callable[..., Any]] = {(name, 1): f for name, f in one.items()}
    table.update({(name, 2): f for name, f in two.items()})
    table[("if", 3)] = lambda c, yes, no: np.where(c != 0, yes, no)
    return table


_ANY_ARGUMENTS = {
    "min": lambda np, xs: np.minimum.reduce(np.broadcast_arrays(*xs)),
    "max": lambda np, xs: np.maximum.reduce(np.broadcast_arrays(*xs)),
    "sum": lambda np, xs: sum(xs),
    "average": lambda np, xs: sum(xs) / len(xs),
    "and": lambda np, xs: np.where(np.logical_and.reduce(np.broadcast_arrays(*[x != 0 for x in xs])), 1.0, 0.0),
    "or": lambda np, xs: np.where(np.logical_or.reduce(np.broadcast_arrays(*[x != 0 for x in xs])), 1.0, 0.0),
}


class _Evaluator:
    """Formula trees computed over arrays: one value per set of parameter values."""

    def __init__(self, np: Any):
        self.np = np
        self.table = _functions(np)

    def __call__(self, tree: Any, env: dict[str, Any]) -> Any:
        if isinstance(tree, (int, float)):
            return float(tree)
        if isinstance(tree, str):
            try:
                return env[tree]
            except KeyError:
                raise ValueError(f"unknown variable {tree}") from None
        word, *arguments = tree
        values = [self(a, env) for a in arguments]
        if word in _ANY_ARGUMENTS:
            return _ANY_ARGUMENTS[word](self.np, values)
        f = self.table.get((word, len(values)))
        if f is None:
            raise ValueError(f"the formula language has no function {word!r} of {len(values)} arguments; a newer pyvolca may")
        return f(*values)

    def bound(self, env: dict[str, Any], bindings: list[dict]) -> dict[str, Any]:
        """Names given their values, then each binding computed in turn."""
        env = dict(env)
        for b in bindings:
            env[b["name"]] = self(b["formula"], env)
        return env


class _Column:
    """A moving process's column at some sets of values: its suppliers' entries and its emissions scored by method."""

    def __init__(self, technosphere: Any, emissions: Any, reference: Any):
        self.technosphere = technosphere  # (sets, suppliers)
        self.emissions = emissions  # (sets, methods)
        self.reference = reference  # (sets,)


def _columns(model: ReducedModel, values: Any, first_set: int, ev: _Evaluator) -> list[_Column]:
    np = ev.np
    n = values.shape[0]
    defaults = np.array([[p.parameter.value or 0.0 for p in model.parameters]])
    m = len(model.methods)
    s = len(model.suppliers)
    columns = []
    for process in model.processes:
        fixed = process["values"]

        def scope(at: Any, process: dict = process, fixed: dict = fixed) -> dict[str, Any]:
            free = {name: at[:, i] for name, i in process["parameters"].items()}
            return ev.bound({**fixed, **free}, process["bindings"])

        at, at0 = scope(values), scope(defaults)

        def amount(line: dict, what: str, at: dict = at, at0: dict = at0, process: dict = process) -> Any:
            """The engine's rule: a formula giving at these values what it gives at the defaults keeps the stored amount."""
            stored = line["stored"]
            if line.get("formula") is None:
                return np.full(n, stored)
            v = np.broadcast_to(ev(line["formula"], at), (n,)).astype(float)
            v0 = ev(line["formula"], at0)
            bad = ~np.isfinite(v)
            if bad.any():
                raise ValueError(f"set {first_set + int(np.argmax(bad))}: {what} of {process['name']!r} has no finite amount")
            return np.where(v == v0, stored, v)

        ref = process["reference"]
        o = sum((amount(line, "a reference line") for line in ref["outputs"]), np.zeros(n))
        loops = sum((amount(line, "a reference line") for line in ref["selfLoops"]), np.zeros(n))
        inputs = sum((np.abs(amount(line, "a reference line")) for line in ref["inputs"]), np.zeros(n))
        reference = np.where((np.abs(o) > 1e-15) & (np.abs(o - loops) > 1e-15), o - loops, np.where(inputs > 1e-15, inputs, 1.0))
        tech = np.zeros((n, s))
        for line in process["technosphere"]:
            tech[:, line["supplier"]] += line["factor"] * amount(line["line"], "an input") / reference
        emissions = np.zeros((n, m))
        for line in process["biosphere"]:
            emissions += np.outer(amount(line["line"], "an emission") / reference, line["factors"])
        columns.append(_Column(tech, emissions, reference))
    return columns


def _root_flips(model: ReducedModel, at: list[_Column], at0: list[_Column], first_set: int, np: Any) -> None:
    """The engine scores a root by its reference's sign at the defaults: a set that turns it over is refused."""
    for process, c, c0 in zip(model.processes, at, at0):
        if process["processId"] == model.process_id or process["processId"].startswith(model.process_id + "_"):
            flipped = np.sign(c.reference) != np.sign(c0.reference)
            if flipped.any():
                raise ValueError(f"set {first_set + int(np.argmax(flipped))}: the reference amount of {process['name']!r} changes sign")


def method_scores(model: ReducedModel, values: Any) -> Any:
    """The score of every method at every set: (sets, methods)."""
    np = _numpy()
    ev = _Evaluator(np)
    values = np.atleast_2d(np.asarray(values, dtype=float))
    if values.shape[1] != len(model.parameters):
        raise ValueError(f"each set holds {len(model.parameters)} values, one per parameter of the model, not {values.shape[1]}")
    k, s = len(model.processes), len(model.suppliers)
    u = np.array([sup["scores"] for sup in model.suppliers]).reshape(s, len(model.methods))
    mm = np.array([sup["scalings"] for sup in model.suppliers]).reshape(s, k).T  # (moving, suppliers)
    x0 = np.array([p["scaling"] for p in model.processes])
    s0 = np.array(model.baseline)
    defaults = np.array([[p.parameter.value or 0.0 for p in model.parameters]])
    with np.errstate(all="ignore"):  # a NaN matters only on an amount, which _columns refuses by name
        return _solved(model, values, defaults, u, mm, x0, s0, ev)


def _solved(model: ReducedModel, values: Any, defaults: Any, u: Any, mm: Any, x0: Any, s0: Any, ev: _Evaluator) -> Any:
    np = ev.np
    k, s = len(model.processes), len(model.suppliers)
    at0 = _columns(model, defaults, 0, ev)
    chunk = max(1, _CHUNK_BYTES // (8 * max(1, k * (s + k))))
    out = []
    for start in range(0, values.shape[0], chunk):
        sets = values[start : start + chunk]
        at = _columns(model, sets, start, ev)
        _root_flips(model, at, at0, start, np)
        d = np.stack([c.technosphere - c0.technosphere for c, c0 in zip(at, at0)], axis=2) if k else np.zeros((len(sets), s, 0))
        system = np.eye(k) - mm @ d  # (sets, moving, moving)
        w = np.linalg.solve(system, np.broadcast_to(x0, (len(sets), k))[..., None])[..., 0]
        dw = np.einsum("nsk,nk->ns", d, w)
        dg = np.stack([c.emissions - c0.emissions for c, c0 in zip(at, at0)], axis=2) if k else np.zeros((len(sets), len(s0), 0))
        out.append(s0 + dw @ u + np.einsum("nmk,nk->nm", dg, w))
    return np.concatenate(out) if out else np.zeros((0, len(s0)))


def scoring_set_scores(model: ReducedModel, scores: Any) -> dict[str, dict[str, Any]]:
    """Each scoring set's scores, from the method scores, as the engine computes them."""
    np = _numpy()
    ev = _Evaluator(np)
    sets = {}
    with np.errstate(all="ignore"):
        for ss in model.scoring_sets:
            raw = ev.bound({v: scores[:, j] for v, j in ss["variables"].items()}, ss["computed"])
            weighted = {v: x / ss["normalization"].get(v, 1.0) * ss["weighting"].get(v, 0.0) for v, x in raw.items()}
            sets[ss["name"]] = {name: np.broadcast_to(ev(f, weighted), (scores.shape[0],)) * ss["displayMultiplier"] for name, f in ss["scores"].items()}
    return sets
