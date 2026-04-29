"""Verify every ``python`` code block in README.md executes.

Catches silently-broken examples on PyPI: a renamed method, a removed
dataclass field, a typo in a kwarg name. The mocked Client (provided by
the ``readme_namespace`` fixture) returns realistic typed values, so the
examples exercise real attribute access against the real ``volca.types``
classes.

Blocks whose first non-whitespace line starts with ``# no-test`` are
skipped — used for the Quick start, which needs an actual engine via
:class:`volca.Server`.
"""

from __future__ import annotations

import re
from pathlib import Path

README = Path(__file__).resolve().parent.parent / "README.md"

_PYTHON_BLOCK = re.compile(r"```python\n(.*?)```", re.DOTALL)


def _runnable_blocks(text: str) -> list[str]:
    blocks: list[str] = []
    for match in _PYTHON_BLOCK.finditer(text):
        body = match.group(1)
        first_line = next((ln.strip() for ln in body.splitlines() if ln.strip()), "")
        if first_line.startswith("# no-test"):
            continue
        blocks.append(body)
    return blocks


def test_readme_has_runnable_examples() -> None:
    blocks = _runnable_blocks(README.read_text(encoding="utf-8"))
    assert len(blocks) >= 5, (
        f"README has too few runnable python blocks ({len(blocks)}); the doc "
        "may have lost its capability examples."
    )


def test_all_runnable_blocks_execute(readme_namespace: dict) -> None:
    """Run every non-skipped python block in README order, in one shared namespace.

    Earlier blocks (e.g. ``plants = c.search_activities(...)``) define names
    that later blocks reference. A failure in any block fails the test.
    """
    blocks = _runnable_blocks(README.read_text(encoding="utf-8"))
    for i, block in enumerate(blocks, start=1):
        try:
            exec(compile(block, f"<README:block{i}>", "exec"), readme_namespace)
        except Exception as exc:  # noqa: BLE001 — surface the offending block
            raise AssertionError(
                f"README block #{i} failed to execute: {exc!r}\n"
                f"--- block source ---\n{block}\n--- end ---"
            ) from exc
