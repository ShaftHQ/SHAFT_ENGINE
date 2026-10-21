#!/usr/bin/env python3
"""Session-start token-economy receipt and doctor check (#7242). Stdlib only.

One receipt line replaces the old entry retrieve hint. Doctor's `token-economy`
row fails when any core always-on card or the lazy map is missing or stale.
"""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
CORE_CARDS = {
    "caveman": "companions/caveman-ultra.md",
    "ponytail": "companions/ponytail-ultra.md",
    "ethics": "companions/ethics-card.md",
}
PERSONA_MAX_BYTES = 2048
MISSING = "missing-fresh-install"


def _load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def persona_files(root: Path = HERE) -> list[Path]:
    """Add-on manifests may name one `persona` file relative to the add-on directory."""
    found = []
    for manifest in sorted((root / "addons").glob("*/addon.json")):
        try:
            relative = json.loads(manifest.read_text(encoding="utf-8")).get("persona")
        except (OSError, ValueError):
            continue
        if isinstance(relative, str) and relative and ".." not in Path(relative).parts:
            path = manifest.parent / relative
            if path.is_file() and path.stat().st_size <= PERSONA_MAX_BYTES:
                found.append(path)
    return found


def store_states(project: Path) -> tuple[str, str]:
    """`ready` when a store exists on disk; fresh installs report it honestly."""
    try:
        stores = _load(HERE / "stores.py", "_ce_stores_receipt")
        palace = stores.resolve_palace(project)
        graph = stores.resolve_graph_out(project)
    except Exception:  # nosec B110 - receipt must never break entry
        return MISSING, MISSING
    mem = "ready" if palace.is_dir() and any(palace.iterdir()) else MISSING
    gra = "ready" if (graph / "graph.json").is_file() else MISSING
    return mem, gra


def receipt(root: Path = HERE, project: Path | None = None, stopped: frozenset[str] = frozenset()) -> str:
    project = project or Path.cwd()
    cards = {name: ("ultra" if name not in stopped and (root / rel).is_file() else "off")
             for name, rel in CORE_CARDS.items() if name != "ethics"}
    ethics = "loaded" if (root / CORE_CARDS["ethics"]).is_file() else "missing"
    persona = "loaded" if persona_files(root) else "none"
    try:
        count = len(_load(root / "harness_map.py", "_ce_map_receipt").mapped_files(root))
    except Exception:  # nosec B110
        count = 0
    mem, gra = store_states(project)
    return (
        f"entry: caveman={cards['caveman']} ponytail={cards['ponytail']} ethics={ethics} "
        f"persona={persona} index={count} retrieve=mempalace:{mem} graphify:{gra}\n"
        "Retrieve before any broad search; record `retrieve: used|skipped(<why>)`. "
        "Map: `references/harness-map.md`\n"
    )


def doctor_check(root: Path = HERE) -> tuple[str, str]:
    """Return (`pass`|`fail`, reason)."""
    for name, rel in CORE_CARDS.items():
        if not (root / rel).is_file():
            return "fail", f"{name} card missing: {rel}"
    try:
        harness_map = _load(root / "harness_map.py", "_ce_map_doctor")
        current = (root / harness_map.MAP).read_text(encoding="utf-8")
    except Exception:
        return "fail", "harness map missing"
    # Installs ship a subset of the source tree, so the map may list absent files;
    # every installed harness file must still be listed (completeness).
    unlisted = [rel for _, rel in harness_map.mapped_files(root) if f"`{rel}`:" not in current]
    if unlisted:
        return "fail", f"harness map stale ({len(unlisted)} unlisted)"
    return "pass", "core cards and lazy map present"
