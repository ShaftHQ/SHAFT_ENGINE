#!/usr/bin/env python3
"""Session-start token-economy receipt and doctor check (#7242). Stdlib only.

One receipt line replaces the old entry retrieve hint. Doctor's `token-economy`
row fails when any core always-on card or the lazy map is missing or stale.
"""

from __future__ import annotations

import hashlib
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
# Pinned after the #7983 texts. Doctor fails when a copy does not match.
ETHICS_TEXT_SHA256 = {
    "companions/ethics-card.md": "d4978444698a231168f4a13e1933567919965c3ac6bad348e3d02baa14f79cf6",
    "references/ethical-conduct.md": "ed67e2d48b93d1d7c1b260c2ca8f26e63f851f64f922443ee815127b29fae1d1",
    "identity.md": "a9b7e86c8eb0a03d01f4e5f32478915b959f42ff53cc62bce6c33b314667b3cc",
}


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


INSTRUCTION_FILES = ("CLAUDE.md", "AGENTS.md", "GEMINI.md", ".github/copilot-instructions.md")
INSTRUCTION_MAX_LINES = 150


def instruction_overflow(project: Path, limit: int = INSTRUCTION_MAX_LINES) -> list[str]:
    """Always-loaded host instruction files over `limit` lines; detail belongs on demand."""
    over = []
    for rel in INSTRUCTION_FILES:
        path = project / rel
        try:
            lines = len(path.read_text(encoding="utf-8", errors="replace").splitlines()) if path.is_file() else 0
        except OSError:
            continue
        if lines > limit:
            over.append(f"{rel} {lines}>{limit} lines")
    return over


_ETHICS_FIXTURE = Path("evals/ethics-attestation-fixtures.json")
_FIXTURE_COUNT = 13
_FIXTURE_COUNTERPARTS = frozenset({"ai-accepted", "person-only", "unclear"})
_FIXTURE_DECISIONS = frozenset({"allow", "decline", "alternative"})


def _fixture_example_ok(example: object) -> bool:
    if not isinstance(example, dict):
        return False
    counterpart = example.get("counterpart")
    decision = example.get("decision")
    expect = example.get("expect")
    live_feed = example.get("live_feed")
    situation = example.get("situation")
    return (
        counterpart in _FIXTURE_COUNTERPARTS
        and decision in _FIXTURE_DECISIONS
        and expect in _FIXTURE_DECISIONS
        and expect == decision
        and isinstance(live_feed, bool)
        and isinstance(situation, str)
        and "?" not in situation
    )


def ethics_fixture_status(root: Path) -> tuple[str, str]:
    """Validate the attestation fixture when the file is installed. Absent is not a failure."""
    path = root / _ETHICS_FIXTURE
    if not path.is_file():
        return "pass", ""
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError):
        return "fail", f"ethics fixture schema: {_ETHICS_FIXTURE.as_posix()}"
    examples = document.get("examples") if isinstance(document, dict) else None
    if not isinstance(examples, list) or len(examples) != _FIXTURE_COUNT:
        return "fail", f"ethics fixture schema: {_ETHICS_FIXTURE.as_posix()}"
    for example in examples:
        if not _fixture_example_ok(example):
            return "fail", f"ethics fixture schema: {_ETHICS_FIXTURE.as_posix()}"
    return "pass", ""


def ethics_text_status(root: Path = HERE) -> tuple[str, str]:
    """Fail when ethics-card, ethical-conduct, or identity bytes drift from the pin."""
    for rel, digest in ETHICS_TEXT_SHA256.items():
        path = root / rel
        try:
            actual = hashlib.sha256(path.read_bytes()).hexdigest()
        except OSError:
            return "fail", f"ethics text missing: {rel}"
        if actual != digest:
            return "fail", f"ethics text hash mismatch: {rel}"
    return "pass", "ethics texts match"


def doctor_check(root: Path = HERE) -> tuple[str, str]:
    """Return (`pass`|`fail`, reason)."""
    for name, rel in CORE_CARDS.items():
        if not (root / rel).is_file():
            return "fail", f"{name} card missing: {rel}"
    hashed = ethics_text_status(root)
    if hashed[0] != "pass":
        return hashed
    fixture = ethics_fixture_status(root)
    if fixture[0] != "pass":
        return fixture
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
    over = instruction_overflow(root.parent)
    if over:
        return "warn", "move detail to on-demand references: " + ", ".join(over)
    return "pass", "core cards and lazy map present"
