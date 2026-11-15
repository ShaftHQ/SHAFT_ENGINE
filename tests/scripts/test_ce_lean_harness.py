"""Lean ChaosEngine harness contract (epic #6342): budgets, flow, agnostic core."""

from __future__ import annotations

import importlib.util
import json
import os
import re
import subprocess
import sys
import unittest
from pathlib import Path

ROOT = Path(os.environ.get("CE_LEAN_ROOT") or Path(__file__).resolve().parents[2])
CE = ROOT / "chaos-engine"
ROUTER = CE / "skills/chaos-engine/SKILL.md"
BUDGET = json.loads((ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8"))
ACTIVATION = "Follow .chaos-engine/skills/chaos-engine/SKILL.md before continuing."

def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module

def _size(relative: str) -> int:
    path = ROOT / relative
    return len(path.read_text(encoding="utf-8").encode("utf-8")) if path.is_file() else 0

def _chain() -> dict:
    return BUDGET.get("lean_chain", {})

def implement_path_files() -> list[str]:
    chain = _chain()
    paths = set(chain["common"])
    paths.add(chain["profile"])
    for host_paths in chain["hosts"].values():
        paths.update(host_paths)
    return sorted(paths)

def _paragraphs(text: str) -> list[str]:
    blocks: list[str] = []
    buf: list[str] = []

    def flush() -> None:
        if buf:
            blocks.append(" ".join(buf))
            buf.clear()

    for line in text.splitlines():
        stripped = line.strip()
        if (
            not stripped
            or stripped.startswith("#")
            or stripped.startswith("|")
            or stripped.startswith("```")
        ):
            flush()
            continue
        if stripped.startswith(("- ", "* ")) and buf:
            flush()
        buf.append(stripped)
    flush()
    return blocks

def sentences_of(text: str) -> set[str]:
    found: set[str] = set()
    for paragraph in _paragraphs(text):
        for part in re.split(r"(?<=[.!?])\s+", paragraph):
            part = part.strip()
            if part:
                found.add(part)
    return found

def lost_unique_sentences(before: dict[str, str], after: dict[str, str]) -> list[str]:
    before_all: set[str] = set()
    after_all: set[str] = set()
    for text in before.values():
        before_all |= sentences_of(text)
    for text in after.values():
        after_all |= sentences_of(text)
    return sorted(before_all - after_all)

def _session_start_bytes() -> int:
    lifecycle = _load("ce_lean_lifecycle", CE / "hooks/lifecycle.py")
    return len(lifecycle.session_start_context("t" * 36, ACTIVATION).encode("utf-8"))

def _implement_bytes(host: str) -> int:
    chain = _chain()
    files = [*chain["hosts"][host], *chain["common"], chain["profile"]]
    return sum(_size(name) for name in dict.fromkeys(files)) + _session_start_bytes()

def _router_text() -> str:
    return ROUTER.read_text(encoding="utf-8")

class BudgetTests(unittest.TestCase):
    def test_lean_chain_declared_for_every_parity_host(self):
        parity = json.loads((ROOT / "scripts/ci/agent_harness_parity.json").read_text(encoding="utf-8"))
        self.assertEqual(set(parity["hosts"]), set(_chain().get("hosts", {})))
        self.assertEqual(9, len(parity["hosts"]))

    def test_deleted_unique_sentence_is_named(self):
        before = {"a.md": "Use the tool for memory."}
        after = {"a.md": ""}
        self.assertEqual(["Use the tool for memory."], lost_unique_sentences(before, after))

    def test_duplicate_sentence_may_be_deleted_once(self):
        sentence = "Use the tool for memory."
        before = {"a.md": sentence, "b.md": sentence}
        after = {"a.md": "", "b.md": sentence}
        self.assertEqual([], lost_unique_sentences(before, after))

    def test_hard_wrap_is_one_sentence(self):
        before = {"a.md": "Use the tool\nfor memory."}
        after = {"a.md": "Use the tool for memory."}
        self.assertEqual([], lost_unique_sentences(before, after))
        gone = lost_unique_sentences(before, {"a.md": ""})
        self.assertEqual(1, len(gone))
        self.assertIn("Use the tool for memory.", gone[0])

if __name__ == "__main__":
    unittest.main()

AGNOSTIC_SCOPES = (
    "chaos-engine/references/",
    "chaos-engine/skills/",
    "chaos-engine/hooks/",
    "chaos-engine/identity.md",
    "chaos-engine/companions/",
)
LOCAL_LLM_SKILLS = (
    "omniroute",
    "local-agency",
    "local-runtimes",
    "local-openai-compat",
    "colibri",
    "freetoken",
    "local-coding-delegate",
)

class AgnosticCoreTests(unittest.TestCase):
    """CE-12/13/14 (#6354, #6355, #6356)."""

    def test_allowlist_is_empty_for_the_agnostic_core_scopes(self):
        allow = json.loads((ROOT / "scripts/ci/ce_lean_allowlist.json").read_text(encoding="utf-8"))
        for kind in ("leaks", "issueTags"):
            scoped = sorted(path for path in allow[kind] if path.startswith(AGNOSTIC_SCOPES))
            self.assertEqual([], scoped, kind)
        self.assertEqual({}, allow["nearDuplicates"])

    def test_personal_authorizations_live_outside_the_repository(self):
        for path in (CE / "profiles").rglob("*.md"):
            text = path.read_text(encoding="utf-8")
            for secret in ("F79E3F65", "Mohab.MohieElDeen@", "Anyone with the link"):
                self.assertNotIn(secret, text, path)
        entry = (ROOT / "shaft-skills/ce-pack/entrypoint.md").read_text(encoding="utf-8")
        self.assertIn("chaos-engine/authorizations.md", entry)

