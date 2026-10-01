"""Lean ChaosEngine harness contract (epic #6342): budgets, flow, agnostic core."""

from __future__ import annotations

import importlib.util
import json
import os
import re
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
        self.assertEqual(8, len(parity["hosts"]))

    def test_implement_path_fits_16_kib_on_every_host(self):
        for host in _chain()["hosts"]:
            with self.subTest(host=host):
                self.assertLessEqual(_implement_bytes(host), _chain()["implement_max_bytes"])
        self.assertLessEqual(_chain()["implement_max_bytes"], 16384)

    def test_orchestrate_path_fits_32_kib_on_every_host(self):
        extra = sum(_size(name) for name in _chain()["orchestrate"])
        for host in _chain()["hosts"]:
            with self.subTest(host=host):
                self.assertLessEqual(_implement_bytes(host) + extra, _chain()["orchestrate_max_bytes"])
        self.assertLessEqual(_chain()["orchestrate_max_bytes"], 32768)

    def test_session_start_is_a_small_locator(self):
        self.assertLessEqual(_session_start_bytes(), _chain()["session_start_max_bytes"])
        self.assertLessEqual(_chain()["session_start_max_bytes"], 600)

    def test_router_cannot_smuggle_mandatory_loads(self):
        head = _router_text().split("## Route", 1)[0]
        declared = {*_chain()["common"], *_chain()["orchestrate"], *_chain()["on_trigger"]}
        declared.add("chaos-engine/references/delegate-card.md")
        for link in re.findall(r"\]\(([^)#]+)", head):
            target = (ROUTER.parent / link).resolve().relative_to(ROOT.resolve()).as_posix()
            with self.subTest(link=link):
                self.assertIn(target, declared)


class CompanionTests(unittest.TestCase):
    def test_ultra_cards_are_small_pinned_and_linked(self):
        router = _router_text()
        for name in ("caveman", "ponytail"):
            card = CE / f"companions/{name}-ultra.md"
            with self.subTest(card=name):
                raw = card.read_text(encoding="utf-8")
                text = " ".join(raw.split())
                self.assertLessEqual(len(raw.encode("utf-8")), 1024)
                self.assertIn("ultra", text)
                self.assertIn('"Default: full" line does not apply', text)
                self.assertIn(f"companions/{name}-ultra.md", router)

    def test_caveman_scope_is_chat_and_handoffs_not_artifacts(self):
        text = " ".join((CE / "companions/caveman-ultra.md").read_text(encoding="utf-8").split())
        self.assertIn("handoffs", text)
        self.assertIn("PR bodies, issues", text)
        self.assertIn("professional prose", text)

    def test_session_start_points_at_cards_not_vendor_bodies(self):
        lifecycle = _load("ce_lean_lifecycle_cards", CE / "hooks/lifecycle.py")
        context = lifecycle.session_start_context(None, ACTIVATION)
        self.assertIn("caveman-ultra.md", context)
        self.assertIn("ponytail-ultra.md", context)
        self.assertNotIn("skills/caveman/SKILL.md", context)


class FlowTests(unittest.TestCase):
    def test_kanban_skill_encodes_wip_pull_dod_and_findings(self):
        raw = (CE / "skills/kanban/SKILL.md").read_text(encoding="utf-8")
        self.assertLessEqual(len(raw.encode("utf-8")), 6144)
        text = " ".join(raw.split())
        for phrase in ("WIP **1 writer**", "WIP **2**", "## Pull rule", "## Definition of Done",
                       "red first", "fixed", "filed", "not critical, blocker, or high"):
            with self.subTest(phrase=phrase):
                self.assertIn(phrase, text)

    def test_core_card_encodes_locked_decisions(self):
        text = " ".join(_router_text().split())
        for phrase in ("One fresh-context review", "second round only for",
                       "Learning Session only on trigger", "One blocking CI wait per push",
                       "retry at most once", "Every finding ends fixed",
                       "not critical, blocker, or high", "Measure thrice"):
            with self.subTest(phrase=phrase):
                self.assertIn(phrase, text)

    def test_identity_is_lean_and_agnostic(self):
        text = (CE / "identity.md").read_text(encoding="utf-8")
        self.assertLessEqual(len(text.encode("utf-8")), 1536)
        self.assertIn("CHAOSENGINE-IDENTITY-TRUTH:START", text)
        for phrase in ("measure thrice", "WIP", "Eliminate waste"):
            self.assertIn(phrase, text)

    def test_every_core_skill_is_linked_from_the_router(self):
        router = _router_text()
        for skill in sorted((CE / "skills").glob("*/SKILL.md")):
            name = skill.parent.name
            if name == "chaos-engine":
                continue
            with self.subTest(skill=name):
                self.assertIn(f"]({'../' + name}/SKILL.md)", router)


class LintTests(unittest.TestCase):
    def test_lean_lint_is_clean_against_its_shrink_only_allowlist(self):
        lint = _load("ce_lean_lint_t", ROOT / "scripts/ci/ce_lean_lint.py")
        self.assertEqual([], lint.violations(ROOT))

    def test_new_leak_is_caught(self):
        lint = _load("ce_lean_lint_t2", ROOT / "scripts/ci/ce_lean_lint.py")
        allow = lint.load_allowlist()
        allow["leaks"] = {}
        self.assertTrue(any(e.startswith("core-leak") for e in lint.violations(ROOT, allow)) or not lint.leak_counts(ROOT))


if __name__ == "__main__":
    unittest.main()


class LearningTriggerTests(unittest.TestCase):
    def _guard(self):
        sys.path.insert(0, str(CE / "hooks"))
        try:
            return _load("ce_lean_guard", CE / "hooks/guard.py")
        finally:
            sys.path.remove(str(CE / "hooks"))

    def test_delivery_without_trigger_owes_no_learning_session(self):
        guard = self._guard()
        entries = [{"kind": "task-activity", "activity": "delivery-complete"}]
        guard.reflection.entries = lambda _sid: entries
        self.assertIsNone(guard.learning_session_reason("s", {}))

    def test_failure_or_owner_ask_triggers_learning_session(self):
        guard = self._guard()
        guard.learning_completion_artifact = lambda _sid: None
        for trigger in ({"kind": "task-failure"},
                        {"kind": "task-activity", "activity": "learning-requested"}):
            entries = [{"kind": "task-activity", "activity": "delivery-complete"}, trigger]
            guard.reflection.entries = lambda _sid, e=entries: e
            with self.subTest(trigger=trigger):
                self.assertIn("trigger fired", guard.learning_session_reason("s", {}))
