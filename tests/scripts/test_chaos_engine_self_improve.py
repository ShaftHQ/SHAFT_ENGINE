"""Standalone self-improve skill — activation + learning.py wrap (#5613)."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/self-improve/SKILL.md"
RESEARCH = ROOT / "chaos-engine/skills/self-improve/references/research-adopt-reject.md"
TAXONOMY = ROOT / "chaos-engine/skills/self-improve/references/observation-taxonomy.md"
ACTIVATION = ROOT / "chaos-engine/skills/self-improve/references/activation.md"
UPSTREAM = ROOT / "chaos-engine/skills/self-improve/UPSTREAM.md"
LICENSE = ROOT / "chaos-engine/skills/self-improve/LICENSE"
NOTICES = ROOT / "chaos-engine/THIRD_PARTY_NOTICES.md"
LEARNING = ROOT / "chaos-engine/learning.py"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class SelfImproveSkillTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.lifecycle = load(ROOT / "chaos-engine/hooks/lifecycle.py", "ce_lifecycle_si")
        cls.learning = load(LEARNING, "ce_learning_si")

    def test_skill_surface_and_attribution(self):
        for path in (SKILL, RESEARCH, TAXONOMY, ACTIVATION, UPSTREAM, LICENSE):
            self.assertTrue(path.is_file(), path)
        skill = SKILL.read_text(encoding="utf-8")
        self.assertIn("name: self-improve", skill)
        self.assertIn("dual track", skill.casefold())
        self.assertIn("learning.py", skill)
        self.assertNotIn("shaft", skill.casefold())
        research = RESEARCH.read_text(encoding="utf-8")
        self.assertIn("Adopted", research)
        self.assertIn("Rejected", research)
        notices = NOTICES.read_text(encoding="utf-8")
        self.assertIn("Task Observer", notices)
        self.assertIn("CC BY 4.0", notices)
        self.assertIn("Eoghan Henn", notices)

    def test_session_start_locator_is_cheap(self):
        context = self.lifecycle.session_start_context(None, "activation")
        self.assertIn("self-improve", context.casefold())
        self.assertIn("self-improve", context.casefold())
        self.assertLessEqual(
            len(context.encode("utf-8")),
            self.lifecycle.SESSION_START_MAX_BYTES,
        )
        # Always-on must not dump observation taxonomy bodies.
        self.assertNotIn("siblings_checked", context)

    def test_learning_queue_harness_and_product_candidates(self):
        harness = {
            "category": "tooling",
            "title": "Headroom doctor fix-next gap",
            "lesson": "Operators needed one install command after the pin landed",
            "proposedChange": "Document uv tool install in doctor fix-next",
            "benefit": "Faster optional Headroom provisioning",
            "estimatedTokens": 100,
        }
        product = {
            "category": "guidance",
            "title": "Clarify empty-state copy",
            "lesson": "Users misread an empty dashboard as an error",
            "proposedChange": "Add empty-state guidance in the product UI",
            "benefit": "Fewer false support tickets",
            "estimatedTokens": 80,
        }
        with tempfile.TemporaryDirectory() as temporary:
            state = Path(temporary)
            first = self.learning.queue_learning(state, harness, "Owner/ExampleRepo")
            second = self.learning.queue_learning(state, product, "Owner/ExampleRepo")
            self.assertEqual("queued", first["status"])
            self.assertEqual("queued", second["status"])
            document = self.learning.queue_document(state)
            self.assertEqual(2, len(document["items"]))
            raw = (state / "queue.json").read_text(encoding="utf-8")
            self.assertNotIn("/home/", raw)
            self.assertNotIn("password", raw.casefold())

    def test_router_skill_points_at_self_improve(self):
        router = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("self-improve", router)

    def test_activation_forbids_ce_untouched_skip(self):
        activation = ACTIVATION.read_text(encoding="utf-8")
        self.assertIn("not a valid skip", activation.casefold())
        self.assertIn("harness parity", activation.casefold())
        self.assertIn("hooks own", activation.casefold())
        self.assertIn("learning session", activation.casefold())
        router = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("untouched", router.casefold())
        self.assertIn("harness queued", router.casefold())
        life = (ROOT / "chaos-engine/references/lifecycle-hooks.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("gh pr merge", life)
        self.assertIn("not a valid skip", life.casefold())

    def test_portable_guard_marks_pr_merge_as_confirmed_delivery(self):
        guard = load(ROOT / "chaos-engine/hooks/guard.py", "ce_guard_si_delivery")
        self.assertTrue(guard.confirmed_delivery_command("gh pr merge 123 --merge"))
        self.assertTrue(
            guard.confirmed_delivery_command(
                "py -3 scripts/agents/chaos_engine_cli.py delivery-status "
                "--manifest m --receipt-out r"
            )
        )
        self.assertFalse(guard.confirmed_delivery_command("git push origin HEAD"))
        self.assertFalse(
            guard.confirmed_delivery_command("gh pr create --title x --body y")
        )
        source = (ROOT / "chaos-engine/hooks/guard.py").read_text(encoding="utf-8")
        self.assertIn("not a valid skip", source)
        self.assertIn("confirmed_delivery_command", source)
        self.assertIn("harness queued N / product queued N / nothing durable", source)



if __name__ == "__main__":
    unittest.main()
