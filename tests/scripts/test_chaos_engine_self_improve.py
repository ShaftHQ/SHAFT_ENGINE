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
        self.assertNotIn("self-improve", context.casefold())
        self.assertLessEqual(
            len(context.encode("utf-8")),
            self.lifecycle.SESSION_START_MAX_BYTES,
        )
        # Always-on must not dump observation taxonomy bodies.
        self.assertNotIn("siblings_checked", context)

    def test_learning_queue_harness_and_product_candidates(self):
        harness = {
            "category": "tooling",
            "title": "Graphify doctor fix-next gap",
            "lesson": "Operators needed one repair command after a store probe failed",
            "proposedChange": "Document repair --component graphify in doctor fix-next",
            "benefit": "Faster store repair on adopter hosts",
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
            with self.assertRaisesRegex(ValueError, "GitHub issues only"):
                self.learning.queue_learning(state, harness, "Owner/ExampleRepo", track="harness")
            with self.assertRaisesRegex(ValueError, "GitHub issues only"):
                self.learning.queue_learning(state, harness, "Owner/ExampleRepo")
            self.assertFalse((state / "queue.json").exists())
            second = self.learning.queue_learning(
                state, product, "Owner/ExampleRepo", track="product"
            )
            self.assertEqual("queued", second["status"])
            document = self.learning.queue_document(state)
            self.assertEqual(1, len(document["items"]))
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
        self.assertIn("learning session after every final delivery", activation.casefold())
        self.assertIn("harness parity", activation.casefold())
        self.assertIn("hooks own", activation.casefold())
        self.assertIn("learning session", activation.casefold())
        router = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("learning session after every final delivery", router.casefold())
        self.assertNotIn("only on trigger", router.casefold())
        self.assertIn("never local queues", router.casefold())
        life = (ROOT / "chaos-engine/references/lifecycle-hooks.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("gh pr merge", life)
        self.assertIn("learning session after every final delivery", life.casefold())

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
        self.assertIn("Learning Session after every final delivery", source)
        self.assertNotIn("learning_triggered", source)
        self.assertIn("confirmed_delivery_command", source)
        self.assertIn("Do not write them to a local queue or into chat.", source)
        self.assertNotIn("harness queued N", source)


    def test_learning_session_runs_after_every_final_delivery(self):  # #6739
        ce = ROOT / "chaos-engine"
        homes = ("references/router-contract.md", "references/permanent-rules.md",
                 "references/bot-entry.md", "references/work-github-playbook.md",
                 "references/lifecycle-hooks.md", "references/hook-trigger-map.md",
                 "references/orchestrator-bootstrap.md", "skills/chaos-engine/SKILL.md",
                 "skills/self-improve/SKILL.md", "skills/self-improve/references/activation.md",
                 "addons/design-skills/references/pipelines/runbook.md")
        for home in homes:
            text = " ".join((ce / home).read_text(encoding="utf-8").split())
            with self.subTest(home=home):
                self.assertIn("Learning Session after every final delivery", text)
                self.assertNotIn("only when a trigger fired", text)
                self.assertNotIn("only on trigger", text)
        router = " ".join((ce / "references/router-contract.md").read_text(encoding="utf-8").split())
        for rule in ("the owner approves a deliverable", "a publish (release, deploy, upload) completes",
                     "the final pull request of a task merges", "without the owner asking",
                     "File one spec issue", "Open ONE pull request", "`skip-release-notes`",
                     "MERGE method", "babysit it to merged", "never starts another Learning Session"):
            with self.subTest(rule=rule):
                self.assertIn(rule, router)


if __name__ == "__main__":
    unittest.main()
