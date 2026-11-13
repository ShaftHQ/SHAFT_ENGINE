"""job-apply on-demand skill: reachability, abstraction, research + gate cards."""

from __future__ import annotations

import shutil
import subprocess
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import harness_reachability as reach  # noqa: E402

BASE = "chaos-engine/skills/job-apply"
SKILL = f"{BASE}/SKILL.md"
CARDS = [f"{BASE}/references/{name}.md" for name in ("research", "template", "gate", "lessons")]
CHECK = ROOT / BASE / "scripts" / "check_abstraction.sh"
ROUTER = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"


def _text(path: str) -> str:
    return (ROOT / path).read_text(encoding="utf-8")


class JobApplySkillTest(unittest.TestCase):
    def test_every_host_reaches_the_skill(self) -> None:
        contexts = {
            "agents": "AGENTS.md",
            "grok-bot": "chaos-engine/references/bot-entry.md",
        }
        for host, context in contexts.items():
            with self.subTest(host=host):
                self.assertIn(SKILL, reach.link_walk(ROOT, context)[0])

    def test_cards_reachable_from_skill(self) -> None:
        reached = reach.link_walk(ROOT, SKILL)[0]
        for card in CARDS:
            with self.subTest(card=card):
                self.assertIn(card, reached)

    def test_one_source_no_host_copies(self) -> None:
        copies = [
            p
            for p in ROOT.glob("**/job-apply/SKILL.md")
            if ".chaos-engine" not in p.parts and ".git" not in p.parts
        ]
        self.assertEqual([ROOT / SKILL], copies)

    def test_on_demand_only_never_in_the_always_loaded_router(self) -> None:
        self.assertNotIn("job-apply", ROUTER.read_text(encoding="utf-8"))
        self.assertIn("../skills/job-apply/SKILL.md", _text("chaos-engine/references/level-1-catalog.md"))

    def test_skill_opens_with_both_retrieves(self) -> None:
        body = _text(SKILL)
        self.assertIn("--store graphify", body)
        self.assertIn("--store mempalace", body)

    def test_process_research_template_gate_lessons(self) -> None:
        skill = _text(SKILL).casefold()
        self.assertIn("## process", skill)
        self.assertIn("private per-user store", skill)
        self.assertIn("application ledger", skill)
        self.assertIn("triage fit", skill)
        research = _text(f"{BASE}/references/research.md")
        for marker in ("Harvard", "Indeed", "Fonzi", "Cornell", "LHH", "G2"):
            with self.subTest(marker=marker):
                self.assertIn(marker, research)
        template = _text(f"{BASE}/references/template.md")
        self.assertIn("Subject: Application:", template)
        self.assertIn("looking for a remote role", template.casefold())
        gate = _text(f"{BASE}/references/gate.md").casefold()
        self.assertIn("exactly one", gate)
        self.assertIn("all 10 research criteria", gate)
        self.assertIn("fit triaged", gate)
        self.assertIn("application ledger", gate)
        self.assertIn("private per-user store", gate)
        lessons = _text(f"{BASE}/references/lessons.md").casefold()
        self.assertIn("short links", lessons)
        self.assertIn("one multi-step card", lessons)
        self.assertIn("private per-user store", lessons)

    def test_abstraction_script_passes(self) -> None:
        bash = shutil.which("bash")
        self.assertIsNotNone(bash, "bash must be on PATH")
        completed = subprocess.run(  # nosec B603 - absolute bash + fixed script path
            [bash, str(CHECK)],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
        self.assertIn("check_abstraction: ok", completed.stdout)

    def test_send_authority_agrees_across_both_skills(self) -> None:
        rule = (
            "default is draft only. a recorded owner standing send permission "
            "that names the channel class may send only after the shared gate "
            "passes on the exact final text."
        )
        surfaces = {
            "job-apply": _text(SKILL).casefold(),
            "gate": _text(f"{BASE}/references/gate.md").casefold(),
            "job-applications": _text("chaos-engine/skills/job-applications/SKILL.md").casefold(),
            "index": _text("chaos-engine/harness_index.py").casefold(),
            "reference": _text("chaos-engine/references/outbound-send.md").casefold(),
        }
        for name, text in surfaces.items():
            with self.subTest(surface=name):
                self.assertIn(rule, text)
        for name in ("job-apply", "gate", "job-applications"):
            text = surfaces[name]
            self.assertNotIn("does not authorize the agent to send", text)
            self.assertNotIn("does not create an exception", text)
            self.assertNotIn("does not authorize send", text)
        self.assertIn(
            "submit only after the send rule allows that form submit",
            surfaces["job-applications"],
        )
        self.assertNotIn("then submit and capture the confirmation page", surfaces["job-applications"])
        self.assertIn("or the owner approved this exact send", surfaces["reference"])
        lessons = _text(f"{BASE}/references/lessons.md").casefold()
        self.assertNotIn("must not send email", lessons)
        self.assertNotIn("the human sends", lessons)
        self.assertNotIn("from a human send", surfaces["gate"])
        self.assertIn("unless the standing permission names this form channel", lessons)
        self.assertIn("unless the standing permission names this form channel", surfaces["gate"])
        self.assertIn("unless the standing permission names this form channel", surfaces["job-apply"])
        reference = _text("chaos-engine/references/outbound-send.md")
        self.assertLess(len(reference.splitlines()), 80)
        routes = _text("chaos-engine/skills/chaos-engine/SKILL.md")
        start = routes.index("<!-- HARNESS-ROUTES:START -->")
        end = routes.index("<!-- HARNESS-ROUTES:END -->")
        self.assertNotIn("outbound-send", routes[start:end])
        self.assertNotIn("outbound-send", _text("chaos-engine/always-loaded-budget.json"))
        self.assertFalse((ROOT / "chaos-engine/skills/outbound-send").exists())

    def test_skill_is_fully_abstracted(self) -> None:
        paths = [ROOT / SKILL, *[ROOT / c for c in CARDS]]
        banned = (
            "mohab",
            "mohie",
            "automatest",
            "linkedin.com/in/",
            *private_terms(),
        )
        for path in paths:
            text = path.read_text(encoding="utf-8")
            lower = text.casefold()
            with self.subTest(path=str(path.relative_to(ROOT))):
                for name in banned:
                    self.assertNotIn(name.casefold(), lower)
                self.assertNotRegex(text, r"(?<![<\w])[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b")
                self.assertNotRegex(text, r"(?:\+|00)?\d[\d().\s-]{7,}\d")


if __name__ == "__main__":
    unittest.main()
