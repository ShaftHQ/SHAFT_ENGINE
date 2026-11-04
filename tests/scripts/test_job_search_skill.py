"""job-search on-demand skill: reachability, abstraction, channels + gates."""

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

BASE = "chaos-engine/skills/job-search"
SKILL = f"{BASE}/SKILL.md"
CARDS = [
    f"{BASE}/references/{name}.md"
    for name in (
        "channels",
        "browser",
        "approval-gate",
        "tracker",
        "gate",
        "lessons",
        "reflection",
        "RETRO-job-search",
    )
]
CHECK = ROOT / BASE / "scripts" / "check_abstraction.sh"
ROUTER = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"


def _text(path: str) -> str:
    return (ROOT / path).read_text(encoding="utf-8")


class JobSearchSkillTest(unittest.TestCase):
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
            for p in ROOT.glob("**/job-search/SKILL.md")
            if ".chaos-engine" not in p.parts and ".git" not in p.parts
        ]
        self.assertEqual([ROOT / SKILL], copies)

    def test_on_demand_only_never_in_the_always_loaded_router(self) -> None:
        self.assertNotIn("job-search", ROUTER.read_text(encoding="utf-8"))
        self.assertIn("../skills/job-search/SKILL.md", _text("chaos-engine/references/level-1-catalog.md"))

    def test_skill_opens_with_both_retrieves(self) -> None:
        body = _text(SKILL)
        self.assertIn("--store graphify", body)
        self.assertIn("--store mempalace", body)

    def test_process_channels_browser_approval_reflection(self) -> None:
        skill = _text(SKILL).casefold()
        self.assertIn("## process", skill)
        self.assertIn("two-channel", skill)
        self.assertIn("private per-user store", skill)
        self.assertIn("end", skill)
        self.assertIn("share-profile", skill)
        channels = _text(f"{BASE}/references/channels.md").casefold()
        self.assertIn("guest", channels)
        self.assertIn("activity", channels)
        browser = _text(f"{BASE}/references/browser.md").casefold()
        self.assertIn("page down", browser)
        self.assertIn("mouse-wheel", browser)
        approval = _text(f"{BASE}/references/approval-gate.md").casefold()
        self.assertIn("explicit user approval", approval)
        self.assertIn("irreversible", approval)
        gate = _text(f"{BASE}/references/gate.md").casefold()
        self.assertIn("share-profile", gate)
        self.assertIn("end / page down", gate)
        lessons = _text(f"{BASE}/references/lessons.md").casefold()
        self.assertIn("end / page down", lessons)
        reflection = _text(f"{BASE}/references/reflection.md").casefold()
        self.assertIn("full, detailed markdown", reflection)
        retro = _text(f"{BASE}/references/RETRO-job-search.md")
        self.assertIn("Suggested ChaosEngine changes", retro)
        harness = _text("chaos-engine/references/reflection-checkpoints.md").casefold()
        self.assertIn("full detailed markdown lessons", harness)

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
                self.assertNotRegex(
                    text, r"(?<![<\w])[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b"
                )


if __name__ == "__main__":
    unittest.main()
