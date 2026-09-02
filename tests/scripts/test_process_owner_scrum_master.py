import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
REFERENCE = ROOT / "chaos-engine/references/process-owner-scrum-master.md"
OWNERS = (
    ROOT / "chaos-engine/skills/chaos-engine/SKILL.md",
    ROOT / "chaos-engine/references/roles.md",
    ROOT / "chaos-engine/references/orchestrator-follow-through.md",
    ROOT / "chaos-engine/references/execution-workflows.md",
)


class ProcessOwnerScrumMasterTest(unittest.TestCase):
    def test_reference_exists(self):
        self.assertTrue(REFERENCE.is_file(), REFERENCE)

    def test_owners_link_the_reference(self):
        for path in OWNERS:
            with self.subTest(path=path.relative_to(ROOT).as_posix()):
                text = path.read_text(encoding="utf-8")
                self.assertIn("process-owner-scrum-master.md", text)

    def test_must_invariants_are_present(self):
        text = REFERENCE.read_text(encoding="utf-8")
        compact = " ".join(text.split())
        required = (
            ("delegation verification", "verify delegation deliverables before parent-slice completion"),
            ("TDD/PDCA", "no Plan→Complete without red/green or automated verifier proof"),
            ("evidence status", "Evidence-backed status"),
            ("impediment removal", "Impediment removal"),
            (
                "recurring-only research threshold",
                "recurring process-failure / impediment classes (2+)",
            ),
        )
        for label, phrase in required:
            with self.subTest(label=label):
                self.assertIn(phrase, compact)

    def test_must_vs_adaptive_and_anti_patterns_exist(self):
        text = REFERENCE.read_text(encoding="utf-8")
        self.assertIn("MUST", text)
        self.assertRegex(text, r"(?i)adaptive")
        self.assertRegex(text, r"(?i)anti-pattern")


if __name__ == "__main__":
    unittest.main()
