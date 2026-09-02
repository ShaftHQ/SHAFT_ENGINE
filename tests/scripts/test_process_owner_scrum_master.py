import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
REFERENCE = ROOT / "chaos-engine/references/process-owner-scrum-master.md"
OWNERS = (
    ROOT / "chaos-engine/skills/chaos-engine/SKILL.md",
    ROOT / "chaos-engine/references/roles.md",
    ROOT / "chaos-engine/references/orchestrator-follow-through.md",
    ROOT / "chaos-engine/references/execution-workflows.md",
    ROOT / ".claude/agents/chaos-engine-orchestrator.md",
    ROOT / ".codex/agents/chaos-engine-orchestrator.toml",
)

# Long MUST / anti-pattern body that owners must link, not restate.
OWNER_FORBIDDEN_RESTATEMENTS = (
    "Silent acceptance of a delegate narrative is forbidden",
    "Narrative claims of testing do not satisfy the gate",
    "MUST publish Evidence-backed status using artifacts, exit codes, logs, or",
    "Online research to optimize orchestrator or Scrum practice is allowed only for",
    "Silent delegation drop: parent slice marked done from delegate prose alone",
    "Narrative TDD bypass: Plan→Complete with \"tests should pass\"",
)

ANTI_PATTERNS = (
    (
        "Silent delegation drop",
        "Re-open the slice; demand artifacts and exit evidence; verify before completion",
    ),
    (
        "Narrative TDD bypass",
        "Halt completion; require RED/GREEN or automated verifier proof, or record the blocker",
    ),
    (
        "Heartbeat status",
        "Replace with evidence-backed status or mark blocked",
    ),
    (
        "Impediment theater",
        "Remove within authority, coach, or escalate owner-only / paid spend immediately",
    ),
    (
        "Research every Learning Session",
        "Skip unless the failure class has recurred (2+); otherwise capture the single learning and move on",
    ),
    (
        "Orchestrator implements while writers are live",
        "Stop self-work; restore orchestrated boundaries; re-dispatch or switch mode only after handover",
    ),
)


class ProcessOwnerScrumMasterTest(unittest.TestCase):
    def test_reference_exists(self):
        self.assertTrue(REFERENCE.is_file(), REFERENCE)

    def test_owners_link_the_reference(self):
        for path in OWNERS:
            with self.subTest(path=path.relative_to(ROOT).as_posix()):
                text = path.read_text(encoding="utf-8")
                self.assertIn("process-owner-scrum-master.md", text)

    def test_owners_do_not_restate_must_invariants(self):
        for path in OWNERS:
            text = path.read_text(encoding="utf-8")
            compact = " ".join(text.split())
            for phrase in OWNER_FORBIDDEN_RESTATEMENTS:
                with self.subTest(path=path.relative_to(ROOT).as_posix(), phrase=phrase):
                    self.assertNotIn(phrase, compact)

    def test_must_invariants_are_present(self):
        text = REFERENCE.read_text(encoding="utf-8")
        compact = " ".join(text.split())
        required = (
            ("delegation verification", "verify delegation deliverables before parent-slice completion"),
            ("TDD/PDCA", "no Plan→Complete without red/green or automated verifier proof"),
            ("evidence status", "Evidence-backed status"),
            ("impediment removal", "Impediment removal"),
            ("consult-on-ambiguity", "consult on ambiguity"),
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
        self.assertIn("## Anti-patterns and self-correction", text)
        for anti_pattern, correction in ANTI_PATTERNS:
            with self.subTest(anti_pattern=anti_pattern):
                self.assertIn(anti_pattern, text)
                self.assertIn(correction, text)


if __name__ == "__main__":
    unittest.main()
