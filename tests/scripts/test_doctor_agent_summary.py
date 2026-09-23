"""Agent-facing doctor summary stays bounded; --json stays the full document."""

from __future__ import annotations

import importlib.util
import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    spec.loader.exec_module(module)
    return module


INSTALL = load("chaos_engine_install_agent_summary", ROOT / "chaos-engine/install.py")


def _document(**overrides):
    document = {
        "status": "healthy",
        "policySha256": "ab" * 32,
        "commit": "c" * 40,
        "components": {"core": {"status": "healthy"}},
    }
    document.update(overrides)
    return document


class DoctorAgentSummaryTest(unittest.TestCase):
    def test_agent_summary_is_four_lines_and_passes_when_healthy(self):
        rendered = INSTALL.format_agent_summary(_document())
        self.assertLessEqual(len(rendered.splitlines()), INSTALL.AGENT_SUMMARY_MAX_LINES)
        self.assertEqual(0, INSTALL.agent_summary_exit_code(_document()))
        self.assertIn("doctor: pass", rendered)
        self.assertIn("component: core healthy", rendered)
        self.assertIn("hash: " + "ab" * 32, rendered)
        self.assertIn("drift: none", rendered)

    def test_policy_hash_drift_fails_closed(self):
        drifted = _document(
            status="recovery-required",
            components={
                "core": {
                    "status": "recovery-required",
                    "detail": "overlay-commit-mismatch",
                    "coreMatchesSource": False,
                }
            },
        )
        rendered = INSTALL.format_agent_summary(drifted)
        self.assertLessEqual(len(rendered.splitlines()), INSTALL.AGENT_SUMMARY_MAX_LINES)
        self.assertIn("drift: overlay-commit-mismatch", rendered)
        self.assertEqual(1, INSTALL.agent_summary_exit_code(drifted))
        placeholder = _document(policySha256="0" * 64)
        self.assertEqual(1, INSTALL.agent_summary_exit_code(placeholder))
        self.assertIn("drift: policy-hash-drift", INSTALL.format_agent_summary(placeholder))

    def test_json_flag_still_serializes_the_full_document(self):
        document = _document(clients={"grok": {"status": "healthy"}}, phaseLedger={"status": "absent"})
        rendered = json.dumps(document, sort_keys=True, separators=(",", ":"))
        parsed = json.loads(rendered)
        self.assertEqual(parsed, document)
        self.assertGreater(rendered.count(","), INSTALL.format_agent_summary(document).count(","))
        parser = INSTALL.parser()
        args = parser.parse_args(["doctor", "--project", ".", "--json"])
        self.assertTrue(args.json)
        self.assertFalse(args.agent_summary)
        summary = parser.parse_args(["doctor", "--project", ".", "--agent-summary"])
        self.assertTrue(summary.agent_summary)
        self.assertFalse(summary.json)
        with self.assertRaises(ValueError):
            INSTALL.validate_install_options(
                parser.parse_args(["doctor", "--project", ".", "--json", "--agent-summary"])
            )

    def test_learning_session_non_skip_sentence_stays_reachable(self):
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("Unchanged ChaosEngine sources are not a valid skip.", skill)
        self.assertIn("`chaos-engine/` files were untouched", skill)
        laws = skill.split("## Iron laws", 1)[1].split("## Triage", 1)[0]
        for number in range(1, 8):
            self.assertIn(f"{number}. ", laws)


if __name__ == "__main__":
    unittest.main()
