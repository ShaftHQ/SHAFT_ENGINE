"""Unit tests for scripts/ci/skillspector_gate.py (SkillSpector static gate)."""

from __future__ import annotations

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
import json
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest import TestCase, main, mock

from scripts.ci.skillspector_gate import (
    evaluate_report,
    gate_failures,
    skill_dirs_from_paths,
)


class SkillDirsFromPathsTest(TestCase):
    def test_maps_nested_skill_paths(self) -> None:
        paths = [
            "chaos-engine/skills/omniroute/scripts/runner.py",
            "chaos-engine/addons/design-skills/SKILL.md",
            "README.md",
            "chaos-engine/skills/omniroute/SKILL.md",
        ]
        dirs = {p.as_posix() for p in skill_dirs_from_paths(paths)}
        self.assertEqual(
            dirs,
            {
                "chaos-engine/skills/omniroute",
                "chaos-engine/addons/design-skills",
            },
        )


class EvaluateReportTest(TestCase):
    def test_score_over_threshold_fails(self) -> None:
        report = {
            "skill": {"name": "demo"},
            "risk_assessment": {"score": 51, "severity": "HIGH"},
            "issues": [],
        }
        self.assertTrue(any("51" in f for f in evaluate_report(report)))

    def test_unsuppressed_high_fails_even_if_score_low(self) -> None:
        report = {
            "skill": {"name": "demo"},
            "risk_assessment": {"score": 10, "severity": "LOW"},
            "issues": [
                {
                    "id": "E2",
                    "severity": "HIGH",
                    "location": {"file": "scripts/x.py", "start_line": 1},
                }
            ],
        }
        failures = evaluate_report(report)
        self.assertTrue(any("HIGH E2" in f for f in failures))

    def test_clean_report_passes(self) -> None:
        report = {
            "skill": {"name": "demo"},
            "risk_assessment": {"score": 0, "severity": "LOW"},
            "issues": [],
        }
        self.assertEqual(evaluate_report(report), [])


class GateFailuresTest(TestCase):
    def test_missing_scanner_warns_unless_required(self) -> None:
        with TemporaryDirectory() as tmp:
            root = Path(tmp)
            pin = root / "scripts/ci/skillspector"
            pin.mkdir(parents=True)
            (pin / "PIN.json").write_text(
                json.dumps({"commit": "abc", "version": "2.12.0"}), encoding="utf-8"
            )
            (pin / ".skillspector-baseline.yaml").write_text("version: 2\n", encoding="utf-8")
            skill = root / "chaos-engine/skills/demo"
            skill.mkdir(parents=True)
            (skill / "SKILL.md").write_text("---\nname: demo\n---\n", encoding="utf-8")
            with mock.patch(
                "scripts.ci.skillspector_gate.resolve_skillspector", return_value=None
            ):
                self.assertEqual(
                    gate_failures(root, paths=["chaos-engine/skills/demo/SKILL.md"], required=False),
                    [],
                )
                required = gate_failures(
                    root, paths=["chaos-engine/skills/demo/SKILL.md"], required=True
                )
                self.assertTrue(required and "not installed" in required[0])


if __name__ == "__main__":
    main()
