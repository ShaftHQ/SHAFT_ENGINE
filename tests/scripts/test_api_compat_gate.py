"""Tests for scripts/ci/api_compat_gate.py (#7002)."""
from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("api_compat_gate", ROOT / "scripts/ci/api_compat_gate.py")
gate = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(gate)

REPORT = """<japicmp><classes>
<class fullyQualifiedName="com.shaft.A" binaryCompatible="false">
  <compatibilityChanges><compatibilityChange type="METHOD_REMOVED_IN_SUPERCLASS" binaryCompatible="false"/></compatibilityChanges>
</class>
<class fullyQualifiedName="com.shaft.B" binaryCompatible="true"/>
</classes></japicmp>"""


class ApiCompatGateTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        target = Path(self.tmp.name, "shaft-ai", "target", "japicmp")
        target.mkdir(parents=True)
        (target / "default-cli.xml").write_text(REPORT, encoding="utf-8")

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def test_lists_only_incompatible_classes(self) -> None:
        found = gate.collect(Path(self.tmp.name), ["shaft-ai"])
        self.assertEqual({"shaft-ai": ["com.shaft.A: METHOD_REMOVED_IN_SUPERCLASS"]}, found)

    def test_break_blocks_without_label_and_passes_with_it(self) -> None:
        args = ["--root", self.tmp.name, "--modules", "shaft-ai"]
        self.assertEqual(1, gate.main([*args, "--labels", "enhancement"]))
        self.assertEqual(0, gate.main([*args, "--labels", "enhancement,breaking-change"]))

    def test_leg_runs_on_pull_request_in_summary(self) -> None:
        jobs = yaml.safe_load((ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))["jobs"]
        self.assertIn("api-compat", jobs["summary"]["needs"])
        self.assertIn("pull_request", jobs["api-compat"]["if"])
        self.assertIn("<id>japicmp</id>", (ROOT / "pom.xml").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
