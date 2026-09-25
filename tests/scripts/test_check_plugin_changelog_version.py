"""#6222: the plugin CHANGELOG bump is enforced whenever the engine version moves."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

import yaml

from scripts.ci.check_plugin_changelog_version import defects, engine_version, main

ROOT = Path(__file__).resolve().parents[2]
POM = """<project xmlns="http://maven.apache.org/POM/4.0.0"><version>{}</version></project>\n"""


class PluginChangelogVersionTest(unittest.TestCase):
    def fixture(self, version: str, changelog: str, compatibility: str) -> Path:
        root = Path(self.enterContext(tempfile.TemporaryDirectory()))
        (root / "pom.xml").write_text(POM.format(version), encoding="utf-8")
        plugin = root / "agent-plugins/chaos-engine"
        plugin.mkdir(parents=True)
        (plugin / "CHANGELOG.md").write_text(changelog, encoding="utf-8")
        (plugin / "COMPATIBILITY.md").write_text(compatibility, encoding="utf-8")
        return root

    def test_covered_version_passes(self) -> None:
        root = self.fixture("1.2.3", "# Changelog\n\n## 1.2.3 - 2026-09-25\n\n- x\n", "Evidence for 1.2.3.\n")
        self.assertEqual([], defects(root))
        self.assertEqual(0, main(["--root", str(root)]))

    def test_missing_changelog_entry_or_compatibility_fails(self) -> None:
        root = self.fixture("1.2.4", "# Changelog\n\n## 1.2.3 - 2026-09-25\n", "Evidence for 1.2.3.\n")
        problems = defects(root)
        self.assertEqual(2, len(problems))
        self.assertIn("## 1.2.4", problems[0])
        self.assertEqual(1, main(["--root", str(root)]))

    def test_prefix_versions_do_not_count(self) -> None:
        root = self.fixture("1.2.3", "## 1.2.30 - 2026-09-25\n", "1.2.3\n")
        self.assertTrue(defects(root))

    def test_repository_reads_the_root_pom_version(self) -> None:
        self.assertRegex(engine_version(ROOT), r"^\d+\.\d+\.\d+")

    def test_pr_gate_runs_the_check_when_the_version_or_changelog_moves(self) -> None:
        workflow = yaml.safe_load((ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8"))
        job = workflow["jobs"]["workflow-timeouts"]
        commands = "\n".join(str(step.get("run", "")) for step in job["steps"])
        self.assertIn("python3 scripts/ci/check_plugin_changelog_version.py", commands)
        self.assertIn("tests.scripts.test_check_plugin_changelog_version", commands)
        self.assertIn("needs.changes.outputs.dependencies == 'true'", job["if"])
        filters = yaml.safe_load(
            next(step["with"]["filters"] for step in workflow["jobs"]["changes"]["steps"] if step.get("id") == "filter")
        )
        self.assertIn("**/pom.xml", filters["dependencies"])
        for path in (
            "agent-plugins/chaos-engine/CHANGELOG.md",
            "agent-plugins/chaos-engine/COMPATIBILITY.md",
            "scripts/ci/check_plugin_changelog_version.py",
            "tests/scripts/test_check_plugin_changelog_version.py",
        ):
            self.assertIn(path, filters["workflows"])


if __name__ == "__main__":
    unittest.main()
