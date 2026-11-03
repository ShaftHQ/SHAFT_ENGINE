"""Full pin coverage for the upstream drift watch (#7738)."""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

import sys as _sys; from pathlib import Path as _Path; _sys.path.insert(0, str(_Path(__file__).resolve().parents[2]))  # noqa: E402,E702 - self-locating under python3 -I (#7197)
from scripts.ci.watch_upstream_pins import (  # noqa: E402
    COVERAGE_EXEMPT,
    Pin,
    Stable,
    compare_pin,
    load_pins,
    newest_maven_version,
    newest_python_version,
    repository_runtime_pins,
    require_full_coverage,
)

ROOT = Path(__file__).resolve().parents[2]


class FullCoverageTest(unittest.TestCase):
    def test_repository_spec_is_fully_covered(self):
        pins = load_pins(ROOT)
        names = {pin.name for pin in pins}
        for required in ("java-feature", "python", "maven", "deja", "node", "uv"):
            self.assertIn(required, names)

    def test_new_unwatched_pin_fails_loudly(self):
        spec = {"dependencies": {"brand-new-tool": {}}, "runtimes": {}}
        with self.assertRaisesRegex(ValueError, "dependencies.brand-new-tool"):
            require_full_coverage(spec, [])

    def test_every_exemption_names_a_reason(self):
        for key, reason in COVERAGE_EXEMPT.items():
            self.assertTrue(reason.strip(), key)

    def test_workflow_and_pom_runtime_literals_are_watched(self):
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            workflows = root / ".github" / "workflows"
            workflows.mkdir(parents=True)
            (workflows / "a.yml").write_text(
                "      node-version: '20'\n      java-version: 17\n"
                "      python-version: ${{ matrix.python }}\n      node-version: lts/*\n",
                encoding="utf-8",
            )
            (root / "pom.xml").write_text("<jdk.version>25</jdk.version>", encoding="utf-8")
            pins = {pin.name: pin for pin in repository_runtime_pins(root)}
        self.assertEqual(
            sorted(pins),
            ["ci-java@17 (.github/workflows/a.yml)", "ci-java@25 (pom.xml)", "ci-node@20 (.github/workflows/a.yml)"],
        )

    def test_series_pin_is_behind_only_on_a_newer_series(self):
        pin = Pin("java-feature", "series-java", "25", "x")
        self.assertTrue(compare_pin(pin, Stable(version="27")).behind)
        self.assertFalse(compare_pin(pin, Stable(version="25")).behind)
        python = Pin("python", "series-python", "3.14", "x")
        self.assertFalse(compare_pin(python, Stable(version="3.14.2")).behind)
        self.assertTrue(compare_pin(python, Stable(version="3.15.0")).behind)
        node = Pin("ci-node@24", "series-node", "24", "x")
        self.assertTrue(compare_pin(node, Stable(version="26.13.0")).behind)

    def test_python_and_maven_skip_prereleases(self):
        payload = [
            {"name": "Python 3.15.0", "pre_release": False, "is_published": True},
            {"name": "Python 3.16.0a1", "pre_release": True, "is_published": True},
            {"name": "Python 2.7.18", "pre_release": False, "is_published": True},
        ]
        self.assertEqual(newest_python_version(payload), "3.15.0")
        xml = "<versions><version>3.9.12</version><version>4.0.0-rc-5</version><version>3.10.0</version></versions>"
        self.assertEqual(newest_maven_version(xml), "3.10.0")


if __name__ == "__main__":
    unittest.main()
