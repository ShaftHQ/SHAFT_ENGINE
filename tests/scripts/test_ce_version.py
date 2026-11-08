"""ChaosEngine VERSION + upgrade broadcast string."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MOD = ROOT / "scripts/ci/ce_version.py"


def _load():
    spec = importlib.util.spec_from_file_location("ce_version", MOD)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod


class CeVersionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.mod = _load()

    def test_repo_version_readable(self) -> None:
        value = self.mod.read_version()
        self.assertRegex(value, r"^\d+\.[1-4]\.\d{8}\.[1-9]\d*$")
        self.assertEqual(f"CE upgrade {value}", self.mod.upgrade_broadcast())

    def test_scheme_keys_and_next(self) -> None:
        key = self.mod.version_key
        self.assertGreater(key("0.4.20261102.1"), key("2026.10.31.13"))
        self.assertGreater(key("0.4.20261102.2"), key("0.4.20261102.1"))
        self.assertEqual("0.4.20261102.1", self.mod.next_version("2026.10.31.13", "2026-11-02"))
        self.assertEqual("0.4.20261102.2", self.mod.next_version("0.4.20261102.1", "2026-11-02"))
        self.assertEqual("0.4.20261103.1", self.mod.next_version("0.4.20261102.5", "2026-11-03"))
        with self.assertRaises(ValueError):
            self.mod.next_version("0.4.20261102.1", "2026-11-01")


    def test_quarter_rollover_orders_by_date(self) -> None:
        """Q4→Q1 and Q1→Q2 compare by date, not by the quarter digit."""
        key = self.mod.version_key
        # Dec 31 Q4 precedes Jan 1 Q1 of the next calendar year
        self.assertLess(key("0.4.20261231.1"), key("0.1.20270101.1"))
        self.assertLess(key("0.4.20261231.9"), key("0.1.20270101.1"))
        # Mar 31 Q1 precedes Apr 1 Q2 of the same year
        self.assertLess(key("0.1.20260331.1"), key("0.2.20260401.1"))
        self.assertEqual("0.1.20270101.1", self.mod.next_version("0.4.20261231.3", "2027-01-01"))
        self.assertEqual("0.2.20260401.1", self.mod.next_version("0.1.20260331.2", "2026-04-01"))
        with self.assertRaises(ValueError):
            key("0.1.20261231.1")  # declared Q1 but December is Q4

    def test_invalid_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            bad = Path(tmp) / "VERSION"
            bad.write_text("not-a-version\n", encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.read_version(bad)

    def test_check_cli_pass(self) -> None:
        self.assertEqual(0, self.mod.main(["check"]))

    def test_concurrent_bump_rule_names_next_version(self) -> None:
        doc = (ROOT / "chaos-engine/references/ce-upgrade-on-release.md").read_text(encoding="utf-8")
        self.assertIn("Concurrent bumps", doc)
        self.assertIn("merges second", doc)
        self.assertIn("next_version", doc)


if __name__ == "__main__":
    unittest.main()
