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
        self.assertRegex(value, r"^\d{4}\.\d{2}\.\d{2}(\.\d+)?$")
        self.assertEqual(f"CE upgrade {value}", self.mod.upgrade_broadcast())

    def test_invalid_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            bad = Path(tmp) / "VERSION"
            bad.write_text("not-a-version\n", encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.read_version(bad)

    def test_check_cli_pass(self) -> None:
        self.assertEqual(0, self.mod.main(["check"]))


if __name__ == "__main__":
    unittest.main()
