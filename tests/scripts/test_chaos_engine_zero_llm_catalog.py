"""Zero-LLM catalog + doctor --fix-next-only (#5582)."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CATALOG = ROOT / "chaos-engine/references/zero-llm-catalog.md"


def load_install():
    path = ROOT / "chaos-engine/install.py"
    spec = importlib.util.spec_from_file_location("ce_install_zero_llm", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class ZeroLlmCatalogTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.install = load_install()

    def test_catalog_lists_doctor_and_fix_next_only(self):
        text = CATALOG.read_text(encoding="utf-8")
        self.assertIn("--fix-next-only", text)
        self.assertIn("doctor --project", text)
        self.assertIn("zero-llm", text.casefold())

    def test_format_fix_next_only_emits_component_lines(self):
        document = {
            "components": {
                "core": {
                    "status": "recovery-required",
                    "taskImpact": "required",
                    "code": "CE_CORE_MISSING",
                },
                "memory": {"status": "healthy", "taskImpact": "advisory"},
            }
        }
        rendered = self.install.format_fix_next_only(document)
        self.assertIn("core:", rendered)
        self.assertNotIn("memory:", rendered)
        self.assertNotIn("ChaosEngine doctor:", rendered)

    def test_fix_next_only_rejects_json_combo(self):
        args = self.install.parser().parse_args(
            ["doctor", "--project", ".", "--json", "--fix-next-only"]
        )
        with self.assertRaises(ValueError):
            self.install.validate_install_options(args)


if __name__ == "__main__":
    unittest.main()
