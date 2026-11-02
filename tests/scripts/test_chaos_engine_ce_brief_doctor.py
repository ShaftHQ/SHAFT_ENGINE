"""Doctor ceBrief surface (#6071)."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def load(name: str, relative: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CeBriefDoctorTest(unittest.TestCase):
    def test_doctor_ce_brief_healthy_on_source_tree(self):
        mod = load("ce_brief_doctor", "chaos-engine/ce_brief.py")
        payload = mod.doctor_ce_brief(ROOT)
        self.assertEqual(payload["schemaVersion"], 1)
        self.assertEqual(payload["status"], "healthy")
        self.assertTrue(payload["dispatchBrief"])
        self.assertGreaterEqual(payload["locatorCount"], 3)
        self.assertIn("sampleBytes", payload)
        # no secrets
        blob = str(payload).lower()
        for banned in ("token", "password", "api_key", "bearer"):
            self.assertNotIn(banned, blob)

    def test_catalogs_document_ce_brief_and_dispatch(self):
        level1 = (ROOT / "chaos-engine/references/level-1-catalog.md").read_text(encoding="utf-8")
        zero = (ROOT / "chaos-engine/references/zero-llm-catalog.md").read_text(encoding="utf-8")
        for text in (level1, zero):
            self.assertIn("ce_brief.py", text)
            self.assertIn("Dispatch CE brief", text)
            self.assertIn("dispatch.py", text)


if __name__ == "__main__":
    unittest.main()
