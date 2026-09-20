"""CE-Brief unit/eval fixtures (#6072) — case_pass_rate must stay 1.0."""

from __future__ import annotations

import importlib.util
import json
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "chaos-engine/evals/ce-brief-unit-fixtures.json"


def load_ce_brief():
    path = ROOT / "chaos-engine/ce_brief.py"
    spec = importlib.util.spec_from_file_location("ce_brief_eval", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CeBriefEvalFixturesTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load_ce_brief()
        cls.document = json.loads(FIXTURES.read_text(encoding="utf-8"))

    def test_fixture_document_thresholds_not_weakened(self):
        self.assertEqual(self.document.get("schema_version"), 1)
        self.assertEqual(self.document.get("package"), "chaos-engine")
        self.assertEqual(self.document["thresholds"]["case_pass_rate"], 1.0)
        self.assertGreaterEqual(len(self.document["fixtures"]), 3)

    def test_all_ce_brief_fixtures_pass(self):
        failures: list[str] = []
        for fixture in self.document["fixtures"]:
            expect = fixture["expect"]
            fid = fixture["id"]
            if fid == "ce-brief-under-budget-locators":
                brief = self.mod.build_brief(project=ROOT, max_bytes=expect["max_bytes"])
                if brief["truncated"] != expect["truncated"]:
                    failures.append(f"{fid}: truncated={brief['truncated']}")
                for needle in expect["must_include"]:
                    if needle not in brief["text"]:
                        failures.append(f"{fid}: missing {needle}")
                blob = brief["text"].lower()
                for banned in expect["must_not_include"]:
                    if banned.lower() in blob:
                        failures.append(f"{fid}: banned {banned!r}")
                if brief["bytes"] > expect["max_bytes"]:
                    failures.append(f"{fid}: bytes {brief['bytes']} > {expect['max_bytes']}")
            elif fid == "ce-brief-utf8-truncate":
                brief = self.mod.build_brief(project=ROOT, max_bytes=expect["max_bytes"])
                if not brief["truncated"]:
                    failures.append(f"{fid}: expected truncated")
                if brief["bytes"] > expect["max_bytes"]:
                    failures.append(f"{fid}: bytes over cap")
            elif fid == "ce-brief-clamp-absolute-max":
                brief = self.mod.build_brief(project=ROOT, max_bytes=99999)
                if brief["bytes"] > expect["max_bytes_cap"]:
                    failures.append(f"{fid}: clamp failed {brief['bytes']}")
            else:
                failures.append(f"unknown fixture id {fid}")
        self.assertEqual(failures, [], msg="; ".join(failures))


if __name__ == "__main__":
    unittest.main()
