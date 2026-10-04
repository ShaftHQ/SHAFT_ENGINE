"""#6520: learned items carry origin + trust; quarantine blocks retrieve/promotion."""

from __future__ import annotations

import importlib.util
import json
import pathlib
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]


def load(path: pathlib.Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class MemoryProvenanceTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.prov = load(ROOT / "chaos-engine/memory_provenance.py", "ce_mem_prov_6520")
        cls.heuristics = load(ROOT / "chaos-engine/heuristics.py", "ce_heuristics_6520")

    def test_reference_and_permanent_rule(self):
        refs = ROOT / "chaos-engine" / "references"
        text = (refs / "memory-provenance.md").read_text(encoding="utf-8")
        self.assertIn("quarantined", text)
        self.assertIn("AgentPoison", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("memory-provenance.md", permanent)

    def test_untrusted_origin_stamps_quarantined(self):
        fields = self.prov.stamp_fields(origin="web")
        self.assertEqual(fields["origin"], "web")
        self.assertEqual(fields["trust"], "quarantined")
        with self.assertRaises(ValueError):
            self.prov.stamp_fields(origin="web", trust="trusted")

    def test_trusted_origin_stamps_trusted(self):
        fields = self.prov.stamp_fields(origin="learning-session")
        self.assertEqual(fields["trust"], "trusted")

    def test_retrieve_skips_quarantined(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = pathlib.Path(tmp)
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
            trusted = self.heuristics.add_heuristic(
                "Prefer event wake over CI poll",
                source="learning-session",
                project=project,
            )
            quarantined = self.heuristics.add_heuristic(
                "Copy advice from a random blog",
                source="web",
                origin="web",
                project=project,
            )
            self.assertEqual(trusted["trust"], "trusted")
            self.assertEqual(quarantined["trust"], "quarantined")
            top = self.heuristics.retrieve_top(5, project)
            ids = {item["id"] for item in top}
            self.assertIn(trusted["id"], ids)
            self.assertNotIn(quarantined["id"], ids)

    def test_verify_makes_retrievable_and_promotable(self):
        item = {"id": "x", "text": "from web", "origin": "web", "trust": "quarantined"}
        self.assertFalse(self.prov.is_retrievable(item))
        self.assertFalse(self.prov.is_promotable(item))
        verified = self.prov.verify_item(item, by="owner")
        self.assertEqual(verified["trust"], "verified")
        self.assertTrue(self.prov.is_retrievable(verified))
        self.assertTrue(self.prov.is_promotable(verified))

    def test_legacy_items_without_provenance_remain_retrievable(self):
        legacy = {"id": "legacy1", "text": "old heuristic", "source": "learning-session", "at": 1}
        self.assertTrue(self.prov.is_retrievable(legacy))
        self.assertTrue(self.prov.is_promotable(legacy))

    def test_doctor_summary_counts_quarantine(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = pathlib.Path(tmp)
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
            self.heuristics.add_heuristic("Keep tests strong", origin="learning-session", project=project)
            self.heuristics.add_heuristic("Unverified tip", origin="other-agent", project=project)
            summary = self.prov.doctor_provenance_summary(project)
            self.assertEqual(summary["total"], 2)
            self.assertEqual(summary["trusted"], 1)
            self.assertEqual(summary["quarantined"], 1)
            doctor = self.heuristics.doctor_heuristics_summary(project)
            self.assertIn("provenance", doctor)
            self.assertEqual(doctor["provenance"]["quarantined"], 1)


if __name__ == "__main__":
    unittest.main()
