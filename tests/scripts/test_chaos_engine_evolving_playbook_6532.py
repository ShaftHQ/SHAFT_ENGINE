"""#6532: evolving playbook helpful/harmful counters, deltas, entry-stamp verify."""

from __future__ import annotations

import importlib.util
import pathlib
import tempfile
import time
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]


def load(path: pathlib.Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load module from {path}")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class EvolvingPlaybookTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.heuristics = load(ROOT / "chaos-engine/heuristics.py", "ce_playbook_6532")
        cls.install = load(ROOT / "chaos-engine/install.py", "ce_install_6532")

    def _project(self, tmp: str) -> pathlib.Path:
        project = pathlib.Path(tmp)
        (project / ".chaos-engine").mkdir()
        (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
        return project

    def test_reference_and_permanent_rule(self):
        refs = ROOT / "chaos-engine" / "references"
        text = (refs / "evolving-playbook.md").read_text(encoding="utf-8")
        self.assertIn("helpful", text)
        self.assertIn("harmful", text)
        self.assertIn("delta", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("evolving-playbook.md", permanent)

    def test_new_items_start_at_zero_counters(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            item = self.heuristics.add_heuristic(
                "Prefer event wake over CI poll",
                project=project,
            )
            self.assertEqual(item["helpful"], 0)
            self.assertEqual(item["harmful"], 0)

    def test_feedback_increments_and_ranks(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            low = self.heuristics.add_heuristic("Low score tip", project=project)
            time.sleep(0.01)
            high = self.heuristics.add_heuristic("High score tip", project=project)
            self.heuristics.record_feedback(high["id"], "helpful", project=project)
            self.heuristics.record_feedback(high["id"], "helpful", project=project)
            self.heuristics.record_feedback(low["id"], "harmful", project=project)
            top = self.heuristics.retrieve_top(2, project)
            self.assertEqual(top[0]["id"], high["id"])
            self.assertEqual(top[0]["helpful"], 2)
            self.assertEqual(top[1]["id"], low["id"])
            self.assertEqual(top[1]["harmful"], 1)

    def test_delta_keeps_id_and_counters(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            item = self.heuristics.add_heuristic("Original tip text", project=project)
            self.heuristics.record_feedback(item["id"], "helpful", project=project)
            updated = self.heuristics.apply_delta(
                item["id"],
                text="Revised tip text after outcome",
                project=project,
            )
            self.assertEqual(updated["id"], item["id"])
            self.assertEqual(updated["text"], "Revised tip text after outcome")
            self.assertEqual(updated["helpful"], 1)
            self.assertEqual(updated["harmful"], 0)
            self.assertEqual(updated.get("trust"), item.get("trust"))

    def test_legacy_missing_counters_rank_as_zero(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            store = project / ".chaos-engine-state" / "heuristics"
            store.mkdir(parents=True)
            legacy = {
                "schemaVersion": 1,
                "updatedAt": 1,
                "items": [
                    {
                        "id": "legacy1",
                        "text": "Legacy tip without counters",
                        "source": "learning-session",
                        "at": 10,
                        "origin": "learning-session",
                        "trust": "trusted",
                    }
                ],
            }
            import json

            (store / "index.json").write_text(json.dumps(legacy), encoding="utf-8")
            top = self.heuristics.retrieve_top(1, project)
            self.assertEqual(top[0]["id"], "legacy1")
            bumped = self.heuristics.record_feedback("legacy1", "helpful", project=project)
            self.assertEqual(bumped["helpful"], 1)
            self.assertEqual(bumped["harmful"], 0)

    def test_quarantined_skipped_despite_helpful(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            trusted = self.heuristics.add_heuristic(
                "Trusted tip",
                origin="learning-session",
                project=project,
            )
            quarantined = self.heuristics.add_heuristic(
                "Untrusted tip",
                origin="web",
                project=project,
            )
            for _ in range(5):
                self.heuristics.record_feedback(quarantined["id"], "helpful", project=project)
            top = self.heuristics.retrieve_top(5, project)
            ids = {item["id"] for item in top}
            self.assertIn(trusted["id"], ids)
            self.assertNotIn(quarantined["id"], ids)

    def test_unknown_id_and_bad_outcome_fail_closed(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            with self.assertRaises(ValueError):
                self.heuristics.record_feedback("missing", "helpful", project=project)
            item = self.heuristics.add_heuristic("Valid tip", project=project)
            with self.assertRaises(ValueError):
                self.heuristics.record_feedback(item["id"], "maybe", project=project)

    def test_entry_stamp_excluded_from_verify_payload(self):
        with tempfile.TemporaryDirectory() as tmp:
            overlay = pathlib.Path(tmp) / ".chaos-engine"
            overlay.mkdir()
            (overlay / "install.py").write_text("# core\n", encoding="utf-8")
            runtime = overlay / "runtime"
            runtime.mkdir()
            (runtime / "entry-stamp").write_text("deadbeefcafe\n", encoding="utf-8")
            # Minimal fake manifest matching only install.py
            import hashlib
            import json

            digest = hashlib.sha256(b"# core\n").hexdigest()
            (overlay / "manifest.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "commit": "0" * 40,
                        "distribution": "default",
                        "files": {"install.py": digest},
                        "capabilities": {},
                        "source": "local",
                    }
                ),
                encoding="utf-8",
            )
            payload = self.install.installed_payload(overlay)
            self.assertNotIn("runtime/entry-stamp", payload)
            self.assertIn("install.py", payload)
            # verify_install may still fail on capability policy; only assert payload skip
            self.assertTrue(self.install.is_generated_runtime_file(pathlib.Path("runtime/entry-stamp")))


if __name__ == "__main__":
    unittest.main()
