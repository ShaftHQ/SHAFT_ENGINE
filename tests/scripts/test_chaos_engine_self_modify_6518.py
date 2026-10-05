"""#6518: self-modify only behind the eval suite, and archive the previous body."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import pathlib
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[2]


def load(path: pathlib.Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load module from {path}")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def suite_report(*, task_passed: bool, suite_passed: bool, required: float) -> dict:
    missed = required - required
    held = required if suite_passed and task_passed else missed
    task_score = required if task_passed else missed
    return {
        "passed": suite_passed and task_passed,
        "pass_at_k": held,
        "threshold_pass_at_k": required,
        "results": [
            {
                "id": "reg-self-modify-6518",
                "module": "tests.scripts.test_chaos_engine_self_modify_6518",
                "passed": task_passed,
                "pass_at_k": task_score,
            }
        ],
    }


class SelfModifyTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/self_modify.py", "ce_self_6518")
        suite = load(ROOT / "scripts/ci/chaos_engine_harness_eval_suite.py", "ce_suite_self_6518")
        cls.required = float(suite.load_manifest()["thresholds"]["pass_at_k"])

    def _project(self, tmp: str) -> pathlib.Path:
        project = pathlib.Path(tmp)
        (project / ".chaos-engine").mkdir()
        (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
        return project

    def _cli(self, argv: list[str]):
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            code = self.mod.main(argv)
        self.assertEqual(code, 0)
        return json.loads(buffer.getvalue())

    def test_reference_and_permanent_rule(self):
        refs = ROOT / "chaos-engine" / "references"
        text = (refs / "self-modify.md").read_text(encoding="utf-8")
        self.assertIn("2505.22954", text)
        self.assertIn("2504.15228", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("self-modify.md", permanent)

    def test_apply_archives_the_previous_body_and_rollback_restores_it(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            manifest = str(ROOT / "chaos-engine/evals/harness-suite/manifest.json")
            first = self._cli(["propose", "--label", "gate-note", "--text", "Keep the old body", *base])
            second = self._cli(["propose", "--label", "gate-note", "--text", "Keep the new body", *base])
            with self.assertRaises(ValueError):
                self.mod.apply_candidate(first["id"], project=project, eval_manifest=pathlib.Path(manifest))
            passed = pathlib.Path(tmp) / "passed.json"
            passed.write_text(
                json.dumps(suite_report(task_passed=True, suite_passed=True, required=self.required)),
                encoding="utf-8",
            )
            self._cli(["score", "--id", first["id"], "--report", str(passed), "--eval-manifest", manifest, *base])
            self._cli(["score", "--id", second["id"], "--report", str(passed), "--eval-manifest", manifest, *base])
            applied = self._cli(["apply", "--id", first["id"], "--eval-manifest", manifest, *base])
            self.assertEqual(applied["archiveCount"], 0)
            self.assertEqual(pathlib.Path(applied["path"]).read_text(encoding="utf-8").strip(), "Keep the old body")
            replaced = self._cli(["apply", "--id", second["id"], "--eval-manifest", manifest, *base])
            self.assertEqual(replaced["archiveCount"], 1)
            self.assertEqual(pathlib.Path(replaced["path"]).read_text(encoding="utf-8").strip(), "Keep the new body")
            archive_id = self.mod.load_index(project)["archive"][0]["id"]
            restored = self._cli(["rollback", "--archive-id", archive_id, *base])
            self.assertEqual(pathlib.Path(restored["path"]).read_text(encoding="utf-8").strip(), "Keep the old body")
            self.assertGreaterEqual(restored["archiveCount"], 2)
            shipped = ROOT / "chaos-engine" / "self_modify.py"
            self.assertNotIn("Keep the new body", shipped.read_text(encoding="utf-8"))

    def test_closed_gate_bad_label_and_private_text_are_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            closed = pathlib.Path(tmp) / "closed.json"
            closed.write_text('{"tasks": []}\n', encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.propose("../escape", "body", project=project)
            with self.assertRaises(ValueError):
                self.mod.propose("note", "api_key=abcdefghijklmnop", project=project)
            item = self.mod.propose("note", "Stay in state", project=project)
            with self.assertRaises(ValueError):
                self.mod.score_candidate(item["id"], closed, project=project, eval_manifest=closed)
            report = self._cli(["summary", "--project", str(project)])
            self.assertEqual(report["appliedCount"], 0)
            self.assertTrue(report["evalGateOpen"])
