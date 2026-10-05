"""#6518: induce repeated successful step windows (AWM) behind the eval gate."""

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


class WorkflowInductionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/workflow_induce.py", "ce_workflow_6518")

    def _project(self, tmp: str) -> pathlib.Path:
        project = pathlib.Path(tmp)
        (project / ".chaos-engine").mkdir()
        (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
        return project

    def _cli(self, argv: list[str]) -> dict:
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            code = self.mod.main(argv)
        self.assertEqual(code, 0)
        return json.loads(buffer.getvalue())

    def test_reference_and_permanent_rule(self):
        refs = ROOT / "chaos-engine" / "references"
        text = (refs / "workflow-induction.md").read_text(encoding="utf-8")
        self.assertIn("2409.07429", text)
        self.assertIn("reg-workflow-induction-6518", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("workflow-induction.md", permanent)

    def test_cli_induces_repeated_success_and_ignores_failure(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            self._cli(
                ["trajectory", "--name", "ship-a", "--step", "edit gate", "--step", "run tests", *base]
            )
            self._cli(
                ["trajectory", "--name", "ship-b", "--step", "edit gate", "--step", "run tests", *base]
            )
            self._cli(
                [
                    "trajectory",
                    "--name",
                    "ship-fail",
                    "--step",
                    "edit gate",
                    "--step",
                    "skip tests",
                    "--failure",
                    *base,
                ]
            )
            induced = self._cli(["induce", *base])
            self.assertEqual(len(induced), 1)
            self.assertEqual(induced[0]["steps"], ["edit gate", "run tests"])
            self.assertEqual(induced[0]["support"], 2)
            ranked = self._cli(["retrieve", "--query", "run tests", *base])
            self.assertEqual(ranked[0]["id"], induced[0]["id"])

    def test_one_trajectory_induces_nothing(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            self._cli(
                ["trajectory", "--name", "once", "--step", "edit gate", "--step", "run tests", *base]
            )
            self.assertEqual(self._cli(["induce", *base]), [])

    def test_privacy_rejects_secret_and_path(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            with self.assertRaises(ValueError):
                self.mod.record_trajectory(
                    "leak", ["api_key=abcdefghijklmnop"], project=project
                )
            with self.assertRaises(ValueError):
                self.mod.record_trajectory("path", ["read /home/owner/file"], project=project)

    def test_materialize_follows_eval_manifest(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            self._cli(
                ["trajectory", "--name", "ship-a", "--step", "edit gate", "--step", "run tests", *base]
            )
            self._cli(
                ["trajectory", "--name", "ship-b", "--step", "edit gate", "--step", "run tests", *base]
            )
            induced = self._cli(["induce", *base])
            workflow_id = induced[0]["id"]
            closed = project / "closed.json"
            closed.write_text('{"tasks": []}\n', encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.main(
                    ["materialize", "--id", workflow_id, "--eval-manifest", str(closed), *base]
                )
            opened = self._cli(
                [
                    "materialize",
                    "--id",
                    workflow_id,
                    "--eval-manifest",
                    str(ROOT / "chaos-engine/evals/harness-suite/manifest.json"),
                    *base,
                ]
            )
            skill = pathlib.Path(opened["path"])
            body = skill.read_text(encoding="utf-8")
            self.assertIn("edit gate", body)
            self.assertTrue(str(skill).endswith(f"skills/{workflow_id}/SKILL.md"))
            report = self._cli(["summary", *base])
            self.assertEqual(report["workflowCount"], 1)
            self.assertEqual(report["status"], "healthy")
            self.assertTrue(report["evalGateOpen"])
