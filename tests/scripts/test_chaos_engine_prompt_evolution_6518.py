"""#6518: accept prompt or skill text only after a passing harness eval report."""

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


def suite_report(*, task_passed: bool, suite_passed: bool) -> dict:
    return {
        "passed": suite_passed and task_passed,
        "pass_at_k": 1.0 if suite_passed and task_passed else 0.0,
        "threshold_pass_at_k": 1.0,
        "results": [
            {
                "id": "reg-prompt-evolution-6518",
                "module": "tests.scripts.test_chaos_engine_prompt_evolution_6518",
                "passed": task_passed,
                "pass_at_k": 1.0 if task_passed else 0.0,
            }
        ],
    }


class PromptEvolutionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/prompt_evolve.py", "ce_prompt_6518")

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
        text = (refs / "prompt-evolution.md").read_text(encoding="utf-8")
        self.assertIn("2507.19457", text)
        self.assertIn("reg-prompt-evolution-6518", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("prompt-evolution.md", permanent)

    def test_accept_requires_a_passing_suite_report(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            manifest = str(ROOT / "chaos-engine/evals/harness-suite/manifest.json")
            proposed = self._cli(
                [
                    "propose",
                    "--kind",
                    "skill",
                    "--name",
                    "shorter-gate",
                    "--text",
                    "Register a new harness test before pushing",
                    *base,
                ]
            )
            candidate_id = proposed["id"]
            with self.assertRaises(ValueError):
                self.mod.accept_candidate(candidate_id, project=project, eval_manifest=pathlib.Path(manifest))
            failed = pathlib.Path(tmp) / "failed.json"
            failed.write_text(
                json.dumps(suite_report(task_passed=False, suite_passed=False)),
                encoding="utf-8",
            )
            with self.assertRaises(ValueError):
                self.mod.main(
                    ["score", "--id", candidate_id, "--report", str(failed), "--eval-manifest", manifest, *base]
                )
            passed = pathlib.Path(tmp) / "passed.json"
            passed.write_text(
                json.dumps(suite_report(task_passed=True, suite_passed=True)),
                encoding="utf-8",
            )
            scored = self._cli(
                ["score", "--id", candidate_id, "--report", str(passed), "--eval-manifest", manifest, *base]
            )
            self.assertTrue(scored["score"]["passed"])
            accepted = self._cli(
                ["accept", "--id", candidate_id, "--eval-manifest", manifest, *base]
            )
            body = pathlib.Path(accepted["path"]).read_text(encoding="utf-8")
            self.assertIn("Register a new harness test", body)
            self.assertTrue(accepted["path"].endswith(f"accepted/{candidate_id}.md"))
            report = self._cli(["summary", *base])
            self.assertEqual(report["acceptedCount"], 1)
            self.assertTrue(report["evalGateOpen"])

    def test_closed_gate_and_private_text_are_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            closed = pathlib.Path(tmp) / "closed.json"
            closed.write_text('{"tasks": []}\n', encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.propose("note", "x", "text", project=project)
            with self.assertRaises(ValueError):
                self.mod.propose("prompt", "x", "api_key=abcdefghijklmnop", project=project)
            item = self.mod.propose("prompt", "keep", "Prefer the suite score", project=project)
            with self.assertRaises(ValueError):
                self.mod.score_candidate(
                    item["id"],
                    closed,
                    project=project,
                    eval_manifest=closed,
                )
