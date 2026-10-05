"""#6518: reflections stay grounded in tests, CI, or doctor."""

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


class ExternalReflectionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/external_reflection.py", "ce_extref_6518")

    def _project(self, tmp: str) -> pathlib.Path:
        project = pathlib.Path(tmp)
        (project / ".chaos-engine").mkdir()
        (project / ".chaos-engine" / "install.py").write_text("# probe\n", encoding="utf-8")
        return project

    def _cli(self, argv: list[str]) -> dict | list:
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            code = self.mod.main(argv)
        self.assertEqual(code, 0)
        return json.loads(buffer.getvalue())

    def test_reference_and_permanent_rule(self):
        refs = ROOT / "chaos-engine" / "references"
        text = (refs / "external-reflection.md").read_text(encoding="utf-8")
        self.assertIn("2310.01798", text)
        self.assertIn("2303.11366", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("external-reflection.md", permanent)

    def test_cli_keeps_external_signals_and_ranks_failures(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            for signal, outcome, summary in (
                ("tests", "pass", "focused unittest passed"),
                ("ci", "fail", "pr gate failed on the new module"),
                ("doctor", "pass", "doctor reported a healthy install"),
            ):
                self._cli(
                    [
                        "record",
                        "--signal",
                        signal,
                        "--outcome",
                        outcome,
                        "--summary",
                        summary,
                        *base,
                    ]
                )
            ranked = self._cli(["retrieve", "--query", "e", *base])
            self.assertEqual(ranked[0]["signal"], "ci")
            self.assertEqual(ranked[0]["outcome"], "fail")
            report = self._cli(["summary", *base])
            self.assertEqual(report["bySignal"], {"tests": 1, "ci": 1, "doctor": 1})
            self.assertEqual(report["status"], "healthy")

    def test_self_report_and_private_text_are_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            with self.assertRaises(ValueError):
                self.mod.record_reflection(
                    "model", "pass", "I believe the change is correct", project=project
                )
            with self.assertRaises(SystemExit):
                self.mod.main(
                    [
                        "record",
                        "--signal",
                        "self",
                        "--outcome",
                        "pass",
                        "--summary",
                        "looks good",
                        "--project",
                        str(project),
                    ]
                )
            with self.assertRaises(ValueError):
                self.mod.record_reflection(
                    "tests", "fail", "token=abcdefghijklmnop leaked", project=project
                )
            self.assertEqual(self.mod.summary(project)["reflectionCount"], 0)
