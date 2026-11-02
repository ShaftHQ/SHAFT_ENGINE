"""#6518: context over the module budget is rot; compaction must shrink it."""

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


class ContextRotTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/context_rot.py", "ce_rot_6518")

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
        text = (refs / "context-rot.md").read_text(encoding="utf-8")
        self.assertIn("reg-context-rot-6518", text)
        self.assertIn("BUDGET_CHARS", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("context-rot.md", permanent)

    def test_check_and_compact_use_the_shipped_budget(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            manifest = str(ROOT / "chaos-engine/evals/harness-suite/manifest.json")
            budget = self.mod.BUDGET_CHARS
            anchor = "keep-anchor"
            filler = "n" * (budget - len(anchor) + 1)
            self._cli(["record", "--text", anchor, "--keep", *base])
            self._cli(["record", "--text", filler, *base])
            before = self._cli(["check", *base])
            self.assertEqual(before["budget"], budget)
            self.assertEqual(before["status"], "over")
            self.assertTrue(before["compactionNeeded"])
            self.assertGreater(before["chars"], budget)
            closed = pathlib.Path(tmp) / "closed.json"
            closed.write_text('{"tasks": []}\n', encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.compact(project, eval_manifest=closed)
            shrunk = self._cli(["compact", "--eval-manifest", manifest, *base])
            self.assertLess(shrunk["after"], shrunk["before"])
            self.assertLessEqual(shrunk["after"], budget)
            body = pathlib.Path(shrunk["path"]).read_text(encoding="utf-8")
            self.assertIn(anchor, body)
            self.assertNotIn(filler, body)
            after = self._cli(["check", *base])
            self.assertEqual(after["status"], "within")
            self.assertFalse(after["compactionNeeded"])

    def test_private_text_and_noop_compaction_are_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            with self.assertRaises(ValueError):
                self.mod.record_segment("api_key=abcdefghijklmnop", project=project)
            self.mod.record_segment("only-kept", keep=True, project=project)
            manifest = ROOT / "chaos-engine/evals/harness-suite/manifest.json"
            with self.assertRaises(ValueError):
                self.mod.compact(project, eval_manifest=manifest)
