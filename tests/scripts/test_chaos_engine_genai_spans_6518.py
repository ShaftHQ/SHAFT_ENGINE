"""#6518: runs emit OpenTelemetry GenAI invoke_agent and execute_tool spans."""

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


class GenAISpansTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load(ROOT / "chaos-engine/genai_spans.py", "ce_genai_6518")

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
        text = (refs / "genai-spans.md").read_text(encoding="utf-8")
        self.assertIn("gen-ai-agent-spans", text)
        self.assertIn("reg-genai-spans-6518", text)
        permanent = (refs / "permanent-rules.md").read_text(encoding="utf-8")
        self.assertIn("genai-spans.md", permanent)

    def test_emit_links_a_tool_span_to_the_agent_span(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            base = ["--project", str(project)]
            manifest = str(ROOT / "chaos-engine/evals/harness-suite/manifest.json")
            closed = pathlib.Path(tmp) / "closed.json"
            closed.write_text('{"tasks": []}\n', encoding="utf-8")
            with self.assertRaises(ValueError):
                self.mod.emit_span(
                    self.mod.INVOKE_AGENT,
                    "local",
                    agent="runner",
                    project=project,
                    eval_manifest=closed,
                )
            agent = self._cli(
                [
                    "emit",
                    "--operation",
                    self.mod.INVOKE_AGENT,
                    "--provider",
                    "local",
                    "--agent",
                    "runner",
                    "--eval-manifest",
                    manifest,
                    *base,
                ]
            )
            self.assertEqual(agent["kind"], self.mod.SPAN_KIND)
            self.assertEqual(agent["name"], f"{self.mod.INVOKE_AGENT} runner")
            self.assertEqual(agent["attributes"][self.mod.ATTR_OPERATION], self.mod.INVOKE_AGENT)
            self.assertEqual(agent["attributes"][self.mod.ATTR_PROVIDER], "local")
            self.assertEqual(len(agent["traceId"]), 32)
            self.assertEqual(len(agent["spanId"]), 16)
            with self.assertRaises(ValueError):
                self.mod.emit_span(
                    self.mod.EXECUTE_TOOL,
                    "local",
                    tool="suite",
                    project=project,
                    eval_manifest=pathlib.Path(manifest),
                )
            tool = self._cli(
                [
                    "emit",
                    "--operation",
                    self.mod.EXECUTE_TOOL,
                    "--provider",
                    "local",
                    "--tool",
                    "suite",
                    "--parent",
                    agent["spanId"],
                    "--eval-manifest",
                    manifest,
                    *base,
                ]
            )
            self.assertEqual(tool["parentSpanId"], agent["spanId"])
            self.assertEqual(tool["name"], f"{self.mod.EXECUTE_TOOL} suite")
            self.assertEqual(tool["attributes"][self.mod.ATTR_TOOL], "suite")
            report = self._cli(["summary", *base])
            self.assertEqual(report["spanCount"], 2)
            self.assertEqual(report["byOperation"][self.mod.INVOKE_AGENT], 1)
            self.assertEqual(report["byOperation"][self.mod.EXECUTE_TOOL], 1)
            self.assertEqual(report["status"], "healthy")

    def test_private_provider_is_refused(self):
        with tempfile.TemporaryDirectory() as tmp:
            project = self._project(tmp)
            with self.assertRaises(ValueError):
                self.mod.emit_span(
                    self.mod.INVOKE_AGENT,
                    "api_key.abcdefghijklmnop",
                    agent="runner",
                    project=project,
                    eval_manifest=ROOT / "chaos-engine/evals/harness-suite/manifest.json",
                )
