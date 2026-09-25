"""Instruction-only hosts: a missing `retrieve:` receipt is flagged, never blocking (#6201)."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - tests run fixed local scripts.
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _load():
    path = ROOT / "chaos-engine/learning_session.py"
    spec = importlib.util.spec_from_file_location("learning_session_6201", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


SESSION = _load()


class ReceiptFlagTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name)
        previous = Path.cwd()
        os.chdir(self.project)
        self.addCleanup(os.chdir, previous)

    def finalize(self, host):
        return SESSION.finalize("s-6201", extract_heuristics=False, host=host)

    def test_missing_receipt_is_flagged_not_blocking(self):
        receipt = self.finalize("cursor")
        self.assertEqual("issues-first", receipt["disposition"])
        self.assertEqual("missing", receipt["retrieveReceipt"]["status"])
        self.assertIn("missing-retrieve-receipt", receipt["flags"])

    def test_receipt_file_field_satisfies_the_check(self):
        sink = self.project / SESSION.RESEARCH_RECEIPT_SINK
        sink.parent.mkdir(parents=True)
        sink.write_text("Research receipt\n- retrieve: skipped(no-project-index)\n", encoding="utf-8")
        receipt = self.finalize("opencode")
        self.assertEqual("present", receipt["retrieveReceipt"]["status"])
        self.assertEqual("skipped(no-project-index)", receipt["retrieveReceipt"]["field"])
        self.assertNotIn("missing-retrieve-receipt", receipt.get("flags", []))

    def test_retrieve_ledger_counts_as_a_machine_readable_receipt(self):
        ledger = self.project / ".chaos-engine-state/retrieve-justification.json"
        ledger.parent.mkdir(parents=True)
        ledger.write_text(
            json.dumps({"citations": ["a/b.md"], "outcomes": []}), encoding="utf-8"
        )
        receipt = self.finalize("grok-bot")
        self.assertEqual("present", receipt["retrieveReceipt"]["status"])
        self.assertEqual("ledger", receipt["retrieveReceipt"]["source"])

    def test_hook_hosts_are_not_checked_here(self):
        receipt = self.finalize("claude")
        self.assertNotIn("retrieveReceipt", receipt)

    def test_cli_accepts_host(self):
        self.assertEqual(0, SESSION.main(["finalize", "--session-id", "s", "--host", "cursor", "--silent", "--no-heuristics"]))


class ShimEvaluationDocTest(unittest.TestCase):
    def test_reference_names_the_sink_and_the_shim_evaluation(self):
        text = (ROOT / "chaos-engine/references/research-receipt.md").read_text(encoding="utf-8")
        self.assertIn(".chaos-engine-state/research-receipt.md", text)
        self.assertIn("## Instruction-only hosts", text)
        for host in ("OpenCode", "Cursor", "Grok Bot", "Copilot cloud"):
            self.assertIn(host, text)


SHIM = ROOT / "chaos-engine/hooks/receipt_shim.py"


def _run_shim(project: Path, *args: str, stdin: str = "") -> subprocess.CompletedProcess:
    return subprocess.run(  # nosec B603 - fixed interpreter and repository script.
        [sys.executable, str(SHIM), *args],
        input=stdin,
        capture_output=True,
        text=True,
        cwd=project,
        timeout=60,
        check=False,
    )


class ReceiptShim6218Test(unittest.TestCase):
    """#6218: hook-capable instruction-only hosts open the receipt before the first read."""

    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name).resolve()
        self.sink = self.project / ".chaos-engine-state/research-receipt.md"

    def test_cursor_before_read_opens_a_pending_receipt_and_allows_the_read(self):
        payload = json.dumps({
            "hook_event_name": "beforeReadFile",
            "file_path": str(self.project / "src/Main.java"),
            "workspace_roots": [str(self.project)],
        })
        done = _run_shim(Path(tempfile.gettempdir()), "ensure", "--host", "cursor", stdin=payload)
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertEqual({"permission": "allow"}, json.loads(done.stdout))
        text = self.sink.read_text(encoding="utf-8")
        self.assertIn("host: cursor", text)
        self.assertIn("retrieve: pending", text)
        previous = Path.cwd()
        os.chdir(self.project)
        self.addCleanup(os.chdir, previous)
        self.assertEqual("missing", SESSION.retrieve_receipt_check(self.project)["status"])

    def test_an_existing_receipt_is_never_overwritten(self):
        self.sink.parent.mkdir(parents=True)
        self.sink.write_text("retrieve: skipped(one-file doc edit)\n", encoding="utf-8")
        done = _run_shim(self.project, "ensure", "--host", "opencode", "--project", str(self.project))
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertEqual("retrieve: skipped(one-file doc edit)\n", self.sink.read_text(encoding="utf-8"))

    def test_bad_input_never_blocks(self):
        done = _run_shim(self.project, "ensure", "--host", "cursor", stdin="not json")
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertEqual({"permission": "allow"}, json.loads(done.stdout))

    def test_cursor_install_merges_hooks_idempotently(self):
        hooks = self.project / ".cursor/hooks.json"
        hooks.parent.mkdir(parents=True)
        own = {"command": "./format.sh"}
        hooks.write_text(json.dumps({"version": 1, "hooks": {"afterFileEdit": [own]}}), encoding="utf-8")
        for _ in range(2):
            done = _run_shim(self.project, "install", "--host", "cursor", "--project", str(self.project))
            self.assertEqual(0, done.returncode, done.stderr)
        config = json.loads(hooks.read_text(encoding="utf-8"))
        self.assertEqual([own], config["hooks"]["afterFileEdit"])
        for event in ("beforeReadFile", "beforeShellExecution"):
            entries = config["hooks"][event]
            self.assertEqual(1, len(entries), event)
            self.assertIn("receipt_shim.py ensure --host cursor", entries[0]["command"])

    def test_opencode_plugin_opens_the_receipt_before_a_read_tool(self):
        node = shutil.which("node")
        if node is None:
            self.skipTest("node is not installed")
        done = _run_shim(self.project, "install", "--host", "opencode", "--project", str(self.project))
        self.assertEqual(0, done.returncode, done.stderr)
        plugin = self.project / ".opencode/plugins/chaos-engine-receipt.js"
        self.assertIn("tool.execute.before", plugin.read_text(encoding="utf-8"))
        installed = self.project / ".chaos-engine/hooks/receipt_shim.py"
        installed.parent.mkdir(parents=True)
        shutil.copy2(SHIM, installed)
        runner = self.project / "run.mjs"
        runner.write_text(
            "const mod = await import(process.argv[2]);\n"
            "const hooks = await mod.ChaosEngineReceipt({ directory: process.argv[3] });\n"
            "const fs = await import('node:fs');\n"
            "await hooks['tool.execute.before']({ tool: 'edit' }, { args: {} });\n"
            "console.log(fs.existsSync(process.argv[4]) ? 'early' : 'absent-after-edit');\n"
            "await hooks['tool.execute.before']({ tool: 'read' }, { args: {} });\n",
            encoding="utf-8",
        )
        result = subprocess.run(  # nosec B603 - fixed node binary and generated test files.
            [node, str(runner), plugin.as_uri(), str(self.project), str(self.sink)],
            capture_output=True, text=True, timeout=60, check=False,
        )
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertIn("absent-after-edit", result.stdout)
        self.assertIn("host: opencode", self.sink.read_text(encoding="utf-8"))

    def test_grok_bot_stays_instruction_only(self):
        done = _run_shim(self.project, "install", "--host", "grok-bot", "--project", str(self.project))
        self.assertNotEqual(0, done.returncode)
        self.assertIn("instruction-only", done.stderr)


class CopilotCloudSetupStep6218Test(unittest.TestCase):
    """#6218 (owner-approved 07:40 Cairo): Copilot cloud opens the receipt in its setup step."""

    WORKFLOW = ROOT / ".github/workflows/copilot-setup-steps.yml"

    def test_setup_step_opens_the_receipt_before_the_agent_starts(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary).resolve()
            done = _run_shim(project, "ensure", "--host", "copilot-cloud", "--project", str(project))
            self.assertEqual(0, done.returncode, done.stderr)
            text = (project / ".chaos-engine-state/research-receipt.md").read_text(encoding="utf-8")
            self.assertIn("host: copilot-cloud", text)
            self.assertIn("retrieve: pending", text)
            refused = _run_shim(project, "install", "--host", "copilot-cloud", "--project", str(project))
            self.assertNotEqual(0, refused.returncode)
            self.assertIn("setup step", refused.stderr)

    def test_workflow_is_the_minimal_copilot_setup_job(self):
        text = self.WORKFLOW.read_text(encoding="utf-8")
        self.assertIn("\n  copilot-setup-steps:\n", text, "Copilot only runs this exact job name")
        self.assertIn("timeout-minutes:", text)
        self.assertIn("contents: read", text)
        self.assertIn(
            "python3 chaos-engine/hooks/receipt_shim.py ensure --host copilot-cloud --project .",
            text,
        )
        self.assertNotIn("secrets.", text)
        readme = (ROOT / ".github/workflows/README.md").read_text(encoding="utf-8")
        self.assertIn("`copilot-setup-steps.yml`", readme)


if __name__ == "__main__":
    unittest.main()
