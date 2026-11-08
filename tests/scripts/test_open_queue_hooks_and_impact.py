"""#7831 #7830 #7826 #7824: hook polyglot, plugin-list timeout, impact retrieve."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import os
import shutil
import subprocess  # nosec B404 - drives the shipped hook command.
import tempfile
import unittest
import urllib.error
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, relative: str):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {relative}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


HOSTS = _load("open_queue_hosts", "chaos-engine/hosts.py")
BOOTSTRAP = _load("open_queue_bootstrap", "chaos-engine/bootstrap.py")
RETRIEVE = _load("open_queue_retrieve", "chaos-engine/retrieve.py")
JUSTIFICATION = _load("open_queue_justification", "chaos-engine/hooks/retrieve_justification.py")
GUARD = _load("open_queue_guard", "chaos-engine/hooks/guard.py")
SHIM = _load("open_queue_shim", "chaos-engine/hooks/receipt_shim.py")
DEPENDENCIES = _load("open_queue_dependencies", "chaos-engine/dependencies.py")


class NullSinkAndLifecycleTest(unittest.TestCase):
    def test_dev_null_redirect_is_the_drive_qualified_out_file_target(self) -> None:
        legacy = "command -v python3 >/dev/null 2>&1 && exec python3 tool; echo 'x'; exit 2"
        self.assertEqual(["D:\\dev\\null"], HOSTS.powershell_out_file_targets(legacy))
        self.assertIsNotNone(HOSTS.powershell_hook_parse_error(legacy))

    def test_generated_commands_have_no_out_file_path(self) -> None:
        for host, event, windows in (
            ("grok", None, False),
            ("claude", None, False),
            ("copilot", "userPromptSubmitted", False),
            ("copilot", "preToolUse", True),
            ("grok", None, True),
        ):
            command = HOSTS.chaos_guard_locator_command(windows=windows, host=host, event=event)
            with self.subTest(host=host, windows=windows, event=event):
                self.assertEqual([], HOSTS.powershell_out_file_targets(command))
                self.assertNotIn("D:\\dev\\null", command)
                self.assertNotIn(">/dev/null", command)
                self.assertLessEqual(len(command), HOSTS.HOOK_COMMAND_MAX_CHARS)

    def test_bash_uses_py_when_python3_is_missing_and_does_not_emit_the_lifecycle_block(self) -> None:
        command = HOSTS.chaos_guard_locator_command(windows=False, host="grok")
        with tempfile.TemporaryDirectory() as temporary:
            bindir = Path(temporary) / "bin"
            bindir.mkdir()
            py = bindir / "py"
            py.write_text("#!/bin/sh\nprintf '%s\\n' '{}'\nexit 0\n", encoding="utf-8")
            py.chmod(0o755)  # nosec B103 - fixture interpreter the test itself runs.
            environment = os.environ.copy()
            environment["PATH"] = str(bindir)
            bash = shutil.which("bash") or "/bin/bash"
            result = subprocess.run(  # nosec B603 - fixed bash argv, no shell.
                [bash, "-lc", command],
                capture_output=True,
                text=True,
                encoding="utf-8",
                check=False,
                cwd=temporary,
                env=environment,
            )
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertNotIn(HOSTS.HOOK_LAUNCH_PYTHON_BLOCK, result.stdout + result.stderr)
        self.assertNotIn("prompt blocked by project/lifecycle", result.stdout + result.stderr)
        self.assertEqual({}, json.loads(result.stdout or "{}"))


class PluginListTimeoutTest(unittest.TestCase):
    def test_a_still_running_plugin_list_returns_an_empty_document(self) -> None:
        def runner(*_args, **_kwargs):
            raise subprocess.TimeoutExpired(
                ["tool", "plugin", "list", "--available", "--json"], 30
            )

        result = HOSTS.client_command(
            "tool",
            ["plugin", "list", "--available", "--json"],
            Path("."),
            runner=runner,
        )
        self.assertEqual(0, result.returncode)
        self.assertEqual("{}", result.stdout)

    def test_a_different_timeout_still_raises(self) -> None:
        def runner(*_args, **_kwargs):
            raise subprocess.TimeoutExpired(["tool", "plugin", "marketplace", "list"], 30)

        with self.assertRaises(subprocess.TimeoutExpired):
            HOSTS.client_command(
                "tool", ["plugin", "marketplace", "list"], Path("."), runner=runner
            )

    def test_a_failed_plugin_command_still_raises(self) -> None:
        def runner(*_args, **_kwargs):
            return subprocess.CompletedProcess(["tool"], 1, stdout="", stderr="no")

        with self.assertRaises(RuntimeError):
            HOSTS.client_command(
                "tool",
                ["plugin", "list", "--available", "--json"],
                Path("."),
                runner=runner,
            )

    def test_classify_does_not_call_a_running_plugin_list_a_failed_install(self) -> None:
        timeout = subprocess.TimeoutExpired(
            ["tool", "plugin", "list", "--available", "--json"], 30
        )
        self.assertEqual("CE-INSTALL-PLUGIN-LIST-RUNNING", BOOTSTRAP.classify_install_error(timeout))
        self.assertEqual(
            "CE-INSTALL-FAILED",
            BOOTSTRAP.classify_install_error(RuntimeError("client plugin command failed")),
        )


class ImpactRetrieveAndEditReceiptTest(unittest.TestCase):
    def _project(self) -> Path:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        project = Path(temporary.name)
        graph = project / "graphify-out"
        graph.mkdir()
        pages = {
            "pages": [
                {
                    "path": "chaos-engine/skills/widget/SKILL.md",
                    "headings": ["Widget"],
                    "text": "The contract flag is --widget-flag.",
                    "sections": [],
                },
                {
                    "path": "chaos-engine/INSTALL.md",
                    "headings": ["Install"],
                    "text": "Pass --widget-flag to the one-liner.",
                    "sections": [],
                },
                {
                    "path": "chaos-engine/notes/unrelated.md",
                    "headings": ["Other"],
                    "text": "This page does not state the flag.",
                    "sections": [],
                },
            ]
        }
        (graph / "documentation-pages.json").write_text(json.dumps(pages), encoding="utf-8")
        nodes = [
            {"path": f"src/Filler{index}.java", "label": f"Filler{index}", "rationale": "unrelated"}
            for index in range(12)
        ]
        (graph / "graph.json").write_text(json.dumps({"nodes": nodes, "edges": []}), encoding="utf-8")
        readme = project / "chaos-engine" / "README.md"
        readme.parent.mkdir(parents=True)
        readme.write_text(
            "<!-- inventory:external-services:start -->\n"
            "| Item | Purpose |\n| --- | --- |\n"
            "| ExampleOrg/companion-docs | Publish companion functional documentation. |\n"
            "<!-- inventory:external-services:end -->\n",
            encoding="utf-8",
        )
        (project / "chaos-engine" / "install.py").write_text("# marker\n", encoding="utf-8")
        return project

    def test_impact_retrieve_ranks_the_skill_and_install_page_and_skips_an_absent_companion(self) -> None:
        project = self._project()
        receipt = RETRIEVE.retrieve("--widget-flag", store="graphify", project=project, mode="impact")
        paths = [hit["path"] for hit in receipt["hits"]]
        self.assertLessEqual(len(paths), 8)
        self.assertIn("chaos-engine/skills/widget/SKILL.md", paths)
        self.assertIn("chaos-engine/INSTALL.md", paths)
        self.assertLess(
            paths.index("chaos-engine/skills/widget/SKILL.md"),
            paths.index("src/Filler0.java"),
        )
        self.assertLess(paths.index("chaos-engine/INSTALL.md"), paths.index("src/Filler0.java"))
        self.assertIn("companion page was not searched", str(receipt.get("companion")))
        self.assertNotIn("companionPath", receipt)

    def test_missing_impact_receipt_denies_the_first_edit_and_exits_2(self) -> None:
        project = self._project()
        event = {
            "hook_event_name": "PreToolUse",
            "tool_name": "Write",
            "tool_input": {"file_path": "chaos-engine/hosts.py"},
            "session_id": "impact-missing",
            "cwd": str(project),
        }
        reason = JUSTIFICATION.impact_edit_block_reason(
            project=project,
            event_name="PreToolUse",
            tool_name="Write",
            session_id="impact-missing",
        )
        self.assertIn("Impact receipt missing", reason or "")
        self.assertIsNone(
            JUSTIFICATION.impact_edit_block_reason(
                project=project,
                event_name="PreToolUse",
                tool_name="Read",
                session_id="impact-missing",
            )
        )
        for host in ("grok", "copilot"):
            buffer = io.StringIO()
            with redirect_stdout(buffer):
                code = GUARD._guarded_event(dict(event), host)
            self.assertEqual(2, code, buffer.getvalue())
            self.assertIn("Impact receipt missing", buffer.getvalue())
        JUSTIFICATION._mark_session(project, "impact-present", retrieved=True)
        self.assertIsNone(
            JUSTIFICATION.impact_edit_block_reason(
                project=project,
                event_name="PreToolUse",
                tool_name="Edit",
                session_id="impact-present",
            )
        )

    def test_instruction_only_hosts_record_the_retrieve_line_and_gain_no_shim(self) -> None:
        project = self._project()
        for host in ("cursor", "opencode", "grok-bot"):
            line = JUSTIFICATION.record_instruction_only_impact_line(project, host, "used")
            self.assertEqual("retrieve: used", line)
            text = (project / ".chaos-engine-state" / "research-receipt.md").read_text(encoding="utf-8")
            self.assertIn("retrieve: used", text)
            self.assertFalse((project / ".cursor" / "hooks.json").exists())
            self.assertFalse((project / ".opencode" / "plugins" / "chaos-engine-receipt.js").exists())
        stdout = io.StringIO()
        with redirect_stdout(stdout):
            code = SHIM.main(["install", "--host", "grok-bot", "--project", str(project)])
        self.assertEqual(2, code, stdout.getvalue())
        stderr = io.StringIO()
        with contextlib.redirect_stderr(stderr):
            code = SHIM.main(["install", "--host", "grok-bot", "--project", str(project)])
        self.assertEqual(2, code)
        self.assertIn("instruction-only", stderr.getvalue())
        self.assertIn("no receipt shim", stderr.getvalue())


class PinnedRuntimeWhenChannelFailsTest(unittest.TestCase):
    def test_missing_uv_installs_the_pinned_runtime_when_the_channel_returns_http_error(self) -> None:
        specification = json.loads((ROOT / "chaos-engine" / "dependencies.json").read_text(encoding="utf-8"))
        local = {name: {"healthy": True, "version": "1.0.0"} for name in specification["dependencies"]}
        local["uv"] = {"healthy": False, "version": None, "status": "absent"}
        error = urllib.error.HTTPError("https://api.github.com/repos/astral-sh/uv/releases/latest", 503, "down", {}, None)
        with mock.patch.object(DEPENDENCIES, "resolve_stable_version", side_effect=error):
            actions = DEPENDENCIES.resolve_account_actions(specification, local)
        self.assertEqual("installed", actions["uv"]["action"])
        self.assertEqual(specification["runtimes"]["uv"]["version"], actions["uv"]["resolvedVersion"])
        self.assertNotIn("uv (", DEPENDENCIES.describe_blocked_dependencies(actions))


if __name__ == "__main__":
    unittest.main()
