"""#6174: the retrieve gate scopes project reads only.

Running a script is not reading it, harness files are exempt, the project
root is resolved the same way as `retrieve.project_root()`, and a graph with
no project nodes fails open with `skipped(no-project-index)`. Table-driven
over every hook host in `agent_harness_parity.json`, on an installed
empty-project fixture.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess  # nosec B404 - fixed interpreter and installed fixture hook.
import sys
import tempfile
import unittest
from pathlib import Path

from tests.scripts.ce_installed_fixture import (
    HOOK_HOSTS,
    READ_EVENTS,
    ROOT,
    build_installed_project,
    load_module,
    parity_hosts,
)

ALLOWED_READS = (
    ".chaos-engine/hooks/guard.py",
    ".chaos-engine/skills/chaos-engine/SKILL.md",
    ".chaos-engine/vendor/caveman/skills/caveman/SKILL.md",
    ".chaos-engine/vendor/ponytail/skills/ponytail/SKILL.md",
    ".claude/settings.json",
    ".codex/config.toml",
    ".gemini/settings.json",
)
ALLOWED_RUNS = (
    "python3 .chaos-engine/learning.py finalize --help",
    "python3 .chaos-engine/install.py doctor",
    "py -3 .chaos-engine/install.py doctor --project .",
    "python3 scripts/agents/watch_pr_checks.py --pr 1 --poll-once",
    "python3 .chaos-engine/skills/local-agency/scripts/dispatch.py --help",
    "python3 -m unittest tests/scripts/test_x.py",
    "python3 -m unittest tests.scripts.test_x -v",
    "node .chaos-engine/hooks/launch.js",
    "bash scripts/ci/build_retry.sh",
    "cat .chaos-engine/references/retrieve-first.md",
    "sed -n 1,40p .chaos-engine/hooks/guard.py",
)
BLOCKED_COMMANDS = (
    "rg foo src/",
    "cat src/Foo.java",
    "sed -n 1,10p src/Foo.java",
    "python3 -c \"print(open('src/Foo.java').read())\"",
)


class RetrieveGateScopeTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls._tmp = tempfile.TemporaryDirectory()
        cls.project = build_installed_project(Path(cls._tmp.name) / "adopter")
        cls.gate = load_module(
            "ce_installed_retrieve_justification",
            cls.project / ".chaos-engine/hooks/retrieve_justification.py",
        )

    @classmethod
    def tearDownClass(cls):
        cls._tmp.cleanup()

    def setUp(self):
        for name in (".chaos-engine-state", "graphify-out"):
            shutil.rmtree(self.project / name, ignore_errors=True)

    def block(self, *, tool_input=None, commands=(), tool="Read", cwd=None):
        return self.gate.file_read_block_reason(
            project=self.gate.project_root(Path(cwd or self.project)),
            event_name="PreToolUse",
            tool_name=tool if tool_input is not None else "Bash",
            tool_input=tool_input or {},
            commands=tuple(commands),
        )

    def run_installed_guard(self, host: str, cwd: Path, file_path: str):
        payload = {
            **READ_EVENTS[host],
            "cwd": str(cwd),
            "session_id": f"gate-scope-{host}",
            "tool_input": {"file_path": file_path},
            "toolArgs": {"file_path": file_path},
        }
        with tempfile.TemporaryDirectory() as state:
            return subprocess.run(  # nosec B603 - fixed interpreter and installed hook.
                [sys.executable, str(self.project / ".chaos-engine/hooks/guard.py")],
                input=json.dumps(payload),
                capture_output=True,
                text=True,
                check=False,
                cwd=cwd,
                env={**os.environ, "CHAOS_ENGINE_HOST": host, "TMPDIR": state},
            )

    def test_parity_matrix_lists_every_supported_host(self):
        self.assertEqual(
            ["claude", "codex", "copilot", "gemini", "grok", "opencode", "cursor", "grok-bot"],
            parity_hosts(),
        )

    def test_harness_reads_are_exempt(self):
        for path in ALLOWED_READS:
            with self.subTest(path=path):
                self.assertTrue(self.gate.is_harness_path(path, self.project))
                self.assertIsNone(self.block(tool_input={"file_path": path}))

    def test_running_a_script_is_not_reading_it(self):
        for command in ALLOWED_RUNS:
            with self.subTest(command=command):
                self.assertIsNone(self.block(commands=(command,)))
        self.assertEqual("run", self.gate.segment_kind("python3 .chaos-engine/install.py doctor"))
        self.assertEqual("run", self.gate.segment_kind("node x.js"))
        self.assertEqual("read", self.gate.segment_kind("sed -n 1,5p src/Foo.java"))
        self.assertEqual("read", self.gate.segment_kind("python3 -c \"open('a/b.py')\""))

    def test_uncited_project_reads_stay_blocked(self):
        self.assertIsNotNone(self.block(tool_input={"file_path": "src/Foo.java"}))
        for command in BLOCKED_COMMANDS:
            with self.subTest(command=command):
                self.assertIsNotNone(self.block(commands=(command,)))

    def test_session_start_companion_locators_are_allowed(self):
        lifecycle = load_module(
            "ce_installed_lifecycle_locators", self.project / ".chaos-engine/hooks/lifecycle.py"
        )
        previous = Path.cwd()
        os.chdir(self.project)
        try:
            context = lifecycle.session_start_context(None, "startup")
        finally:
            os.chdir(previous)
        locators = self.gate.session_start_locators(context)
        self.assertTrue(any("vendor/caveman" in item for item in locators), context)
        for locator in locators:
            with self.subTest(locator=locator):
                self.assertIsNone(self.block(tool_input={"file_path": locator}))

    def test_subdirectory_cwd_sees_the_root_ledger(self):
        self.gate.record_citations(self.project, "graphify", "Source: src/Foo.java")
        nested = self.project / "src/sub"
        self.assertEqual(self.project.resolve(), self.gate.project_root(nested).resolve())
        self.assertIsNone(self.block(tool_input={"file_path": "src/Foo.java"}, cwd=nested))

    def test_dot_paths_under_harness_roots_are_citable(self):
        cited = self.gate.extract_citations("see .chaos-engine/references/tdd.md and src/Foo.java")
        self.assertIn(".chaos-engine/references/tdd.md", cited)
        self.assertIn("src/Foo.java", cited)

    def test_graph_without_project_nodes_fails_open_as_skipped(self):
        graph = self.project / "graphify-out"
        graph.mkdir(exist_ok=True)
        (graph / "graph.json").write_text(
            json.dumps({"nodes": [{"id": "g", "source_file": ".chaos-engine/hooks/guard.py"}]}),
            encoding="utf-8",
        )
        self.assertEqual("no-project-index", self.gate.project_index_state(self.project))
        self.assertIsNone(self.block(tool_input={"file_path": "src/Foo.java"}))
        outcomes = json.loads(
            (self.project / ".chaos-engine-state/retrieve-justification.json").read_text(encoding="utf-8")
        )["outcomes"]
        self.assertIn({"store": "graphify", "status": "skipped", "reason": "no-project-index"}, [
            {key: item.get(key) for key in ("store", "status", "reason")} for item in outcomes
        ])

    def test_graph_with_project_nodes_keeps_the_gate(self):
        graph = self.project / "graphify-out"
        graph.mkdir(exist_ok=True)
        (graph / "graph.json").write_text(
            json.dumps({"nodes": [{"id": "f", "source_file": "src/Foo.java"}]}), encoding="utf-8"
        )
        self.assertEqual("ok", self.gate.project_index_state(self.project))
        self.assertIsNotNone(self.block(tool_input={"file_path": "src/Foo.java"}))

    def test_every_hook_host_applies_the_same_scope(self):
        for host in HOOK_HOSTS:
            with self.subTest(host=host, case="harness"):
                result = self.run_installed_guard(host, self.project, ".chaos-engine/hooks/guard.py")
                self.assertEqual(0, result.returncode, result.stdout + result.stderr)
            with self.subTest(host=host, case="project"):
                result = self.run_installed_guard(host, self.project, "src/Foo.java")
                self.assertEqual(2, result.returncode, result.stdout + result.stderr)

    def test_instruction_only_hosts_record_the_receipt_field(self):
        receipt = (ROOT / "chaos-engine/references/research-receipt.md").read_text(encoding="utf-8")
        self.assertIn("retrieve: used|skipped(<reason>)|exempt(harness)", receipt)
        self.assertEqual("used", self.gate.retrieve_receipt_field("retrieve: used"))
        self.assertEqual(
            "skipped(no-project-index)",
            self.gate.retrieve_receipt_field("x\nretrieve: skipped(no-project-index)\n"),
        )
        self.assertIsNone(self.gate.retrieve_receipt_field("no field here"))

    def test_retrieve_first_rule_text_is_the_epic_text(self):
        text = (ROOT / "chaos-engine/references/retrieve-first.md").read_text(encoding="utf-8")
        for phrase in (
            "Graph and memory first, when it pays.",
            "One retrieve per task area, not per file.",
            "record `skipped(<reason>)` once and continue",
            "harness-index.json",
        ):
            self.assertIn(phrase, text)


if __name__ == "__main__":
    unittest.main()
