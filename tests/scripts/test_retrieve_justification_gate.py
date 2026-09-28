"""Retrieve-first gate allowlist, directory prefix, and fail-open (#6126 follow-up)."""

from __future__ import annotations

import importlib.util
import json
import os
import tempfile
import unittest
import unittest.mock
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
_OWED_RETRIEVE = (
    'python3 .chaos-engine/tool.py retrieve --store graphify '
    '"<what calls or depends on this>"'
)
_ALLOW_OWED = (
    "A cheap read does not wait for a store. A broad search is allowed and "
    "owes one retrieve for that session."
)


def load(relative: str, name: str):
    path = ROOT / relative
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise AssertionError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class RetrieveJustificationGateTest(unittest.TestCase):
    def test_router_memory_and_small_heal_artifacts_need_no_citation(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_allow")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            state = project / ".chaos-engine-state"
            state.mkdir()
            (state / "doctor-failure.json").write_text("{}" + "\n", encoding="utf-8")
            (state / "install-console.log").write_text("x" * (gate.HEAL_ARTIFACT_CAP + 1), encoding="utf-8")
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "chaos-engine/skills/chaos-engine/SKILL.md"},
                    commands=(),
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "chaos-engine/identity.md"},
                    commands=(),
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "/home/user/.grok/memory-v2/workspaces/demo/topics/grok-tui.md"},
                    commands=(),
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "chaos-engine/bootstrap.py"},
                    commands=(),
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": ".chaos-engine-state/install-console.log"},
                    commands=(),
                )
            )
            self.assertIsNotNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Bash",
                    tool_input={},
                    commands=("cat .chaos-engine-state/install-console.log",),
                )
            )

    def test_cited_file_unlocks_its_directory_for_later_reads(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_prefix")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            gate.record_citations(project, "graphify", "NODE install [src=chaos-engine/hosts.py loc=L1]")
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"path": "chaos-engine/install.py"},
                    commands=(),
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "tests/scripts/test_chaos_engine_installer.py"},
                    commands=(),
                )
            )
            absolute = str(project / "chaos-engine" / "install.py")
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": absolute},
                    commands=(),
                )
            )

    def test_degraded_store_fails_open_only_for_paths_in_the_query(self):
        """A degraded receipt clears retrieveOwed for that session, not another."""
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_open")
        # #6219: project paths, not harness paths (harness is exempt since #6174).
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            with unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_SESSION_ID": "open"}):
                gate.record_store_outcome(
                    project,
                    "mempalace",
                    "degraded",
                    "src/bootstrap.py chroma mismatch",
                    "",
                )
            for target in ("src/bootstrap.py", "src/hosts.py"):
                self.assertIsNone(
                    gate.file_read_block_reason(
                        project=project,
                        event_name="PreToolUse",
                        tool_name="Read",
                        tool_input={"target_file": target},
                        commands=(),
                        session_id="open",
                    ),
                    target,
                )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"pattern": "bootstrap", "glob": "*.py"},
                    commands=(),
                    session_id="open",
                )
            )
            self.assertIsNone(gate.session_retrieve_gap(project, "open"))
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"pattern": "bootstrap", "glob": "*.py"},
                    commands=(),
                    session_id="other",
                )
            )
            self.assertEqual(
                _OWED_RETRIEVE,
                gate.session_retrieve_gap(project, "other"),
            )

    def test_hits_prefer_query_tokens_and_drop_the_budget_hint(self):
        retrieve = load("chaos-engine/retrieve.py", "retrieve_hits")
        body = """
NODE TouchActions [src=shaft-engine/src/TouchActions.java loc=L1]
NODE install [src=chaos-engine/install.py loc=L12]
[!] TRUNCATED: raise the token budget (CLI: --budget) or narrow the query.
"""
        hits = retrieve._structured_hits(body, query="install.py quarantine rebind")
        self.assertEqual(["chaos-engine/install.py"], [item["path"] for item in hits])
        excerpt = retrieve._bounded_excerpt(body)
        self.assertNotIn("--budget", excerpt)
        self.assertIn("TRUNCATED", excerpt)

    def test_graphify_citation_authorizes_a_second_read_and_denies_uncited(self):
        """Uncited reads run. A glob-only search owes a retrieve per session."""
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_session")
        retrieve = load("chaos-engine/retrieve.py", "retrieve_session")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            tool = project / ".chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("raise SystemExit(0)\n", encoding="utf-8")
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "tests/fixtures/uncited.py"},
                    commands=(),
                    session_id="first",
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"path": "tests/fixtures/uncited.py", "pattern": "def"},
                    commands=(),
                    session_id="first",
                )
            )
            self.assertIsNone(gate.session_retrieve_gap(project, "first"))
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"pattern": "def", "glob": "*.py"},
                    commands=(),
                    session_id="first",
                )
            )
            self.assertEqual(_OWED_RETRIEVE, gate.session_retrieve_gap(project, "first"))
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"pattern": "def", "glob": "*.py"},
                    commands=(),
                    session_id="second",
                )
            )
            body = "NODE guard [src=chaos-engine/hooks/guard.py loc=L10]\n"
            completed = unittest.mock.Mock(returncode=0, stdout=body, stderr="")
            with (
                unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_SESSION_ID": "first"}),
                unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed) as run,
            ):
                receipt = retrieve.retrieve("guard.py calls", store="graphify", project=project)
            self.assertEqual("used", receipt["status"])
            self.assertEqual(1, run.call_count)
            self.assertIsNone(gate.session_retrieve_gap(project, "first"))
            self.assertEqual(_OWED_RETRIEVE, gate.session_retrieve_gap(project, "second"))

    def test_backend_mismatch_is_recorded_once_and_does_not_touch_mempalace(self):
        retrieve = load("chaos-engine/retrieve.py", "retrieve_mismatch")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            home = project / "home"
            home.mkdir()
            tool = project / ".chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("raise SystemExit(0)\n", encoding="utf-8")
            completed = unittest.mock.Mock(
                returncode=1, stdout="", stderr="chroma backend mismatch"
            )
            with (
                unittest.mock.patch.dict(os.environ, {"HOME": str(home)}),
                unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed) as run,
            ):
                first = retrieve.retrieve("guard history", store="mempalace", project=project)
                second = retrieve.retrieve("guard history", store="mempalace", project=project)
            self.assertEqual("backend-mismatch", first["reason"])
            self.assertEqual("degraded", first["status"])
            self.assertEqual("backend-mismatch", second["reason"])
            self.assertFalse(second.get("scheduled", True))
            self.assertEqual(1, run.call_count)
            self.assertNotIn("migrate", json.dumps(second))
            self.assertFalse((home / ".mempalace").exists())

    def test_instruction_markdown_needs_no_citation_and_shell_opens_use_the_ledger(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_shell")
        # #6219: the gated file is project code; chaos-engine/ is harness (#6174).
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            relative = "chaos-engine/references/eliminate-waste.md"
            absolute = str(project / relative)
            for target in (relative, absolute):
                self.assertIsNone(
                    gate.file_read_block_reason(
                        project=project,
                        event_name="PreToolUse",
                        tool_name="Read",
                        tool_input={"target_file": target},
                        commands=(),
                    )
                )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "src/hooks/guard.py"},
                    commands=(),
                )
            )
            denied = (
                "sed -n '1,20p' src/hooks/guard.py",
                'python3 - <<\'PY\'\nPath("src/hooks/guard.py").read_text()\nPY',
                'python3 -c \'print(open("src/hooks/guard.py").read())\'',
                'python3 -c \'import pathlib; pathlib.Path("src/hooks/guard.py").read_text()\'',
                'python3 -c \'open("src/hooks/guard.py").read()\' .chaos-engine/tool.py --help',
                "python3 - <<'PY'\nimport os; open(\"src/hooks/guard.py\").read()\nPY",
            )
            for command in denied:
                self.assertIsNotNone(
                    gate.file_read_block_reason(
                        project=project,
                        event_name="PreToolUse",
                        tool_name="Bash",
                        tool_input={},
                        commands=(command,),
                    ),
                    command,
                )
            allowed = (
                "sed -n '1,20p' chaos-engine/references/eliminate-waste.md",
                "python3 -m unittest tests.scripts.test_watch_pr_checks",
                "python3 .chaos-engine/tool.py retrieve --store graphify guard.py",
            )
            for command in allowed:
                self.assertIsNone(
                    gate.file_read_block_reason(
                        project=project,
                        event_name="PreToolUse",
                        tool_name="Bash",
                        tool_input={},
                        commands=(command,),
                    ),
                    command,
                )

    def test_commands_outside_the_project_are_not_exploratory_reads(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_outside")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("raise SystemExit(0)\n", encoding="utf-8")
            scratch = Path(tempfile.gettempdir()) / "autoclose-job"
            download = (
                f"curl -sS -L -o {scratch}.zip "
                "https://api.github.com/repos/ShaftHQ/SHAFT_ENGINE/actions/jobs/1/logs "
                f"&& unzip -o {scratch}.zip -d {scratch} "
                f"&& find {scratch} -type f | head"
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Bash",
                    tool_input={},
                    commands=(download,),
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": f"{scratch}.log"},
                    commands=(),
                )
            )
            outside = tempfile.TemporaryDirectory()
            self.addCleanup(outside.cleanup)
            checkout = Path(outside.name) / "worktree"
            (checkout / "chaos-engine").mkdir(parents=True)
            (checkout / "chaos-engine" / "install.py").write_text("raise SystemExit(0)\n", encoding="utf-8")
            feature = checkout / "src" / "Sample.feature"
            feature.parent.mkdir()
            feature.write_text("Feature: sample\n", encoding="utf-8")
            self.assertIsNotNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Bash",
                    tool_input={},
                    commands=(f"sed -n '1,5p' {feature}",),
                )
            )
            secret = project / "src" / "Foo.java"
            secret.parent.mkdir()
            secret.write_text("SECRET_PROJECT_BYTES\n", encoding="utf-8")
            for url in (f"file://{secret}", f"file://127.0.0.1{secret}", f"file://[::1]{secret}"):
                bypass = f"find {scratch} -exec curl -s {url} {{}} +"
                self.assertIsNotNone(
                    gate.file_read_block_reason(
                        project=project,
                        event_name="PreToolUse",
                        tool_name="Bash",
                        tool_input={},
                        commands=(bypass,),
                    ),
                    url,
                )
            harness = checkout / "chaos-engine" / "hooks" / "guard.py"
            harness.parent.mkdir(parents=True)
            harness.write_text("print('ok')\n", encoding="utf-8")
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Bash",
                    tool_input={},
                    commands=(f"sed -n '1,5p' {harness}",),
                )
            )

    def test_cheap_file_grep_never_owes_a_retrieve(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_cheap")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            target = project / "src" / "Foo.java"
            target.parent.mkdir(parents=True)
            target.write_text("class Foo {}\n", encoding="utf-8")
            scratch = Path(tempfile.gettempdir()) / "retrieve-cheap-scratch"
            scratch.mkdir(exist_ok=True)
            calls = (
                ("Grep", {"path": "src/Foo.java", "pattern": "class"}, ()),
                ("Read", {"target_file": "src/Foo.java"}, ()),
                ("Bash", {}, ("rg class src/Foo.java",)),
                ("Bash", {}, ("rg class chaos-engine/hooks/retrieve_justification.py",)),
                ("Bash", {}, (f"rg class {scratch}",)),
                ("Bash", {}, ("python3 -m unittest tests.scripts.test_watch_pr_checks",)),
            )
            for tool_name, tool_input, commands in calls:
                self.assertIsNone(
                    gate.file_read_block_reason(
                        project=project,
                        event_name="PreToolUse",
                        tool_name=tool_name,
                        tool_input=tool_input,
                        commands=commands,
                        session_id="cheap",
                    ),
                    (tool_name, tool_input, commands),
                )
            self.assertIsNone(gate.session_retrieve_gap(project, "cheap"))

    def test_broad_search_without_retrieve_fails_stop_and_clears_after_one_retrieve(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_stop")
        retrieve = load("chaos-engine/retrieve.py", "retrieve_stop")
        guard = load("chaos-engine/hooks/guard.py", "retrieve_stop_guard")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "src").mkdir()
            tool = project / ".chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("raise SystemExit(0)\n", encoding="utf-8")
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"path": "src", "pattern": "class"},
                    commands=(),
                    session_id="task",
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Bash",
                    tool_input={},
                    commands=("find src -name Foo.java", "rg -g '*.py' Foo"),
                    session_id="task",
                )
            )
            self.assertEqual(_OWED_RETRIEVE, gate.session_retrieve_gap(project, "task"))
            event = {
                "hook_event_name": "Stop",
                "cwd": str(project),
                "stop_hook_active": True,
            }
            self.assertEqual(_OWED_RETRIEVE, guard._stop_block_reason(event, "task"))
            completed = unittest.mock.Mock(returncode=0, stdout="", stderr="")
            with (
                unittest.mock.patch.dict(os.environ, {"CHAOS_ENGINE_SESSION_ID": "task"}),
                unittest.mock.patch.object(retrieve.subprocess, "run", return_value=completed),
            ):
                receipt = retrieve.retrieve("src callers", store="graphify", project=project)
            self.assertEqual("skipped", receipt["status"])
            self.assertIsNone(gate.session_retrieve_gap(project, "task"))
            self.assertNotEqual(_OWED_RETRIEVE, guard._stop_block_reason(event, "task"))

    def test_grep_with_glob_and_no_path_is_not_denied(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_glob")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Grep",
                    tool_input={"pattern": "foo", "glob": "*.py"},
                    commands=(),
                    session_id="glob",
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Glob",
                    tool_input={"glob_pattern": "**/*.py"},
                    commands=(),
                    session_id="glob",
                )
            )
            self.assertIsNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Bash",
                    tool_input={},
                    commands=("rg foo", "grep -n foo", "find . -name '*.py'"),
                    session_id="glob",
                )
            )
            self.assertEqual(_OWED_RETRIEVE, gate.session_retrieve_gap(project, "glob"))

    def test_policy_pins_allow_plus_owed_not_an_uncited_deny(self):
        retrieve_first = (ROOT / "chaos-engine/references/retrieve-first.md").read_text(encoding="utf-8")
        matrix = (ROOT / "chaos-engine/references/host-parity-matrix.md").read_text(encoding="utf-8")
        for text in (retrieve_first, matrix):
            self.assertIn(_ALLOW_OWED, text)
            self.assertNotIn("An uncited path stays denied", text)
        self.assertIn("One retrieve per task area, not per file.", retrieve_first)
        self.assertIn("retrieve: used|skipped(<reason>)|exempt(harness)", retrieve_first)


if __name__ == "__main__":
    unittest.main()
