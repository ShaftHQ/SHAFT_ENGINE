"""Retrieve-first gate allowlist, directory prefix, and fail-open (#6126 follow-up)."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


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
            self.assertIsNotNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": ".chaos-engine-state/install-console.log"},
                    commands=(),
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
            self.assertIsNotNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "tests/scripts/test_chaos_engine_installer.py"},
                    commands=(),
                )
            )

    def test_degraded_store_fails_open_only_for_paths_in_the_query(self):
        gate = load("chaos-engine/hooks/retrieve_justification.py", "gate_open")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            gate.record_store_outcome(
                project,
                "mempalace",
                "degraded",
                "chaos-engine/bootstrap.py chroma mismatch",
                "",
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
            self.assertIsNotNone(
                gate.file_read_block_reason(
                    project=project,
                    event_name="PreToolUse",
                    tool_name="Read",
                    tool_input={"target_file": "chaos-engine/hosts.py"},
                    commands=(),
                )
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


if __name__ == "__main__":
    unittest.main()
