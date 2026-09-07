"""Wave C (#5623/#5624/#5625): phase ledger, retrieve, wake pack, portable learning."""

from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
PHASE = ROOT / "chaos-engine/phase_ledger.py"
WAKE = ROOT / "chaos-engine/wake_pack.py"
RETRIEVE = ROOT / "chaos-engine/retrieve.py"
LEARN = ROOT / "chaos-engine/learning_session.py"
GUARD = ROOT / "chaos-engine/hooks/guard.py"
LIFECYCLE = ROOT / "chaos-engine/hooks/lifecycle.py"
TOOL = ROOT / "chaos-engine/tool.py"
GATES = ROOT / "chaos-engine/references/delivery-phase-gates.md"
RETRIEVE_DOC = ROOT / "chaos-engine/references/retrieve-first.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class WaveCLedgerRetrieveLearningTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.phase = load(PHASE, "ce_phase_ledger_wave_c")
        cls.wake = load(WAKE, "ce_wake_pack_wave_c")
        cls.retrieve = load(RETRIEVE, "ce_retrieve_wave_c")
        cls.learn = load(LEARN, "ce_learning_session_wave_c")
        cls.lifecycle = load(LIFECYCLE, "ce_lifecycle_wave_c")
        cls.guard = load(GUARD, "ce_guard_wave_c")

    def test_research_required_only_for_public_contract(self):
        self.assertTrue(self.phase.research_required_for_triage("public-contract"))
        self.assertTrue(self.phase.research_required_for_triage("hard-to-reverse"))
        self.assertFalse(self.phase.research_required_for_triage("one-file"))
        self.assertFalse(self.phase.research_required_for_triage("one-module"))
        self.assertFalse(self.phase.research_required_for_triage(None))

    def test_phase_ledger_round_trip(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            entry = self.phase.record_phase(
                "sess-wave-c",
                "triage",
                triage="public-contract",
                note="test",
                project=project,
            )
            self.assertEqual("public-contract", entry["triage"])
            self.assertEqual(
                "public-contract", self.phase.session_triage("sess-wave-c", project)
            )
            summary = self.phase.doctor_phase_ledger_summary(project)
            self.assertEqual("healthy", summary["status"])
            self.assertGreaterEqual(summary["sessions"], 1)

    def test_guard_hard_soft_research_gate(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            self.phase.record_phase(
                "hard-sess", "triage", triage="public-contract", project=project
            )
            self.phase.record_phase(
                "soft-sess", "triage", triage="one-file", project=project
            )
            # Point guard ledger lookup at this project via cwd.
            triage_map = {
                "hard-sess": "public-contract",
                "soft-sess": "one-file",
            }

            def _triage_for(sid: str, mapping=triage_map):
                return mapping.get(sid)

            with mock.patch.object(self.guard, "_phase_ledger_triage", side_effect=_triage_for):
                hard = self.guard._research_before_mutation_reason(
                    "PreToolUse", True, "hard-sess"
                )
                self.assertIsNotNone(hard)
                self.assertIn("Research receipt required", hard)
                with mock.patch.dict(
                    os.environ, {"CHAOS_ENGINE_ENFORCE_RESEARCH_RECEIPT": "1"}
                ):
                    soft = self.guard._research_before_mutation_reason(
                        "PreToolUse", True, "soft-sess"
                    )
                    self.assertIsNone(soft)

    def test_wake_pack_owner_only_and_locator(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            written = self.wake.write_wake_pack(
                "L0: doctor first. L1: repair heal.",
                project=project,
                source="owner",
            )
            self.assertEqual("written", written["status"])
            drafted = self.wake.write_wake_pack(
                "draft from mempalace should not overwrite",
                project=project,
                source="mempalace",
            )
            self.assertEqual("drafted", drafted["status"])
            self.assertEqual(
                "L0: doctor first. L1: repair heal.",
                self.wake.read_wake_pack(project),
            )
            locator = self.wake.session_start_locator(project)
            self.assertIn("wake-pack.md", locator)
            self.assertNotIn("doctor first", locator)

    def test_session_start_bans_memory_prose_includes_wake_locator(self):
        context = self.lifecycle.session_start_context("tok", "activation")
        self.assertLessEqual(
            len(context.encode("utf-8")), self.lifecycle.SESSION_START_MAX_BYTES
        )
        self.assertIn("wake-pack.md", context)
        self.assertNotIn("memory remember", context.casefold())
        self.assertNotIn("MemPalace palace dump", context)

    def test_retrieve_dry_run_and_tool_dispatch(self):
        receipt = self.retrieve.retrieve("probe gotcha", dry_run=True)
        self.assertEqual("skipped", receipt["status"])
        self.assertEqual("memory", receipt["store"])
        graph = self.retrieve.retrieve("what calls FooBar?", dry_run=True)
        self.assertEqual("graphify", graph["store"])
        completed = subprocess.run(  # nosec B603
            [sys.executable, str(TOOL), "retrieve", "--dry-run", "has this bitten us?"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, completed.returncode, msg=completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual("retrieve-orchestrator", payload["kind"])
        self.assertIn(payload["status"], {"used", "skipped", "degraded"})

    def test_portable_learning_finalize_no_draft_prs(self):
        with tempfile.TemporaryDirectory() as temporary:
            cwd = Path.cwd()
            try:
                os.chdir(temporary)
                result = self.learn.finalize("wave-c-learn", disposition="issues-first")
            finally:
                os.chdir(cwd)
            self.assertFalse(result["draftPrs"])
            self.assertEqual("issues-first", result["disposition"])
            self.assertEqual("learning-session-portable-finalize", result["kind"])
            command = (
                "python3 .chaos-engine/learning_session.py finalize "
                "--session-id wave-c-learn"
            )
            self.assertTrue(self.guard.learning_session_finalize_command(command))
            mono = (
                "python3 scripts/agents/learning_session.py finalize "
                "--session-id wave-c-learn"
            )
            self.assertTrue(self.guard.learning_session_finalize_command(mono))

    def test_docs_mention_wave_c_surfaces(self):
        gates = GATES.read_text(encoding="utf-8")
        self.assertIn("Triage-scaled research gate", gates)
        self.assertIn("phase-ledger.json", gates)
        retrieve = RETRIEVE_DOC.read_text(encoding="utf-8")
        self.assertIn("tool.py retrieve", retrieve)
        self.assertIn("wake-pack.md", retrieve)


if __name__ == "__main__":
    unittest.main()
