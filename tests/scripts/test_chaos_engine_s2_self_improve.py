"""Wave S2 (#5658/#5659): significance-filtered capture + SkillOpt compress audit."""

from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CATALOG = ROOT / "chaos-engine/references/zero-llm-catalog.md"
SIG_DOC = ROOT / "chaos-engine/references/significance-capture.md"
COMPRESS_DOC = ROOT / "chaos-engine/references/skill-compress-audit.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class S2SelfImproveTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.significance = load(
            ROOT / "chaos-engine/significance.py", "ce_significance_s2"
        )
        cls.compress = load(
            ROOT / "chaos-engine/skill_compress_audit.py", "ce_skill_compress_s2"
        )
        cls.lifecycle = load(
            ROOT / "chaos-engine/hooks/lifecycle.py", "ce_lifecycle_s2"
        )
        cls.learn_session = load(
            ROOT / "chaos-engine/learning_session.py", "ce_learn_session_s2"
        )
        cls.guard = load(ROOT / "chaos-engine/hooks/guard.py", "ce_guard_s2")

    def test_filter_rejects_happy_path_accepts_fail_deny(self):
        self.assertFalse(
            self.significance.is_significant(failed=False, denied=False)
        )
        self.assertTrue(self.significance.is_significant(failed=True))
        self.assertTrue(self.significance.is_significant(denied=True))
        self.assertTrue(
            self.significance.is_significant(kind="correction", explicit=True)
        )
        self.assertFalse(
            self.significance.is_significant(kind="noise", explicit=True)
        )

    def test_soft_capture_writes_marks_not_every_edit(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            # Happy path → nothing
            self.assertIsNone(
                self.significance.soft_post_tool_capture(
                    {"session_id": "s2", "tool_name": "Edit"},
                    event_name="PostToolUse",
                    failed=False,
                    denied=False,
                    tool_name="Edit",
                    project=project,
                )
            )
            self.assertEqual(0, len(self.significance.list_marks(project)))
            # Fail → mark
            marked = self.significance.soft_post_tool_capture(
                {"session_id": "s2", "tool_name": "Bash"},
                event_name="PostToolUseFailure",
                failed=True,
                tool_name="Bash",
                project=project,
            )
            self.assertIsNotNone(marked)
            self.assertEqual("repeated-failure", marked["kind"])
            # Explicit correction
            corr = self.significance.record_mark(
                "correction",
                "Prefer silent verify on Check",
                session_id="s2",
                project=project,
                source="cli",
            )
            self.assertEqual("correction", corr["kind"])
            # Privacy reject
            self.assertIsNone(
                self.significance.record_mark(
                    "missing-skill",
                    "token=sk-abcdefghijklmnopqrstuvwxyz",
                    project=project,
                )
            )
            locator = self.significance.session_start_locator(project)
            self.assertIn("significance", locator)
            self.assertNotIn("Prefer silent verify", locator)
            drained = self.significance.drain_marks(project)
            self.assertGreaterEqual(len(drained), 2)
            self.assertEqual(0, len(self.significance.list_marks(project)))

    def test_session_start_locator_includes_significance_under_budget(self):
        context = self.lifecycle.session_start_context("tok", "activation")
        self.assertIn("Significance:", context)
        self.assertIn("no Observer", context)
        self.assertLessEqual(
            len(context.encode("utf-8")), self.lifecycle.SESSION_START_MAX_BYTES
        )

    def test_learning_session_drains_significance_marks(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            self.significance.record_mark(
                "missing-skill",
                "Need doctor recovery playbook pointer",
                session_id="s2-drain",
                project=project,
            )
            cwd = Path.cwd()
            try:
                os.chdir(project)
                # Re-record in cwd project root discovery
                self.significance.record_mark(
                    "doctor-recovery",
                    "Used repair --component core",
                    session_id="s2-drain",
                    project=Path.cwd(),
                )
                result = self.learn_session.finalize(
                    "s2-drain", disposition="issues-first", extract_heuristics=False
                )
            finally:
                os.chdir(cwd)
            self.assertGreaterEqual(result.get("significanceDrained", 0), 1)
            self.assertIn("doctor-recovery", result.get("significanceKinds", []))

    def test_guard_soft_helpers_exist_without_observer(self):
        self.assertTrue(callable(self.guard._soft_significance_capture))
        self.assertTrue(callable(self.guard._record_denial_with_significance))
        # Soft helper must not raise / must not inject when given empty fail=False
        self.guard._soft_significance_capture({}, "PostToolUse", failed=False)

    def test_skill_compress_audit_propose_only_no_mutate(self):
        skills = self.compress.discover_skills(ROOT)
        self.assertGreaterEqual(len(skills), 1)
        before = {str(path): path.read_bytes() for path in skills}
        report = self.compress.audit_tree(ROOT, include_diff=True)
        after = {str(path): path.read_bytes() for path in skills}
        self.compress.assert_no_mutate(before, after)
        self.assertEqual("skill-compress-audit", report["kind"])
        self.assertFalse(report["mutate"])
        self.assertIn("propose-only", report["policy"])
        self.assertIn("#5665/#8", report["applyGate"])
        self.assertEqual(self.compress.L2_LINE_BUDGET, 500)
        # self-improve should be under budget
        by_name = {item["name"]: item for item in report["skills"]}
        self.assertIn("self-improve", by_name)
        self.assertFalse(by_name["self-improve"]["score"]["overBudget"])
        # Filler detection on synthetic body
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            skill = project / "chaos-engine" / "skills" / "demo"
            skill.mkdir(parents=True)
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            body = "---\nname: demo\ndescription: demo\n---\n\n"
            body += "Note that this is important.\n\n\n\n"
            body += "It is important to remember filler.\n"
            body += "```\n" + ("x\n" * 30) + "```\n"
            # pad toward budget
            body += "\n".join(f"line {i}" for i in range(20)) + "\n"
            target = skill / "SKILL.md"
            target.write_text(body, encoding="utf-8")
            snap = target.read_bytes()
            audited = self.compress.audit_skill(target)
            self.assertGreaterEqual(audited["score"]["fillerHits"], 1)
            self.assertTrue(audited["proposals"])
            self.assertFalse(audited["mutate"])
            stub = self.compress.propose_diff_stub(audited)
            self.assertIn("PROPOSE ONLY", stub)
            self.assertEqual(snap, target.read_bytes())

    def test_skill_compress_cli_and_docs(self):
        self.assertTrue(SIG_DOC.is_file())
        self.assertTrue(COMPRESS_DOC.is_file())
        catalog = CATALOG.read_text(encoding="utf-8")
        self.assertIn("significance.py", catalog)
        self.assertIn("skill_compress_audit.py", catalog)
        self.assertIn("Task Observer", catalog)
        completed = subprocess.run(  # nosec B603
            [
                sys.executable,
                str(ROOT / "chaos-engine/skill_compress_audit.py"),
                "audit",
                "--skill",
                "self-improve",
                "--project",
                str(ROOT),
            ],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, completed.returncode, msg=completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertEqual(1, payload["skillCount"])
        self.assertFalse(payload["mutate"])
        # significance CLI
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            completed = subprocess.run(  # nosec B603
                [
                    sys.executable,
                    str(ROOT / "chaos-engine/significance.py"),
                    "mark",
                    "--kind",
                    "correction",
                    "--note",
                    "Prefer CLI over MCP",
                    "--project",
                    str(project),
                ],
                cwd=ROOT,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, completed.returncode, msg=completed.stderr)
            item = json.loads(completed.stdout)
            self.assertEqual("correction", item["kind"])


if __name__ == "__main__":
    unittest.main()
