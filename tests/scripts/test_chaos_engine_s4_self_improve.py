"""Wave S4 (#5664/#5665): periodic meta-optimize + opt-in eval-gated draft skill PRs."""

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
LEVEL1 = ROOT / "chaos-engine/references/level-1-catalog.md"
META_DOC = ROOT / "chaos-engine/references/meta-optimize.md"
DRAFT_DOC = ROOT / "chaos-engine/references/draft-skill-pr.md"
COMPRESS_DOC = ROOT / "chaos-engine/references/skill-compress-audit.md"
AGENTS = ROOT / "AGENTS.md"
CE_SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
SI_SKILL = ROOT / "chaos-engine/skills/self-improve/SKILL.md"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class S4SelfImproveTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.meta = load(ROOT / "chaos-engine/meta_optimize.py", "ce_meta_optimize_s4")
        cls.draft = load(ROOT / "chaos-engine/draft_skill_pr.py", "ce_draft_skill_pr_s4")
        cls.significance = load(
            ROOT / "chaos-engine/significance.py", "ce_significance_s4"
        )
        cls.compress = load(
            ROOT / "chaos-engine/skill_compress_audit.py", "ce_skill_compress_s4"
        )

    def test_meta_optimize_review_aggregates_and_stays_offline(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            self.significance.record_mark(
                "correction",
                "Prefer meta-optimize periodic cadence",
                session_id="s4",
                project=project,
                source="cli",
            )
            # Seed a compressible skill for proposals
            skill = project / "chaos-engine" / "skills" / "demo"
            skill.mkdir(parents=True)
            body = "---\nname: demo\ndescription: demo\n---\n\n"
            body += "Note that this is important.\nIt is important to remember filler.\n"
            body += "\n".join(f"line {i}" for i in range(10)) + "\n"
            (skill / "SKILL.md").write_text(body, encoding="utf-8")

            document = self.meta.review(project)
            self.assertEqual("meta-optimize-review", document["kind"])
            self.assertFalse(document["continuous"])
            self.assertFalse(document["sessionStart"])
            self.assertFalse(document["taskObserver"])
            self.assertFalse(document["mutate"])
            self.assertFalse(document["autoMerge"])
            self.assertIn("periodic-offline", document["cadence"])
            self.assertGreaterEqual(document["significance"]["count"], 1)
            self.assertIn("learningMetrics", document)
            self.assertFalse(document["skillCompress"]["mutate"])
            self.assertIsInstance(document["issueCandidates"], list)
            path = self.meta.write_review(document, project)
            self.assertTrue(path.is_file())
            cadence = self.meta.cadence_text().lower()
            self.assertIn("never", cadence)
            self.assertIn("sessionstart", cadence)
            self.assertIn("task observer", cadence)

    def test_meta_optimize_cli(self):
        completed = subprocess.run(  # nosec B603
            [
                sys.executable,
                str(ROOT / "chaos-engine/meta_optimize.py"),
                "cadence",
            ],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, completed.returncode, msg=completed.stderr)
        self.assertIn("periodic", completed.stdout.lower())
        completed = subprocess.run(  # nosec B603
            [
                sys.executable,
                str(ROOT / "chaos-engine/meta_optimize.py"),
                "review",
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
        self.assertEqual("meta-optimize-review", payload["kind"])
        self.assertFalse(payload["continuous"])

    def test_draft_skill_pr_default_off(self):
        status = self.draft.gate_status(flag=False)
        self.assertFalse(status["enabled"])
        self.assertEqual("OFF", status["default"])
        self.assertFalse(status["autoMerge"])
        self.assertFalse(status["autoApply"])
        self.assertFalse(self.draft.opt_in_enabled(flag=True, environ={}))
        self.assertFalse(
            self.draft.opt_in_enabled(
                flag=False, environ={self.draft.OPT_IN_ENV: "1"}
            )
        )
        self.assertTrue(
            self.draft.opt_in_enabled(
                flag=True, environ={self.draft.OPT_IN_ENV: "1"}
            )
        )

    def test_draft_open_blocked_without_opt_in(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            skill = project / "chaos-engine" / "skills" / "demo"
            skill.mkdir(parents=True)
            (skill / "SKILL.md").write_text(
                "---\nname: demo\ndescription: d\n---\n\nNote that filler.\n",
                encoding="utf-8",
            )
            before = (skill / "SKILL.md").read_bytes()
            result = self.draft.open_draft_pr(
                project,
                flag=False,
                dry_run=True,
                environ={},
                skip_tests=True,
            )
            self.assertFalse(result["ok"])
            self.assertIn("opt-in", (result.get("blocked") or "").lower())
            self.assertFalse(result["created"])
            self.assertFalse(result["autoMerge"])
            self.assertFalse(result["applied"])
            self.assertEqual(before, (skill / "SKILL.md").read_bytes())

    def test_draft_open_dry_run_with_opt_in_no_mutate_no_gh(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "chaos-engine").mkdir()
            (project / "chaos-engine" / "install.py").write_text("# stub\n", encoding="utf-8")
            skill = project / "chaos-engine" / "skills" / "demo"
            skill.mkdir(parents=True)
            target = skill / "SKILL.md"
            target.write_text(
                "---\nname: demo\ndescription: d\n---\n\n"
                "Note that this is important.\nIt is important to remember.\n",
                encoding="utf-8",
            )
            before = target.read_bytes()
            calls: list[dict] = []

            def fake_gh(root, *, title, body):
                calls.append({"title": title, "body": body})
                return {"ok": True, "stdout": "https://example.test/pr/1", "autoMerge": False}

            result = self.draft.open_draft_pr(
                project,
                flag=True,
                dry_run=True,
                environ={self.draft.OPT_IN_ENV: "1"},
                skip_tests=True,
                gh_runner=fake_gh,
            )
            self.assertTrue(result["ok"])
            self.assertTrue(result["dryRun"])
            self.assertFalse(result["created"])
            self.assertFalse(result["autoMerge"])
            self.assertFalse(result["applied"])
            self.assertFalse(result["mutate"])
            self.assertEqual([], calls)  # dry-run must not call gh
            self.assertEqual(before, target.read_bytes())
            self.assertIn("wouldCreate", result)
            self.assertTrue(result["wouldCreate"]["draft"])
            self.assertFalse(result["wouldCreate"]["autoMerge"])

            # execute path uses gh_runner but still never mutates skills
            result2 = self.draft.open_draft_pr(
                project,
                flag=True,
                dry_run=False,
                environ={self.draft.OPT_IN_ENV: "1"},
                skip_tests=True,
                gh_runner=fake_gh,
            )
            self.assertTrue(result2["ok"])
            self.assertTrue(result2["created"])
            self.assertEqual(1, len(calls))
            self.assertFalse(result2["autoMerge"])
            self.assertEqual(before, target.read_bytes())

    def test_prepare_artifact_no_mutate(self):
        skills = self.compress.discover_skills(ROOT)
        before = {str(path): path.read_bytes() for path in skills}
        document = self.draft.prepare_artifact(ROOT, skill="self-improve")
        after = {str(path): path.read_bytes() for path in skills}
        self.compress.assert_no_mutate(before, after)
        self.assertEqual("draft-skill-pr-artifact", document["kind"])
        self.assertFalse(document["mutate"])
        self.assertFalse(document["applied"])
        self.assertFalse(document["pr"]["autoMerge"])
        self.assertTrue(document["pr"]["draft"])
        self.assertTrue(Path(document["artifact"]).is_file())

    def test_docs_and_overlay_wiring(self):
        for path in (META_DOC, DRAFT_DOC, COMPRESS_DOC, CATALOG, LEVEL1, AGENTS, CE_SKILL, SI_SKILL):
            self.assertTrue(path.is_file(), msg=str(path))
        meta = META_DOC.read_text(encoding="utf-8")
        for needle in (
            "Task Observer",
            "SessionStart",
            "periodic",
            "meta_optimize.py",
            "Reject",
        ):
            self.assertIn(needle, meta)
        self.assertIn("never", meta.lower())
        self.assertIn("sessionstart", meta.lower())

        draft = DRAFT_DOC.read_text(encoding="utf-8")
        for needle in (
            "CHAOS_ENGINE_DRAFT_SKILL_PRS",
            "Default OFF",
            "eval-parity",
            "all hosts",
        ):
            self.assertIn(needle, draft)
        self.assertIn("auto-merge", draft.lower())
        self.assertIn("never", draft.lower())

        catalog = CATALOG.read_text(encoding="utf-8")
        self.assertIn("meta_optimize.py", catalog)
        self.assertIn("draft_skill_pr.py", catalog)

        level1 = LEVEL1.read_text(encoding="utf-8")
        self.assertIn("meta-optimize.md", level1)
        self.assertIn("draft-skill-pr.md", level1)

        agents = AGENTS.read_text(encoding="utf-8")
        self.assertIn("meta-optimize.md", agents)
        self.assertIn("draft-skill-pr.md", agents)

        ce = CE_SKILL.read_text(encoding="utf-8")
        self.assertIn("meta-optimize.md", ce)
        self.assertIn("draft-skill-pr.md", ce)

        si = SI_SKILL.read_text(encoding="utf-8")
        self.assertIn("meta_optimize.py", si)
        self.assertIn("draft_skill_pr.py", si)

        compress = COMPRESS_DOC.read_text(encoding="utf-8")
        self.assertIn("draft_skill_pr.py", compress)
        self.assertIn("CHAOS_ENGINE_DRAFT_SKILL_PRS", compress)

    def test_cli_status_default_off(self):
        completed = subprocess.run(  # nosec B603
            [sys.executable, str(ROOT / "chaos-engine/draft_skill_pr.py"), "status"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=False,
            env={k: v for k, v in os.environ.items() if k != "CHAOS_ENGINE_DRAFT_SKILL_PRS"},
        )
        self.assertEqual(0, completed.returncode, msg=completed.stderr)
        payload = json.loads(completed.stdout)
        self.assertFalse(payload["enabled"])
        self.assertEqual("OFF", payload["default"])
        self.assertFalse(payload["autoMerge"])


if __name__ == "__main__":
    unittest.main()
