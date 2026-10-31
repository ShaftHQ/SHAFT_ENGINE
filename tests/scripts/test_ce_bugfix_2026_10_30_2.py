"""CE bugfix batch 2026.10.30.2: install UX, identity, overlay reconcile, version."""
from __future__ import annotations

import argparse
import importlib.util
import shutil
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


class VersionTest(unittest.TestCase):
    def test_version_file_present(self) -> None:
        # Exact tip VERSION is owned by the newest bugfix batch; keep shape only.
        text = (CE / "VERSION").read_text(encoding="utf-8").strip()
        self.assertRegex(text, r"^2026\.\d{2}\.\d{2}\.\d+$")


class InstallSourceDefaultsTest(unittest.TestCase):
    def test_detect_source_and_commit(self) -> None:
        install = load("ce_install_bugfix2", CE / "install.py")
        source = install.detect_repo_chaos_engine_source(ROOT)
        self.assertIsNotNone(source)
        assert source is not None
        self.assertEqual("chaos-engine", source.name)
        self.assertTrue((source / "install.py").is_file())
        sha = install.detect_full_commit(ROOT)
        self.assertIsNotNone(sha)
        assert sha is not None
        self.assertRegex(sha, r"^[0-9a-f]{40}$")
        args = argparse.Namespace(project=ROOT, source=None, commit=None, from_bundle=None)
        install.apply_install_source_defaults(args)
        self.assertEqual(source, Path(args.source).resolve())
        self.assertEqual(sha, args.commit)

    def test_local_source_message_names_both_paths(self) -> None:
        install = load("ce_install_bugfix2_msg", CE / "install.py")
        msg = install._local_source_required_message()
        self.assertIn("--source <repo>/chaos-engine", msg)
        self.assertIn("chaos-engine/install.py", msg)
        self.assertIn(".chaos-engine/install.py", msg)

    def test_docs_show_source_commit_and_mempalace(self) -> None:
        raw = (CE / "INSTALL.md").read_text(encoding="utf-8")
        self.assertIn("--source ../ce-src/chaos-engine --commit", raw)
        self.assertIn("MemPalace", raw)
        self.assertIn(".chaos-engine-state/logs/", raw)
        self.assertIn("auto-detects", raw)


class ConsumerAdviceTest(unittest.TestCase):
    def test_advice_names_gitignore(self) -> None:
        consumer = load("ce_consumer_bugfix2", CE / "consumer_mode.py")
        advice = consumer.tracked_core_advice(3)
        self.assertIn("git rm --cached -r .chaos-engine", advice)
        self.assertIn(".gitignore", advice)


class CommitIdentityEntryTest(unittest.TestCase):
    def test_warns_when_local_ce_identity_outside_allowlist(self) -> None:
        ci = load("ce_ci_bugfix2", CE / "commit_identity.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            git = shutil.which("git")
            assert git is not None
            subprocess.run([git, "init", "-q"], cwd=root, check=True, capture_output=True)  # nosec B603
            subprocess.run(  # nosec B603
                [git, "config", "user.email", ci.CHAOSENGINE_EMAIL],
                cwd=root, check=True, capture_output=True,
            )
            subprocess.run(  # nosec B603
                [git, "config", "user.name", ci.CHAOSENGINE_NAME],
                cwd=root, check=True, capture_output=True,
            )
            (root / "README").write_text("x\n", encoding="utf-8")
            subprocess.run([git, "add", "README"], cwd=root, check=True, capture_output=True)  # nosec B603
            subprocess.run([git, "commit", "-qm", "i"], cwd=root, check=True, capture_output=True)  # nosec B603
            subprocess.run(  # nosec B603
                [git, "remote", "add", "origin", "https://github.com/MohabMohie/automatest-coach-brain.git"],
                cwd=root, check=True, capture_output=True,
            )
            warn = ci.entry_identity_warning(root)
            self.assertIn("commit-identity:", warn)
            self.assertIn("allowed only on", warn)
            with self.assertRaises(PermissionError):
                ci.assert_local_identity_matches_remote(root)

    def test_card_mentions_repo_local(self) -> None:
        raw = (CE / "references/commit-identity.md").read_text(encoding="utf-8")
        self.assertIn("repo-local", raw)
        self.assertIn("entry_identity_warning", raw)


class SessionLogsTest(unittest.TestCase):
    def test_log_path_under_state(self) -> None:
        logs = load("ce_session_logs_bugfix2", CE / "session_logs.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            path = logs.log_path("install", root)
            self.assertTrue(str(path).endswith(".chaos-engine-state/logs/install.log"))
            self.assertTrue(path.parent.is_dir())


class WorktreeReconcileTest(unittest.TestCase):
    def test_managed_paths_include_agents(self) -> None:
        overlay = load("ce_overlay_bugfix2", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            primary = Path(temporary) / "primary"
            (primary / ".chaos-engine").mkdir(parents=True)
            (primary / ".chaos-engine" / "VERSION").write_text("x\n", encoding="utf-8")
            agents = primary / ".claude" / "agents"
            agents.mkdir(parents=True)
            (agents / "chaos-engine-implementer.md").write_text("agent\n", encoding="utf-8")
            paths = overlay._managed_checkout_paths(primary)
            self.assertIn(".chaos-engine", paths)
            self.assertIn(".claude/agents", paths)

    def test_upgrade_session_checkouts_before_materialize(self) -> None:
        source = (CE / "worktree_overlay.py").read_text(encoding="utf-8")
        # upgrade_session must reconcile tracked + gitignored CE before materialize
        start = source.index("def upgrade_session")
        body = source[start : source.index("\ndef ", start + 1)]
        materialize_call = "materialize(primary, session, transactional=True)"
        self.assertIn(materialize_call, body)
        self.assertLess(
            body.index("_maybe_checkout_tracked_ce"),
            body.index(materialize_call),
        )
        self.assertLess(
            body.index("_adopt_ce_owned"),
            body.index(materialize_call),
        )


class MergeQueueLessonTest(unittest.TestCase):
    def test_docs_and_skill(self) -> None:
        eco = (CE / "references/ci-status-economy.md").read_text(encoding="utf-8")
        self.assertIn("AWAITING_CHECKS", eco)
        self.assertIn("merge_group", eco)
        skill = (CE / "skills/ci-cd/SKILL.md").read_text(encoding="utf-8")
        self.assertIn("AWAITING_CHECKS", skill)


class BotEntryPathTest(unittest.TestCase):
    def test_bot_entry_names_source_and_overlay(self) -> None:
        raw = (CE / "references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("chaos-engine/install.py", raw)
        self.assertIn(".chaos-engine/install.py", raw)
        self.assertIn(".chaos-engine-state/logs/", raw)


class AddonKeptMessageTest(unittest.TestCase):
    def test_message_is_project_local(self) -> None:
        raw = (CE / "install.py").read_text(encoding="utf-8")
        self.assertIn("project-local; set CHAOS_ENGINE_ADDON_PATHS", raw)
        docs = (CE / "references/addons.md").read_text(encoding="utf-8")
        self.assertIn("project-local", docs)




class IgnoredOverlayUpgradeTest(unittest.TestCase):
    def test_upgrade_session_reconciles_gitignored_stale_overlay(self) -> None:
        """HRSD: VERSION then implementer refuse when overlay is gitignored."""
        overlay = load("ce_overlay_bugfix2_hrsd", CE / "worktree_overlay.py")
        git = shutil.which("git")
        self.assertIsNotNone(git)
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            primary.mkdir()
            subprocess.run([git, "init", "-q"], cwd=primary, check=True, capture_output=True)  # nosec B603
            subprocess.run(  # nosec B603
                [git, "config", "user.email", "t@ex.com"], cwd=primary, check=True, capture_output=True,
            )
            subprocess.run(  # nosec B603
                [git, "config", "user.name", "T"], cwd=primary, check=True, capture_output=True,
            )
            (primary / ".gitignore").write_text(".chaos-engine/\n.claude/\n", encoding="utf-8")
            (primary / "README").write_text("p\n", encoding="utf-8")
            subprocess.run([git, "add", "README", ".gitignore"], cwd=primary, check=True, capture_output=True)  # nosec B603
            subprocess.run([git, "commit", "-qm", "i"], cwd=primary, check=True, capture_output=True)  # nosec B603
            session = base / "session"
            subprocess.run(  # nosec B603
                [git, "worktree", "add", "-q", str(session), "HEAD"],
                cwd=primary, check=True, capture_output=True,
            )

            def write_overlay(root: Path, version: str, implementer: str) -> None:
                ce = root / ".chaos-engine"
                (ce / "skills" / "chaos-engine").mkdir(parents=True)
                (ce / "hooks").mkdir(parents=True)
                (ce / "VERSION").write_text(version + "\n", encoding="utf-8")
                (ce / "skills" / "chaos-engine" / "SKILL.md").write_text("skill-" + version + "\n", encoding="utf-8")
                (ce / "hooks" / "guard.py").write_text("guard\n", encoding="utf-8")
                (ce / "always-loaded-budget.json").write_text('{"cap": 1}\n', encoding="utf-8")
                agents = root / ".claude" / "agents"
                agents.mkdir(parents=True)
                (agents / "chaos-engine-implementer.md").write_text(implementer + "\n", encoding="utf-8")

            write_overlay(primary, "2026.10.30.2", "implementer-NEW")
            write_overlay(session, "2026.10.30.1", "implementer-OLD")
            # No materialize manifest on session — the HRSD failure mode.
            result = overlay.upgrade_session(primary, session, repair_plugins=False)
            self.assertEqual("2026.10.30.2", (session / ".chaos-engine" / "VERSION").read_text(encoding="utf-8").strip())
            self.assertEqual(
                "implementer-NEW",
                (session / ".claude" / "agents" / "chaos-engine-implementer.md").read_text(encoding="utf-8").strip(),
            )
            self.assertGreaterEqual(len(result.get("written") or []), 1)


class SourceBehindConsumerTest(unittest.TestCase):
    def test_consumer_without_source_tree_does_not_claim_behind(self) -> None:
        install = load("ce_install_behind_consumer", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            git = shutil.which("git")
            self.assertIsNotNone(git)
            subprocess.run([git, "init", "-q"], cwd=root, check=True, capture_output=True)  # nosec B603
            subprocess.run(  # nosec B603
                [git, "config", "user.email", "c@ex.com"], cwd=root, check=True, capture_output=True,
            )
            subprocess.run(  # nosec B603
                [git, "config", "user.name", "C"], cwd=root, check=True, capture_output=True,
            )
            (root / "README").write_text("c\n", encoding="utf-8")
            subprocess.run([git, "add", "README"], cwd=root, check=True, capture_output=True)  # nosec B603
            subprocess.run([git, "commit", "-qm", "i"], cwd=root, check=True, capture_output=True)  # nosec B603
            fake = "f251bdd2bee8d13455cb2e48ff3eea80ca0dd148"
            install.installed_commit_of = lambda project: fake  # type: ignore[method-assign]
            tips = install.source_behind_advisory(root)
            self.assertEqual(("commit:", fake[:12]), tips)
            self.assertTrue(all("behind" not in part for part in tips))


class BoxDurabilityChecklistTest(unittest.TestCase):
    def test_bot_entry_lists_box_durability(self) -> None:
        raw = (CE / "references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("box-durability", raw)
        self.assertIn("free -h", raw)
        self.assertIn("/swapfile", raw)
        self.assertIn("swappiness", raw)
        self.assertIn("Only `/workspace` survives", raw)
        self.assertIn("Update Bot's Computer", raw)
        self.assertIn("go idle when asked", raw)


if __name__ == "__main__":
    unittest.main()
