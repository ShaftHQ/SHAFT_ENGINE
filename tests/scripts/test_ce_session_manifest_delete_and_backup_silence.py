"""Session created-paths HMAC/containment + CE restore + backup-remote silence."""
from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - fixed git argv only in fixtures.
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
DG = ROOT / "chaos-engine" / "delete_guard.py"
# /workspace is not on DEFAULT_ALLOW (unlike tempfile.gettempdir()).
WORKSPACE_SCRATCH = Path("/workspace/ce-cleanup-gate-scratch")


def load():
    spec = importlib.util.spec_from_file_location("ce_dg_session", DG)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod


def _git_repo(base: Path) -> Path:
    git = shutil.which("git")
    assert git is not None
    subprocess.run([git, "init", "-q"], cwd=base, check=True, capture_output=True)  # nosec B603
    subprocess.run([git, "config", "user.email", "t@t"], cwd=base, check=True, capture_output=True)  # nosec B603
    subprocess.run([git, "config", "user.name", "t"], cwd=base, check=True, capture_output=True)  # nosec B603
    (base / "README").write_text("x\n", encoding="utf-8")
    subprocess.run([git, "add", "README"], cwd=base, check=True, capture_output=True)  # nosec B603
    subprocess.run([git, "commit", "-qm", "i"], cwd=base, check=True, capture_output=True)  # nosec B603
    return base


class SessionManifestDeleteTest(unittest.TestCase):
    def setUp(self) -> None:
        WORKSPACE_SCRATCH.mkdir(parents=True, exist_ok=True)
        self._dirs: list[Path] = []
        self._env = mock.patch.dict(os.environ, {}, clear=False)
        self._env.start()
        self.dg = load()
        # Isolate secrets under scratch home.
        self.home = self._scratch() / "home"
        self.home.mkdir()
        os.environ["HOME"] = str(self.home)
        os.environ["XDG_CONFIG_HOME"] = str(self.home / "config")
        os.environ[self.dg.SESSION_MANIFEST_SECRET_ENV] = "unit-test-session-secret-v1"

    def tearDown(self) -> None:
        self._env.stop()
        for d in self._dirs:
            shutil.rmtree(d, ignore_errors=True)

    def _scratch(self) -> Path:
        base = WORKSPACE_SCRATCH / f"case-{os.getpid()}-{len(self._dirs)}"
        base.mkdir(parents=True)
        self._dirs.append(base)
        return base

    def test_manifest_path_allowed_without_code(self) -> None:
        dg = self.dg
        base = self._scratch()
        scoped = base / "scoped"
        scoped.mkdir()
        target = scoped / "session-log.txt"
        target.write_text("log\n", encoding="utf-8")
        other = scoped / "other.txt"
        other.write_text("x\n", encoding="utf-8")
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        os.environ[dg.SESSION_CONTAINMENT_ROOTS_ENV] = str(scoped)
        dg.record_session_created_paths([target], manifest=manifest, roots=[scoped])
        self.assertTrue(dg.session_created_allows(target, manifest=manifest))
        self.assertEqual("", dg.path_reason(target))
        reason = dg.path_reason(other)
        self.assertNotEqual("", reason)

    def test_forged_manifest_rejected(self) -> None:
        dg = self.dg
        base = self._scratch()
        scoped = base / "scoped"
        scoped.mkdir()
        victim = scoped / "legit.txt"
        victim.write_text("ok\n", encoding="utf-8")
        outside = base / "outside.txt"
        outside.write_text("secret\n", encoding="utf-8")
        manifest = scoped / "created-paths.json"
        # Forged payload listing an outside path with a fake hmac.
        forged = {
            "schema": 2,
            "roots": [str(scoped)],
            "paths": [str(outside.resolve()), str(victim.resolve())],
            "hmac": "0" * 64,
        }
        manifest.write_text(json.dumps(forged), encoding="utf-8")
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        self.assertEqual(set(), dg.load_session_created_paths(manifest))
        self.assertFalse(dg.session_created_allows(outside, manifest=manifest))
        self.assertFalse(dg.session_created_allows(victim, manifest=manifest))
        reason, _ = dg.check(f"rm -f -- {outside}", cwd=base)
        self.assertTrue(reason)

    def test_absolute_outside_containment_refused_at_record(self) -> None:
        dg = self.dg
        base = self._scratch()
        scoped = base / "scoped"
        scoped.mkdir()
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        hosts = Path("/etc/hosts")
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([hosts], manifest=manifest, roots=[scoped])
        self.assertIn("containment", str(ctx.exception).casefold())

    def test_symlink_refused_at_record_and_allow(self) -> None:
        dg = self.dg
        base = self._scratch()
        scoped = base / "scoped"
        scoped.mkdir()
        real = scoped / "real.txt"
        real.write_text("x\n", encoding="utf-8")
        link = scoped / "link.txt"
        link.symlink_to(real)
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([link], manifest=manifest, roots=[scoped])
        self.assertIn("symlink", str(ctx.exception).casefold())

    def test_never_touch_still_wins_via_path_reason(self) -> None:
        dg = self.dg
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            yt = base / "yt" / "clip.mp4"
            yt.parent.mkdir(parents=True)
            yt.write_text("x", encoding="utf-8")
            manifest = base / "created-paths.json"
            os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
            os.environ[dg.SESSION_CONTAINMENT_ROOTS_ENV] = str(base)
            with self.assertRaises(PermissionError):
                dg.record_session_created_paths([yt], manifest=manifest, roots=[base])
            # Even a hand-written allow cannot bypass never-touch in path_reason.
            os.environ[dg.SESSION_MANIFEST_SECRET_ENV] = "unit-test-session-secret-v1"
            # force a verified-looking entry by recording a sibling then forging is impossible;
            # assert path_reason still blocks yt.
            self.assertTrue(dg.never_touch(yt))
            self.assertIn("never-touch", dg.path_reason(yt))
            reason, _ = dg.check(f"rm -rf -- {yt}", cwd=base)
            self.assertTrue(reason)
            self.assertIn("never-touch", reason)

    def test_creds_like_never_touch(self) -> None:
        dg = self.dg
        base = self._scratch()
        scoped = base / "scoped"
        ssh = scoped / ".ssh" / "id_rsa"
        ssh.parent.mkdir(parents=True)
        ssh.write_text("KEY\n", encoding="utf-8")
        self.assertTrue(dg.never_touch(ssh))
        self.assertIn("never-touch", dg.path_reason(ssh))

    def test_refusal_is_final_block_reason_matches_check(self) -> None:
        dg = self.dg
        base = self._scratch()
        _git_repo(base)
        secret = base / "only-copy.txt"
        secret.write_text("unique\n", encoding="utf-8")
        cmd = f"rm -f -- {secret}"
        via_check = dg.check(cmd, cwd=base)[0]
        via_block = dg.block_reason(cmd, cwd=base)
        self.assertEqual(via_check, via_block)
        self.assertTrue(via_block, via_block)
        doc = DG.read_text(encoding="utf-8")
        self.assertIn("refusal is final", doc)
        lesson = (
            ROOT
            / "chaos-engine/addons/design-skills/references/lessons/delete-check-refusal-final.md"
        )
        self.assertTrue(lesson.is_file())
        body = lesson.read_text(encoding="utf-8")
        self.assertIn("final", body.casefold())
        self.assertIn("raw", body.casefold())


class CeRestoreTest(unittest.TestCase):
    def setUp(self) -> None:
        WORKSPACE_SCRATCH.mkdir(parents=True, exist_ok=True)
        self._dirs: list[Path] = []
        self.dg = load()

    def tearDown(self) -> None:
        for d in self._dirs:
            shutil.rmtree(d, ignore_errors=True)

    def _scratch(self) -> Path:
        base = WORKSPACE_SCRATCH / f"ce-{os.getpid()}-{len(self._dirs)}"
        base.mkdir(parents=True)
        self._dirs.append(base)
        return base

    def test_restore_checkouts_not_deletes_and_refuses_other_worktree(self) -> None:
        dg = self.dg
        git = shutil.which("git")
        assert git is not None
        base = self._scratch()
        primary = base / "primary"
        other = base / "other-wt"
        primary.mkdir()
        _git_repo(primary)
        # CE file committed on primary.
        ce = primary / ".chaos-engine"
        ce.mkdir()
        tool = ce / "tool.py"
        tool.write_text("primary-v1\n", encoding="utf-8")
        subprocess.run([git, "add", ".chaos-engine/tool.py"], cwd=primary, check=True, capture_output=True)  # nosec B603
        subprocess.run([git, "commit", "-qm", "ce"], cwd=primary, check=True, capture_output=True)  # nosec B603
        sha = subprocess.run(  # nosec B603
            [git, "rev-parse", "HEAD"], cwd=primary, check=True, capture_output=True, text=True
        ).stdout.strip()
        # Linked worktree for session.
        session = base / "session"
        subprocess.run(  # nosec B603
            [git, "-C", str(primary), "worktree", "add", "-q", str(session)],
            check=True, capture_output=True,
        )
        # Dirty the session CE file.
        session_tool = session / ".chaos-engine" / "tool.py"
        session_tool.write_text("session-dirty\n", encoding="utf-8")
        os.environ["CHAOS_ENGINE_PRIMARY"] = str(primary)
        # Delete-check must still refuse (no free delete).
        reason, _ = dg.check(f"rm -f -- {session_tool}", cwd=session)
        self.assertTrue(reason, reason)
        self.assertFalse(dg.ce_managed_revert_allows(session_tool, primary=primary))
        # Restore brings primary SHA content back.
        refusal, restored = dg.restore_ce_managed_to_primary(
            [session_tool], session=session, primary=primary
        )
        self.assertEqual("", refusal, refusal)
        self.assertEqual([".chaos-engine/tool.py"], restored)
        self.assertEqual("primary-v1\n", session_tool.read_text(encoding="utf-8"))
        # Other worktree path refused.
        subprocess.run(  # nosec B603
            [git, "-C", str(primary), "worktree", "add", "-q", str(other)],
            check=True, capture_output=True,
        )
        other_tool = other / ".chaos-engine" / "tool.py"
        other_tool.write_text("other-dirty\n", encoding="utf-8")
        refusal2, _ = dg.restore_ce_managed_to_primary(
            [other_tool], session=session, primary=primary
        )
        self.assertTrue(refusal2)
        self.assertIn("outside session", refusal2)
        # And delete-check still refuses the other worktree CE file.
        reason2, _ = dg.check(f"rm -f -- {other_tool}", cwd=other)
        self.assertTrue(reason2)
        self.assertEqual(sha, sha)  # keep sha used for clarity / lint


class BackupRemoteSilenceTest(unittest.TestCase):
    def test_consumer_mode_still_warns_7263(self) -> None:
        """#7263 contract: consumer mode hides harness state → warn, not silence."""
        dg = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            marker = project / dg.CONSUMER_MARKER
            marker.parent.mkdir(parents=True)
            marker.write_text("consumer\n", encoding="utf-8")
            _git_repo(project)
            self.assertEqual(
                "consumer mode hides harness state from git",
                dg.backup_remote_issue(project),
            )

    def test_only_ce_dirt_helper(self) -> None:
        dg = load()
        self.assertTrue(
            dg._only_chaos_engine_dirt(" M .chaos-engine/tool.py\n?? .chaos-engine/VERSION\n")
        )
        self.assertFalse(
            dg._only_chaos_engine_dirt(" M README.md\n M .chaos-engine/tool.py\n")
        )


class L45LessonTest(unittest.TestCase):
    def test_card_and_inventory(self) -> None:
        inv = (ROOT / "chaos-engine/addons/design-skills/INVENTORY.md").read_text(encoding="utf-8")
        collapsed = " ".join(inv.split())
        self.assertIn("L01-L45", collapsed)
        self.assertIn("45 lesson", collapsed)
        idx = (ROOT / "chaos-engine/addons/design-skills/references/lessons/index.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("delete-check-refusal-final.md", idx)


if __name__ == "__main__":
    unittest.main()
