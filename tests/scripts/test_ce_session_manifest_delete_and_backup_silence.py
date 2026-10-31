"""Session created-paths HMAC/containment + CE restore + backup-remote silence."""
from __future__ import annotations

import importlib.util
import json
import os
import shutil
import stat
import subprocess  # nosec B404 - fixed git argv only in fixtures.
import tempfile
import unittest
import unittest.mock
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DG = ROOT / "chaos-engine" / "delete_guard.py"


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
        self.dg = load()
        self._temp = tempfile.TemporaryDirectory()
        self.base = Path(self._temp.name).resolve()
        self.home = self.base / "home"
        self.home.mkdir()
        self._patches = [
            unittest.mock.patch.object(self.dg, "DEFAULT_ALLOW", ()),
            unittest.mock.patch.dict(
                os.environ,
                {
                    "HOME": str(self.home),
                    "XDG_CONFIG_HOME": str(self.home / "config"),
                    self.dg.SESSION_MANIFEST_HMAC_ENV: "unit-test-session-hmac-v1",
                },
                clear=False,
            ),
        ]
        for patch in self._patches:
            patch.start()

    def tearDown(self) -> None:
        for patch in reversed(self._patches):
            patch.stop()
        self._temp.cleanup()

    def _scratch(self) -> Path:
        path = self.base / f"case-{len(list(self.base.iterdir()))}"
        path.mkdir()
        return path

    def _init_manifest(self, scoped: Path, roots: list | None = None) -> Path:
        """Session-start inventory before creating recordable files."""
        dg = self.dg
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        os.environ.pop(dg.SESSION_CONTAINMENT_ROOTS_ENV, None)
        os.environ.pop(dg.SESSION_WORKTREE_ENV, None)
        root_list = roots if roots is not None else [scoped]
        dg.init_session_created_manifest(manifest=manifest, roots=root_list)
        return manifest

    def test_manifest_path_allowed_without_code(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        # Pre-existing sibling must not be recordable; init before creates.
        other = scoped / "other.txt"
        other.write_text("x\n", encoding="utf-8")
        manifest = self._init_manifest(scoped)
        target = scoped / "session-log.txt"
        target.write_text("log\n", encoding="utf-8")
        dg.record_session_created_paths([target], manifest=manifest, roots=[scoped])
        self.assertTrue(dg.session_created_allows(target, manifest=manifest))
        self.assertEqual("", dg.path_reason(target))
        reason = dg.path_reason(other)
        self.assertNotEqual("", reason)
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([other], manifest=manifest, roots=[scoped])
        self.assertIn("pre-existing", str(ctx.exception).casefold())

    def test_forged_manifest_rejected(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        victim = scoped / "legit.txt"
        victim.write_text("ok\n", encoding="utf-8")
        outside = self._scratch() / "outside.txt"
        outside.write_text("secret\n", encoding="utf-8")
        manifest = scoped / "created-paths.json"
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
        reason, _ = dg.check(f"rm -f -- {outside}", cwd=scoped)
        self.assertTrue(reason)

    def test_absolute_outside_containment_refused_at_record(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        hosts = Path("/etc/hosts")
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([hosts], manifest=manifest, roots=[scoped])
        self.assertIn("containment", str(ctx.exception).casefold())

    def test_symlink_refused_at_record_and_allow(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        manifest = self._init_manifest(scoped)
        real = scoped / "real.txt"
        real.write_text("x\n", encoding="utf-8")
        link = scoped / "link.txt"
        link.symlink_to(real)
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([link], manifest=manifest, roots=[scoped])
        self.assertIn("symlink", str(ctx.exception).casefold())

    def test_never_touch_still_wins_via_path_reason(self) -> None:
        dg = self.dg
        base = self._scratch()
        yt = base / "yt" / "clip.mp4"
        yt.parent.mkdir(parents=True)
        yt.write_text("x", encoding="utf-8")
        manifest = base / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        os.environ[dg.SESSION_CONTAINMENT_ROOTS_ENV] = str(base)
        with self.assertRaises(PermissionError):
            dg.record_session_created_paths([yt], manifest=manifest, roots=[base])
        self.assertTrue(dg.never_touch(yt))
        self.assertIn("never-touch", dg.path_reason(yt))
        reason, _ = dg.check(f"rm -rf -- {yt}", cwd=base)
        self.assertTrue(reason)
        self.assertIn("never-touch", reason)

    def test_creds_like_never_touch(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        ssh = scoped / ".ssh" / "id_rsa"
        ssh.parent.mkdir(parents=True)
        ssh.write_text("KEY\n", encoding="utf-8")
        self.assertTrue(dg.never_touch(ssh))
        self.assertIn("never-touch", dg.path_reason(ssh))

    def test_refusal_is_final_block_reason_matches_check(self) -> None:
        dg = self.dg
        base = self._scratch()
        _git_repo(base)
        only = base / "only-copy.txt"
        only.write_text("unique\n", encoding="utf-8")
        cmd = f"rm -f -- {only}"
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


    def test_env_containment_roots_slash_cannot_allow_etc_hosts(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        os.environ[dg.SESSION_CONTAINMENT_ROOTS_ENV] = "/"
        os.environ.pop(dg.SESSION_WORKTREE_ENV, None)
        hosts = Path("/etc/hosts")
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([hosts], manifest=manifest)
        self.assertIn("containment", str(ctx.exception).casefold())
        self.assertFalse(dg.session_created_allows(hosts, manifest=manifest))
        reason, _ = dg.check("rm -f -- /etc/hosts", cwd=scoped)
        self.assertTrue(reason)

    def test_env_worktree_slash_cannot_allow_etc_hosts(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        manifest = scoped / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        os.environ.pop(dg.SESSION_CONTAINMENT_ROOTS_ENV, None)
        os.environ[dg.SESSION_WORKTREE_ENV] = "/"
        hosts = Path("/etc/hosts")
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([hosts], manifest=manifest, roots=[scoped])
        self.assertIn("containment", str(ctx.exception).casefold())
        reason, _ = dg.check("rm -f -- /etc/hosts", cwd=scoped)
        self.assertTrue(reason)

    def test_stolen_key_resign_roots_slash_refuses_etc_hosts(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        # Drop env HMAC so a real XDG key is created (stealable).
        os.environ.pop(dg.SESSION_MANIFEST_HMAC_ENV, None)
        manifest = self._init_manifest(scoped)
        legit = scoped / "ok.txt"
        legit.write_text("ok\n", encoding="utf-8")
        dg.record_session_created_paths([legit], manifest=manifest, roots=[scoped])
        key_path = dg._session_hmac_key_path(manifest)
        self.assertTrue(key_path.is_file())
        hmac_key = key_path.read_bytes()
        hosts = str(Path("/etc/hosts").resolve())
        forged_roots = ["/"]
        forged_paths = [hosts]
        payload = {
            "schema": dg.SESSION_MANIFEST_SCHEMA,
            "roots": forged_roots,
            "paths": forged_paths,
            "hmac": dg._sign_manifest(hmac_key, forged_roots, forged_paths),
        }
        manifest.write_text(json.dumps(payload), encoding="utf-8")
        self.assertEqual(set(), dg.load_session_created_paths(manifest))
        self.assertFalse(dg.session_created_allows(Path("/etc/hosts"), manifest=manifest))
        reason, _ = dg.check("rm -f -- /etc/hosts", cwd=scoped)
        self.assertTrue(reason)

    def test_delete_time_rechecks_floor_after_env_expansion_attempt(self) -> None:
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        manifest = self._init_manifest(scoped)
        target = scoped / "session-log.txt"
        target.write_text("log\n", encoding="utf-8")
        dg.record_session_created_paths([target], manifest=manifest, roots=[scoped])
        self.assertTrue(dg.session_created_allows(target, manifest=manifest))
        # Attacker widens env after record; delete-time floor re-check still scopes.
        os.environ[dg.SESSION_CONTAINMENT_ROOTS_ENV] = "/"
        self.assertTrue(dg.session_created_allows(target, manifest=manifest))
        self.assertFalse(dg.session_created_allows(Path("/etc/hosts"), manifest=manifest))
        reason, _ = dg.check("rm -f -- /etc/hosts", cwd=scoped)
        self.assertTrue(reason)

    def test_D_worktree_hint_cannot_expand_floor_for_foreign_untracked(self) -> None:
        """B1-R2 exploit D: CHAOS_SESSION_WORKTREE must not add a foreign floor."""
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        foreign = self._scratch() / "foreign-wt"
        foreign.mkdir()
        _git_repo(foreign)
        victim = foreign / "preexist-untracked.txt"
        victim.write_text("preexist\n", encoding="utf-8")
        manifest = self._init_manifest(scoped)
        os.environ[dg.SESSION_WORKTREE_ENV] = str(foreign)
        floors = dg._floor_containment_roots(manifest)
        self.assertNotIn(foreign.resolve(), floors)
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([victim], manifest=manifest, roots=[scoped])
        self.assertIn("containment", str(ctx.exception).casefold())
        self.assertFalse(dg.session_created_allows(victim, manifest=manifest))
        reason, _ = dg.check(f"rm -f -- {victim}", cwd=scoped)
        self.assertTrue(reason, reason)

    def test_D_GIT_DIR_cannot_expand_floor_for_foreign_repo(self) -> None:
        """B1-R2 exploit D-GIT_DIR: scrubbed GIT_DIR/GIT_WORK_TREE cannot widen floors."""
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        foreign = self._scratch() / "foreign-gitdir"
        foreign.mkdir()
        _git_repo(foreign)
        victim = foreign / "README"
        manifest = self._init_manifest(scoped)
        with unittest.mock.patch.dict(
            os.environ,
            {
                "GIT_DIR": str(foreign / ".git"),
                "GIT_WORK_TREE": str(foreign),
            },
            clear=False,
        ):
            floors = dg._floor_containment_roots(manifest)
            self.assertNotIn(foreign.resolve(), [Path(p).resolve() for p in floors])
            with self.assertRaises(PermissionError) as ctx:
                dg.record_session_created_paths([victim], manifest=manifest, roots=[scoped])
            self.assertIn("containment", str(ctx.exception).casefold())
            self.assertFalse(dg.session_created_allows(victim, manifest=manifest))
            # Direct discover under env must still scrub inside _git.
            top = dg._discover_git_toplevel(hint=scoped)
            if top is not None:
                self.assertNotEqual(top.resolve(), foreign.resolve())

    def test_preexisting_untracked_launder_refused(self) -> None:
        """Record cannot launder a file that existed before session start."""
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        preexist = scoped / "already-here.txt"
        preexist.write_text("old\n", encoding="utf-8")
        manifest = self._init_manifest(scoped)
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([preexist], manifest=manifest, roots=[scoped])
        self.assertIn("pre-existing", str(ctx.exception).casefold())
        self.assertFalse(dg.session_created_allows(preexist, manifest=manifest))
        # path_reason must still hit git/outside refusal — not session short-circuit.
        reason = dg.path_reason(preexist)
        self.assertNotEqual("", reason)
        reason2, _ = dg.check(f"rm -f -- {preexist}", cwd=scoped)
        self.assertTrue(reason2, reason2)
        # Freshly created after init still records.
        fresh = scoped / "fresh.txt"
        fresh.write_text("new\n", encoding="utf-8")
        dg.record_session_created_paths([fresh], manifest=manifest, roots=[scoped])
        self.assertTrue(dg.session_created_allows(fresh, manifest=manifest))
        self.assertEqual("", dg.path_reason(fresh))


    def test_B1_R3_stolen_key_empty_preexist_forge_refused(self) -> None:
        """Stolen HMAC + started_at=0 + empty preexist must not launder (B1-R3)."""
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        victim = scoped / "preexist-victim.txt"
        victim.write_text("old\n", encoding="utf-8")
        # Drop env HMAC so a real XDG key is created (stealable).
        os.environ.pop(dg.SESSION_MANIFEST_HMAC_ENV, None)
        manifest = self._init_manifest(scoped)
        seal = dg._session_seal_path(manifest)
        self.assertTrue(seal.is_file(), "init must create write-once seal")
        self.assertEqual(stat.S_IMODE(seal.stat().st_mode), 0o444)
        # Honest record of preexist still refused.
        with self.assertRaises(PermissionError) as honest:
            dg.record_session_created_paths([victim], manifest=manifest, roots=[scoped])
        self.assertIn("pre-existing", str(honest.exception).casefold())
        # Steal key and re-sign emptied inventory + rewound started_at.
        key_path = dg._session_hmac_key_path(manifest)
        self.assertTrue(key_path.is_file())
        hmac_key = key_path.read_bytes()
        root_strs = [str(scoped.resolve())]
        forged = {
            "schema": dg.SESSION_MANIFEST_SCHEMA,
            "started_at": 0,
            "roots": root_strs,
            "paths": [],
            "preexist_paths": [],
            "preexist_inodes": [],
            "proofs": [],
            "hmac": dg._sign_manifest(
                hmac_key,
                root_strs,
                [],
                started_at="0",
                preexist_paths=[],
                preexist_inodes=[],
                proofs=[],
            ),
        }
        manifest.write_text(json.dumps(forged, indent=2), encoding="utf-8")
        # Seal still pins honest inventory — load/record/check fail closed.
        self.assertEqual(set(), dg.load_session_created_paths(manifest))
        with self.assertRaises(PermissionError) as ctx:
            dg.record_session_created_paths([victim], manifest=manifest, roots=[scoped])
        self.assertIn("seal", str(ctx.exception).casefold())
        self.assertFalse(dg.session_created_allows(victim, manifest=manifest))
        reason, _ = dg.check(f"rm -f -- {victim}", cwd=scoped)
        self.assertTrue(reason, reason)

    def test_legit_cleanup_still_allows_after_seal(self) -> None:
        """Write-once seal must not block honest session create → record → delete."""
        dg = self.dg
        scoped = self._scratch() / "scoped"
        scoped.mkdir()
        manifest = self._init_manifest(scoped)
        self.assertTrue(dg._session_seal_path(manifest).is_file())
        target = scoped / "session-log.txt"
        target.write_text("log\n", encoding="utf-8")
        dg.record_session_created_paths([target], manifest=manifest, roots=[scoped])
        self.assertTrue(dg.session_created_allows(target, manifest=manifest))
        self.assertEqual("", dg.path_reason(target))
        reason, _ = dg.check(f"rm -f -- {target}", cwd=scoped)
        self.assertEqual("", reason)
        target.unlink()
        self.assertFalse(target.exists())


class CeRestoreTest(unittest.TestCase):

    def setUp(self) -> None:
        self.dg = load()
        self._temp = tempfile.TemporaryDirectory()
        self.base = Path(self._temp.name).resolve()
        # Temp dirs are on DEFAULT_ALLOW; clear so delete-check still guards CE paths.
        self._allow = unittest.mock.patch.object(self.dg, "DEFAULT_ALLOW", ())
        self._allow.start()

    def tearDown(self) -> None:
        self._allow.stop()
        self._temp.cleanup()

    def test_restore_checkouts_not_deletes_and_refuses_other_worktree(self) -> None:
        dg = self.dg
        git = shutil.which("git")
        assert git is not None
        primary = self.base / "primary"
        other = self.base / "other-wt"
        primary.mkdir()
        _git_repo(primary)
        ce = primary / ".chaos-engine"
        ce.mkdir()
        tool = ce / "tool.py"
        tool.write_text("primary-v1\n", encoding="utf-8")
        subprocess.run([git, "add", ".chaos-engine/tool.py"], cwd=primary, check=True, capture_output=True)  # nosec B603
        subprocess.run([git, "commit", "-qm", "ce"], cwd=primary, check=True, capture_output=True)  # nosec B603
        session = self.base / "session"
        subprocess.run(  # nosec B603
            [git, "-C", str(primary), "worktree", "add", "-q", str(session)],
            check=True, capture_output=True,
        )
        session_tool = session / ".chaos-engine" / "tool.py"
        session_tool.write_text("session-dirty\n", encoding="utf-8")
        os.environ["CHAOS_ENGINE_PRIMARY"] = str(primary)
        reason, _ = dg.check(f"rm -f -- {session_tool}", cwd=session)
        self.assertTrue(reason, reason)
        self.assertFalse(dg.ce_managed_revert_allows(session_tool, primary=primary))
        refusal, restored = dg.restore_ce_managed_to_primary(
            [session_tool], session=session, primary=primary
        )
        self.assertEqual("", refusal, refusal)
        self.assertEqual([".chaos-engine/tool.py"], restored)
        self.assertEqual("primary-v1\n", session_tool.read_text(encoding="utf-8"))
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
        reason2, _ = dg.check(f"rm -f -- {other_tool}", cwd=other)
        self.assertTrue(reason2)


class BackupRemoteSilenceTest(unittest.TestCase):
    def _repo_with_upstream(self, project: Path) -> Path:
        git = shutil.which("git")
        assert git is not None
        _git_repo(project)
        bare = project.parent / (project.name + "-bare.git")
        subprocess.run([git, "init", "--bare", "-q", str(bare)], check=True, capture_output=True)  # nosec B603
        subprocess.run(  # nosec B603
            [git, "remote", "add", "origin", str(bare)],
            cwd=project, check=True, capture_output=True,
        )
        subprocess.run(  # nosec B603
            [git, "push", "-u", "origin", "HEAD"],
            cwd=project, check=True, capture_output=True,
        )
        return project

    def test_consumer_mode_clean_is_silent(self) -> None:
        """Consumer mode no longer warns just for being consumer; clean + upstream → silence."""
        dg = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "repo"
            project.mkdir()
            marker = project / dg.CONSUMER_MARKER
            marker.parent.mkdir(parents=True)
            marker.write_text("consumer\n", encoding="utf-8")
            self._repo_with_upstream(project)
            self.assertEqual("", dg.backup_remote_issue(project))

    def test_ce_only_dirt_silent_on_primary(self) -> None:
        dg = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "repo"
            project.mkdir()
            self._repo_with_upstream(project)
            ce = project / ".chaos-engine"
            ce.mkdir()
            (ce / "VERSION").write_text("x\n", encoding="utf-8")
            self.assertEqual("", dg.backup_remote_issue(project))

    def test_non_ce_dirt_still_warns(self) -> None:
        dg = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "repo"
            project.mkdir()
            marker = project / dg.CONSUMER_MARKER
            marker.parent.mkdir(parents=True)
            marker.write_text("consumer\n", encoding="utf-8")
            self._repo_with_upstream(project)
            (project / "notes.txt").write_text("x\n", encoding="utf-8")
            self.assertEqual("uncommitted files", dg.backup_remote_issue(project))

    def test_only_ce_dirt_helper(self) -> None:
        dg = load()
        self.assertTrue(
            dg._only_chaos_engine_dirt(" M .chaos-engine/tool.py\n?? .chaos-engine/VERSION\n")
        )
        self.assertTrue(
            dg._only_chaos_engine_dirt("?? .chaos-engine-state/consumer-mode\n")
        )
        self.assertFalse(
            dg._only_chaos_engine_dirt(" M README.md\n M .chaos-engine/tool.py\n")
        )


class L45LessonTest(unittest.TestCase):
    def test_card_and_inventory(self) -> None:
        inv = (ROOT / "chaos-engine/addons/design-skills/INVENTORY.md").read_text(encoding="utf-8")
        collapsed = " ".join(inv.split())
        self.assertIn("L01-L64", collapsed)
        self.assertIn("63 lesson", collapsed)
        idx = (ROOT / "chaos-engine/addons/design-skills/references/lessons/index.md").read_text(
            encoding="utf-8"
        )
        self.assertIn("delete-check-refusal-final.md", idx)


if __name__ == "__main__":
    unittest.main()
