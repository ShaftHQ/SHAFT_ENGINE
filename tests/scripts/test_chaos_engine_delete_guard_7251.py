"""#7251: the delete guard refuses deletes of work that exists nowhere else."""

from __future__ import annotations

import importlib.util
import io
import json
import os
import shutil
import subprocess  # nosec B404 - fixed git argv in temp fixtures.
import tempfile
import unittest
from pathlib import Path
import unittest.mock

GIT = shutil.which("git") or "git"
ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "chaos-engine"


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


GUARD = load("chaos_engine_delete_guard_7251", CORE / "delete_guard.py")


def git(cwd: Path, *argv: str) -> None:
    subprocess.run(  # nosec B603 - fixed git argv, temp fixture paths.
        [GIT, "-C", str(cwd), "-c", "user.name=t", "-c", "user.email=t@example.invalid", "-c", "commit.gpgsign=false", *argv],
        check=True,
        capture_output=True,
    )


class DeleteGuardTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name).resolve()
        home = self.base / "home"
        home.mkdir()
        # The fixture lives under /tmp, which is allow-listed; drop defaults so the git rules are exercised.
        self.patches = [
            unittest.mock.patch.object(GUARD, "DEFAULT_ALLOW", ()),
            unittest.mock.patch.dict(os.environ, {"HOME": str(home), "XDG_STATE_HOME": str(home / "state")}),
        ]
        for patch in self.patches:
            patch.start()
        self.repo = self.base / "work"
        self.repo.mkdir()
        git(self.repo, "init", "-q", "-b", "main")
        (self.repo / "keep.txt").write_text("x", encoding="utf-8")
        git(self.repo, "add", ".")
        git(self.repo, "commit", "-qm", "init")

    def tearDown(self):
        for patch in reversed(self.patches):
            patch.stop()
        self.temp.cleanup()

    def reason(self, command: str, cwd: Path | None = None) -> str:
        return GUARD.block_reason(command, cwd or self.base, self.repo)

    def push(self) -> None:
        remote = self.base / "remote.git"
        subprocess.run([GIT, "init", "-q", "--bare", str(remote)], check=True, capture_output=True)  # nosec B603
        git(self.repo, "remote", "add", "origin", str(remote))
        git(self.repo, "push", "-q", "-u", "origin", "main")

    def test_delete_guard_blocks_untracked_dir_allows_pushed_clean(self):
        data = self.repo / "data"
        data.mkdir()
        (data / "only-copy.txt").write_text("precious", encoding="utf-8")
        self.assertIn("untracked", self.reason(f"rm -rf {data}"))
        git(self.repo, "add", ".")
        git(self.repo, "commit", "-qm", "data")
        self.assertIn("no upstream", self.reason(f"rm -rf {data}"))
        self.push()
        self.assertEqual("", self.reason(f"rm -rf {data}"))
        self.assertEqual("", self.reason(f"rm -rf {self.repo}"))
        (self.repo / "keep.txt").write_text("edited", encoding="utf-8")
        self.assertIn("modified", self.reason(f"rm -rf {self.repo}"))

    def test_ahead_of_upstream_is_refused(self):
        self.push()
        (self.repo / "new.txt").write_text("n", encoding="utf-8")
        git(self.repo, "add", ".")
        git(self.repo, "commit", "-qm", "local only")
        self.assertIn("ahead", self.reason(f"rm -rf {self.repo}"))

    def test_find_delete_and_exec_rm_are_refused(self):
        (self.repo / "u.txt").write_text("u", encoding="utf-8")
        self.assertIn("refused", self.reason(f"find {self.repo} -name '*.txt' -delete"))
        self.assertIn("refused", self.reason(f"find {self.repo} -type f -exec rm -f {{}} +"))
        self.assertEqual("", self.reason(f"find {self.repo} -name '*.txt' -print"))

    def test_glob_cd_and_xargs_cases(self):
        (self.repo / "a.log").write_text("a", encoding="utf-8")
        self.assertIn("a.log", self.reason(f"rm -f {self.repo}/*.log"))
        self.assertIn("a.log", self.reason(f"cd {self.repo} && rm -rf a.log"))
        self.assertIn("a.log", self.reason("rm -rf a.log", cwd=self.repo))
        self.assertIn("a.log", self.reason(f"bash -c 'cd {self.repo}; rm a.log'"))
        self.assertIn("stdin", self.reason(f"ls {self.repo} | xargs rm -f"))
        self.assertIn("computed", self.reason('rm -rf "$TARGET"'))
        self.assertIn("a.log", self.reason(f"git -C {self.repo} clean -fdx"))
        self.assertEqual("", self.reason(f"git -C {self.repo} clean -n"))

    def test_outside_git_needs_allow_list_and_tmp_default(self):
        loose = self.base / "loose"
        loose.mkdir()
        self.assertIn("outside git", self.reason(f"rm -rf {loose}"))
        state = self.repo / ".chaos-engine-state"
        state.mkdir()
        (state / "delete-allow.txt").write_text(f"{loose}/**\n", encoding="utf-8")
        self.assertEqual("", self.reason(f"rm -rf {loose}"))
        self.assertEqual("", self.reason("rm -rf /nonexistent-7251/x"))

    def test_default_allow_list_covers_tmp_caches_and_video_renders(self):
        allowed = (
            str(Path(tempfile.gettempdir()).resolve() / "x"),
            str(Path.home() / ".cache/pip"),
            "/w/app/node_modules/.cache/babel",
            "/w/app/target/classes",
            "/w/pkg/__pycache__/m.pyc",
            "/w/pkg/.pytest_cache",
            "/w/bot/video/clips/a.mp4",
            "/w/bot/video/renders",
            "/w/bot/videos/ep1/out/final.mp4",
        )
        refused = ("/w/bot/video/src/a.mp4", "/w/bot/website", "/w/bot/video", "/w/app/src/target.py")
        with unittest.mock.patch.object(GUARD, "DEFAULT_ALLOW", load("fresh_7251", CORE / "delete_guard.py").DEFAULT_ALLOW):
            for path in allowed:
                self.assertTrue(GUARD.allow_listed(Path(path)), path)
            for path in refused:
                self.assertFalse(GUARD.allow_listed(Path(path)), path)

    def test_python_rmtree_literal_checked_and_computed_refused(self):
        (self.repo / "d").mkdir()
        (self.repo / "d" / "f").write_text("f", encoding="utf-8")
        self.assertIn("refused", self.reason(f"python3 -c \"import shutil; shutil.rmtree('{self.repo / 'd'}')\""))
        self.assertIn("non-literal", self.reason("python3 -c 'import shutil,sys; shutil.rmtree(sys.argv[1])' x"))

    def _loose(self, name="loose"):
        path = self.base / name
        path.mkdir()
        return path

    def test_code_match_allows_once_with_warning_and_logs(self):
        """#7343: owner pastes the chat code back; it unlocks exactly once."""
        loose = self._loose()
        self.assertIn("outside git", self.reason(f"rm -rf {loose}"))
        code = GUARD.request_code([loose])
        self.assertRegex(code, r"^[A-Z2-9]{6}$")
        self.assertNotIn(code, GUARD._ack_store().read_text(encoding="utf-8"))  # Only the hash is stored.
        reason, warnings = GUARD.check(f"rm -rf {loose}", self.base, self.base, code.lower())
        self.assertEqual("", reason)
        self.assertIn("outside git", warnings[0])  # Not-pushed becomes a warning, not a block.
        self.assertIn("used", GUARD._ack_store().with_name("delete-overrides.jsonl").read_text(encoding="utf-8"))
        again, _ = GUARD.check(f"rm -rf {loose}", self.base, self.base, code)  # Reuse.
        self.assertIn("already used", again)

    def test_code_mismatch_expiry_and_path_change_refuse(self):
        loose, other = self._loose(), self._loose("other")
        code = GUARD.request_code([loose])
        self.assertIn("unknown", GUARD.check(f"rm -rf {loose}", self.base, self.base, "ZZZZZZ")[0])
        self.assertIn("different path list", GUARD.check(f"rm -rf {loose} {other}", self.base, self.base, code)[0])
        self.assertIn("different path list", GUARD.check(f"rm -rf {other}", self.base, self.base, code)[0])
        with unittest.mock.patch.dict(os.environ, {"CHAOS_DELETE_ACK": code}):
            self.assertEqual("", self.reason(f"rm -rf {loose}"))  # Env form; still valid after mismatches.
        stale = GUARD.request_code([loose], ttl=-1)
        self.assertIn("expired", GUARD.check(f"rm -rf {loose}", self.base, self.base, stale)[0])
        self.assertFalse(hasattr(GUARD, "issue_ack"))  # TTY-only override is gone.

    def test_never_touch_list_no_code_unlocks(self):
        for name in ("yt", "designer-ws.git.retired", "ROG-x"):
            target = self._loose(name)
            with self.assertRaises(PermissionError):
                GUARD.request_code([target])
            self.assertIn("never-touch", self.reason(f"rm -rf {target}/sub"))
        self.assertIn("contains never-touch", self.reason(f"rm -rf {self.base}"))
        profile = self.base / "h" / ".config" / "google-chrome"
        profile.mkdir(parents=True)
        self.assertIn("never-touch", self.reason(f"rm -rf {profile}/Default"))
        self.assertIn("browser profile", self.reason(f"rm -rf {self.base / 'h'}"))
        self.assertEqual(2, GUARD.main(["--request-code", str(profile)]))

    def test_unlocked_names_need_a_code(self):
        for name in ("wt-relblock", "wt-ls", "ce-src-abc"):
            target = self._loose(name)
            self.assertEqual("", GUARD.never_touch(target))
            self.assertIn("outside git", self.reason(f"rm -rf {target}"))
            code = GUARD.request_code([target])
            self.assertEqual("", GUARD.check(f"rm -rf {target}", self.base, self.base, code)[0])

    def test_yt_verified_uploaded_needs_code_meta_and_unlisted_refused(self):
        """#7525: listed+proof+code unlocks exact yt files; meta/unlisted/globs stay locked."""
        yt = self.base / "yt"
        (yt / "meta").mkdir(parents=True)
        (yt / "src").mkdir(parents=True)
        listed = yt / "src" / "AbCdEfGhIjK.mp4"
        unlisted = yt / "src" / "unlisted.mp4"
        no_proof = yt / "src" / "noproof.mp4"
        bad_proof = yt / "src" / "badproof.mp4"
        meta_listed = yt / "meta" / "AbCdEfGhIjK.mp4"
        for target in (listed, unlisted, no_proof, bad_proof, meta_listed):
            target.write_text("x", encoding="utf-8")
        allow = self.base / "home" / "yt-verified-delete.txt"
        allow.write_text(
            "\n".join(
                [
                    f"{listed} AbCdEfGhIjK",
                    f"{no_proof}",
                    f"{bad_proof} not-a-valid-id",
                    f"{meta_listed} AbCdEfGhIjK",
                    f"{yt / 'src'}/** AbCdEfGhIjK",
                    f"{yt / 'src'}/ AbCdEfGhIjK",
                    f"{yt / 'batch'} AbCdEfGhIjK",
                    "",
                ]
            ),
            encoding="utf-8",
        )
        with unittest.mock.patch.dict(os.environ, {"CHAOS_YT_VERIFIED_DELETE": str(allow)}):
            self.assertEqual("", GUARD.never_touch(listed))
            self.assertIn("outside git", self.reason(f"rm -f {listed}"))
            code = GUARD.request_code([listed])
            self.assertEqual("", GUARD.check(f"rm -f {listed}", self.base, self.base, code)[0])
            for kept in (unlisted, no_proof, bad_proof, meta_listed, yt, yt / "meta", yt / "src"):
                self.assertIn("never-touch", GUARD.never_touch(kept) or self.reason(f"rm -rf {kept}"), kept)
                with self.assertRaises(PermissionError):
                    GUARD.request_code([kept])
            loaded = GUARD._load_yt_verified()
            self.assertEqual("AbCdEfGhIjK", loaded.get(str(listed.resolve())))
            self.assertNotIn(str(unlisted.resolve()), loaded)
            self.assertNotIn(str(no_proof.resolve()), loaded)
            self.assertNotIn(str(bad_proof.resolve()), loaded)
            self.assertNotIn(str((yt / "src").resolve()), loaded)
            self.assertNotIn(str((yt / "batch").resolve()), loaded)
            # meta may be listed in the file but never_touch still refuses it (above).

    def test_yt_old_renders_need_a_code_rest_refused(self):
        yt = self.base / "yt"
        (yt / "meta").mkdir(parents=True)
        for name in ("ep1.r3.mp4", "ep1.mp4", "meta/ep1.r3.mp4", "meta/notes.json"):
            (yt / name).write_text("x", encoding="utf-8")
        old = yt / "ep1.r3.mp4"
        self.assertEqual("", GUARD.never_touch(old))
        self.assertIn("outside git", self.reason(f"rm -f {old}"))
        code = GUARD.request_code([old])
        self.assertEqual("", GUARD.check(f"rm -f {old}", self.base, self.base, code)[0])
        for kept in (yt, yt / "meta", yt / "ep1.mp4", yt / "meta" / "ep1.r3.mp4", yt / "meta" / "notes.json"):
            self.assertIn("never-touch", self.reason(f"rm -rf {kept}"))
            with self.assertRaises(PermissionError):
                GUARD.request_code([kept])

    def test_cli_request_and_verify(self):
        loose = self._loose()
        out = io.StringIO()
        with unittest.mock.patch("sys.stdout", out), unittest.mock.patch.object(GUARD.Path, "cwd", return_value=self.base):
            self.assertEqual(0, GUARD.main(["--request-code", str(loose)]))
            code = out.getvalue().split()[1]
            self.assertEqual(1, GUARD.main([str(loose)]))
            self.assertEqual(0, GUARD.main(["--code", code, str(loose)]))
            self.assertEqual(1, GUARD.main(["--code", code, str(loose)]))

    def test_hook_blocks_and_doctor_row_passes(self):
        guard = load("chaos_engine_guard_hook_7251", CORE / "hooks" / "guard.py")
        (self.repo / "u.txt").write_text("u", encoding="utf-8")
        with unittest.mock.patch.object(guard, "_delete_guard", return_value=GUARD):
            self.assertIn("Delete guard", guard.delete_guard_reason((f"rm -rf {self.repo}",), self.base))
        self.assertIsNotNone(guard._delete_guard())  # Real wiring: the hook finds the core module.
        event = {"hook_event_name": "PreToolUse", "tool_name": "Bash", "tool_input": {"command": f"rm -rf {self.repo}"}, "cwd": str(self.base)}
        with unittest.mock.patch.object(guard, "_delete_guard", return_value=GUARD), unittest.mock.patch.object(guard, "_record_denial_with_significance"), unittest.mock.patch.object(guard, "_run_event", return_value=0) as rest:
            self.assertEqual(2, guard._guarded_event(event, "claude"))
            rest.assert_not_called()
        self.assertIn("_guarded_event", (CORE / "hooks" / "guard.py").read_text(encoding="utf-8").split("def main()")[1])
        self.assertEqual(("pass", ""), GUARD.doctor_check(CORE))
        install = load("chaos_engine_install_7251", CORE / "install.py")
        self.assertEqual(("pass",), install.delete_guard_status(CORE))
        status = install.backup_remote_advisory(self.repo, CORE)
        self.assertEqual("warn", status[0])
        self.assertIn("no remote", status[1])
        self.push()
        (self.repo / "u.txt").unlink()
        self.assertEqual(("pass",), install.backup_remote_advisory(self.repo, CORE))


    def test_backup_remote_warns_in_consumer_mode(self):
        """#7263: consumer exclude hides .memory, so a pushed clean tree must still warn."""
        self.push()
        self.assertEqual(("pass", ""), GUARD.backup_remote_status(self.repo))
        marker = self.repo / ".chaos-engine-state" / "consumer-mode"
        marker.parent.mkdir()
        marker.write_text("consumer\n", encoding="utf-8")
        exclude = self.repo / ".git" / "info" / "exclude"
        exclude.write_text("/.chaos-engine-state/\n/.memory/\n", encoding="utf-8")
        (self.repo / ".memory").mkdir()
        (self.repo / ".memory" / "m.md").write_text("only copy", encoding="utf-8")
        status, reason = GUARD.backup_remote_status(self.repo)
        self.assertEqual("warn", status)
        self.assertIn("consumer mode hides harness state", reason)

if __name__ == "__main__":
    unittest.main()
