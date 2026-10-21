"""#7251: the delete guard refuses deletes of work that exists nowhere else."""

from __future__ import annotations

import importlib.util
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

    def test_owner_ack_overrides_and_is_logged(self):
        loose = self.base / "loose"
        loose.mkdir()
        store = GUARD._ack_store()
        store.parent.mkdir(parents=True)
        import hashlib
        import time

        store.write_text(json.dumps({hashlib.sha256(b"tok").hexdigest(): {"path": str(loose), "expires": time.time() + 60}}), encoding="utf-8")
        self.assertIn("outside git", self.reason(f"rm -rf {loose}"))
        with unittest.mock.patch.dict(os.environ, {"CHAOS_DELETE_ACK": "tok"}):
            self.assertEqual("", self.reason(f"rm -rf {loose}"))
        self.assertIn(str(loose), store.with_name("delete-overrides.jsonl").read_text(encoding="utf-8"))
        with self.assertRaises(PermissionError):
            GUARD.issue_ack(loose)  # Non-interactive agents cannot mint a token.

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


if __name__ == "__main__":
    unittest.main()
