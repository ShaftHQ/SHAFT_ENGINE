"""Fresh worktrees receive a real guard, not a symlink or an exit-0 stub (#7916)."""

from __future__ import annotations

import importlib.util
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
OVERLAY_SOURCE = ROOT / "chaos-engine" / "worktree_overlay.py"
LAUNCH_SOURCE = ROOT / "chaos-engine" / "hooks" / "launch.py"


def _load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _git(cwd: Path, *args: str) -> None:
    git = shutil.which("git")
    if not git:
        raise RuntimeError("git is not on PATH")
    env = os.environ.copy()
    env.update(
        {
            "GIT_AUTHOR_NAME": "Harness Test",
            "GIT_AUTHOR_EMAIL": "harness@example.com",
            "GIT_COMMITTER_NAME": "Harness Test",
            "GIT_COMMITTER_EMAIL": "harness@example.com",
        }
    )
    subprocess.run(
        [git, "-c", "user.name=Harness Test", "-c", "user.email=harness@example.com", *args],
        cwd=cwd,
        check=True,
        capture_output=True,
        text=True,
        env=env,
    )


def _primary_with_worktree(root: Path, guard: bytes, launch: bytes) -> tuple[Path, Path]:
    primary = root / "primary"
    primary.mkdir()
    _git(primary, "init", "-b", "main")
    (primary / "README").write_text("primary\n", encoding="utf-8")
    _git(primary, "add", "README")
    _git(primary, "commit", "-m", "init")
    hooks = primary / ".chaos-engine" / "hooks"
    hooks.mkdir(parents=True)
    (hooks / "guard.py").write_bytes(guard)
    (hooks / "launch.py").write_bytes(launch)
    for name in ("lifecycle.py", "kernel.py", "retrieve_justification.py", "reflection.py"):
        sibling = hooks / name
        if not sibling.exists():
            sibling.write_text(f"print({name!r})\n", encoding="utf-8")
    session = root / "session"
    _git(primary, "worktree", "add", "--detach", str(session), "HEAD")
    return primary, session


class DelegateWorktreeHooksTest(unittest.TestCase):
    def test_fresh_worktree_guard_is_real_file(self) -> None:
        overlay = _load(OVERLAY_SOURCE, "ce_delegate_hooks")
        guard_bytes = (
            b"#!/usr/bin/env python3\n"
            b"# real portable guard fixture for a delegated worktree\n"
            b"print('guard-7916')\n"
        )
        launch_bytes = (
            b"#!/usr/bin/env python3\n"
            b"# real portable launcher fixture for a delegated worktree\n"
            b"print('launch-7916')\n"
        )
        with tempfile.TemporaryDirectory() as temporary:
            primary, session = _primary_with_worktree(Path(temporary), guard_bytes, launch_bytes)
            self.assertFalse((session / ".chaos-engine").exists())
            overlay.ensure_delegate_hooks(primary, session)
            copied = session / ".chaos-engine" / "hooks" / "guard.py"
            self.assertTrue(copied.is_file())
            self.assertFalse(copied.is_symlink())
            self.assertEqual(guard_bytes, copied.read_bytes())
            launched = session / ".chaos-engine" / "hooks" / "launch.py"
            self.assertTrue(launched.is_file())
            self.assertFalse(launched.is_symlink())
            self.assertEqual(launch_bytes, launched.read_bytes())
            for name in ("lifecycle.py", "kernel.py", "retrieve_justification.py", "reflection.py"):
                sibling = session / ".chaos-engine" / "hooks" / name
                self.assertTrue(sibling.is_file(), name)
                self.assertFalse(sibling.is_symlink(), name)
                self.assertEqual((primary / ".chaos-engine" / "hooks" / name).read_bytes(), sibling.read_bytes())

    def test_primary_symlink_or_exit0_stub_is_refused(self) -> None:
        overlay = _load(OVERLAY_SOURCE, "ce_delegate_hooks_refuse")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            primary, session = _primary_with_worktree(
                root,
                b"raise SystemExit(0)\n",
                b"print('launch')\n",
            )
            with self.assertRaises(RuntimeError):
                overlay.ensure_delegate_hooks(primary, session)
            self.assertFalse((session / ".chaos-engine" / "hooks" / "guard.py").exists())

            real = primary / ".chaos-engine" / "hooks" / "guard-real.py"
            real.write_bytes(b"print('real-guard')\n")
            guard = primary / ".chaos-engine" / "hooks" / "guard.py"
            guard.unlink()
            guard.symlink_to(real)
            with self.assertRaises(RuntimeError):
                overlay.ensure_delegate_hooks(primary, session)
            self.assertFalse((session / ".chaos-engine" / "hooks" / "guard.py").exists())

            guard.unlink()
            guard.write_text("print('real-guard')\n", encoding="utf-8")
            lifecycle = primary / ".chaos-engine" / "hooks" / "lifecycle.py"
            lifecycle.write_text("raise SystemExit(0)\n", encoding="utf-8")
            with self.assertRaises(RuntimeError):
                overlay.ensure_delegate_hooks(primary, session)
            self.assertFalse((session / ".chaos-engine" / "hooks" / "lifecycle.py").exists())

    def test_launch_seeds_guard_before_failing(self) -> None:
        launch = _load(LAUNCH_SOURCE, "ce_launch_delegate_hooks")
        guard_bytes = (
            b"#!/usr/bin/env python3\n"
            b"# real portable guard fixture beside the delegated launcher\n"
            b"print('guard-beside')\n"
        )
        launch_bytes = LAUNCH_SOURCE.read_bytes()
        with tempfile.TemporaryDirectory() as temporary:
            primary, session = _primary_with_worktree(Path(temporary), guard_bytes, launch_bytes)
            self.assertFalse((session / ".chaos-engine").exists())
            launch.seed_delegate_hooks_if_guard_missing(session)
            copied = session / ".chaos-engine" / "hooks" / "guard.py"
            self.assertTrue(copied.is_file())
            self.assertFalse(copied.is_symlink())
            self.assertEqual(guard_bytes, copied.read_bytes())

    def test_host_locator_copies_hooks_into_fresh_linked_worktree(self) -> None:
        """The shipped locator reads the primary overlay and copies real siblings."""
        hosts = _load(ROOT / "chaos-engine" / "hosts.py", "ce_hosts_delegate_hooks")
        names = (
            "guard.py",
            "launch.py",
            "lifecycle.py",
            "kernel.py",
            "retrieve_justification.py",
            "reflection.py",
        )
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            primary = root / "primary"
            primary.mkdir()
            _git(primary, "init", "-b", "main")
            (primary / "README").write_text("primary\n", encoding="utf-8")
            _git(primary, "add", "README")
            _git(primary, "commit", "-m", "init")
            hooks = primary / ".chaos-engine" / "hooks"
            hooks.mkdir(parents=True)
            for name in names:
                shutil.copy2(ROOT / "chaos-engine" / "hooks" / name, hooks / name)
            overlay = primary / "chaos-engine"
            overlay.mkdir()
            shutil.copy2(OVERLAY_SOURCE, overlay / "worktree_overlay.py")
            session = root / "session"
            _git(primary, "worktree", "add", "--detach", str(session), "HEAD")
            self.assertFalse((session / "chaos-engine" / "worktree_overlay.py").exists())
            self._run_host_locator(hosts, primary, session, names, windows=False, host="claude")
            windows_session = root / "session-windows"
            _git(primary, "worktree", "add", "--detach", str(windows_session), "HEAD")
            self.assertFalse((windows_session / "chaos-engine" / "worktree_overlay.py").exists())
            self._run_host_locator(hosts, primary, windows_session, names, windows=True, host="grok")

    def _run_host_locator(
        self,
        hosts,
        primary: Path,
        session: Path,
        names: tuple[str, ...],
        *,
        windows: bool,
        host: str,
    ) -> None:
        command = hosts.chaos_guard_locator_command(windows=windows, host=host)
        python3 = shutil.which("python3")
        if not python3:
            self.fail("python3 is not on PATH")
        if windows:
            prefix = 'py -3 -c "'
            self.assertTrue(command.startswith(prefix))
            script, separator, tail = command[len(prefix) :].partition('"')
            self.assertEqual('"', separator)
            argv = [python3, "-c", script, *tail.split()]
        else:
            argv = ["/bin/sh", "-c", command]
        result = subprocess.run(
            argv,
            cwd=session,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=20,
            check=False,
        )
        combined = result.stdout + result.stderr
        self.assertNotIn("ChaosEngine lifecycle core is unavailable", combined)
        self.assertNotIn("ChaosEngine policy kernel is unavailable", combined)
        self.assertNotIn("Traceback (most recent call last)", combined)
        self.assertNotEqual(1, result.returncode, combined)
        self.assertIn(result.returncode, (0, 2), combined)
        for name in names:
            copied = session / ".chaos-engine" / "hooks" / name
            source = primary / ".chaos-engine" / "hooks" / name
            self.assertTrue(copied.is_file(), combined)
            self.assertFalse(copied.is_symlink(), name)
            self.assertEqual(source.read_bytes(), copied.read_bytes(), name)
        self.assertTrue("{" in result.stdout or "{" in result.stderr, combined)


if __name__ == "__main__":
    unittest.main()
