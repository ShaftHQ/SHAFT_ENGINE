"""A named worktree must win over the process cwd when unittest imports tests."""

from __future__ import annotations

import os
import subprocess  # nosec B404 - fixed interpreter, runner path, and temp fixtures.
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNNER = ROOT / "scripts" / "ci" / "worktree_unittest.py"
_EMPTY_CWD_LAUNCH = (
    "import runpy, sys\n"
    "sys.argv = sys.argv[1:]\n"
    "runpy.run_path(sys.argv[0], run_name='__main__')\n"
)


def _probe(marker: str) -> str:
    return textwrap.dedent(
        f"""\
        import os
        import unittest
        from pathlib import Path

        MARKER = {marker!r}

        class ProbeTest(unittest.TestCase):
            def test_loaded_path(self):
                loaded = Path(__file__).resolve()
                Path(os.environ["PROBE_REPORT"]).write_text(
                    MARKER + "\\n" + str(loaded) + "\\n",
                    encoding="utf-8",
                )
        """
    )


def _invoke(primary: Path, worktree: Path, report: Path, via: str) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env.pop("PYTHONPATH", None)
    env["PROBE_REPORT"] = str(report)
    if via == "script":
        command = [sys.executable, str(RUNNER), "--worktree", str(worktree), "wt_probe"]
    elif via == "empty-cwd":
        command = [
            sys.executable,
            "-c",
            _EMPTY_CWD_LAUNCH,
            str(RUNNER),
            "--worktree",
            str(worktree),
            "wt_probe",
        ]
    else:
        raise AssertionError(via)
    return subprocess.run(  # nosec B603 - argv list, no shell.
        command,
        cwd=primary,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


class WorktreeUnittestTest(unittest.TestCase):
    def _assert_worktree_loaded(
        self,
        primary: Path,
        worktree: Path,
        report: Path,
        proc: subprocess.CompletedProcess[str],
        marker: str,
    ) -> None:
        self.assertEqual(proc.returncode, 0, proc.stdout + proc.stderr)
        marker_text, loaded_text = report.read_text(encoding="utf-8").splitlines()
        loaded = Path(loaded_text)
        self.assertEqual(marker_text, marker)
        self.assertTrue(loaded.is_relative_to(worktree.resolve()))
        self.assertFalse(loaded.is_relative_to(primary.resolve()))

    def test_different_markers_import_the_worktree_file(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            primary = root / "primary"
            worktree = root / "worktree"
            primary.mkdir()
            worktree.mkdir()
            (primary / "wt_probe.py").write_text(_probe("primary"), encoding="utf-8")
            (worktree / "wt_probe.py").write_text(_probe("worktree"), encoding="utf-8")
            self.assertNotEqual(
                (primary / "wt_probe.py").read_bytes(),
                (worktree / "wt_probe.py").read_bytes(),
            )
            for via in ("script", "empty-cwd"):
                with self.subTest(via=via):
                    report = root / f"report-{via}.txt"
                    proc = _invoke(primary, worktree, report, via)
                    self._assert_worktree_loaded(primary, worktree, report, proc, "worktree")

    def test_identical_bytes_still_report_the_worktree_path(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            primary = root / "primary"
            worktree = root / "worktree"
            primary.mkdir()
            worktree.mkdir()
            source = _probe("same")
            (primary / "wt_probe.py").write_text(source, encoding="utf-8")
            (worktree / "wt_probe.py").write_text(source, encoding="utf-8")
            self.assertEqual(
                (primary / "wt_probe.py").read_bytes(),
                (worktree / "wt_probe.py").read_bytes(),
            )
            for via in ("script", "empty-cwd"):
                with self.subTest(via=via):
                    report = root / f"report-{via}.txt"
                    proc = _invoke(primary, worktree, report, via)
                    self._assert_worktree_loaded(primary, worktree, report, proc, "same")
