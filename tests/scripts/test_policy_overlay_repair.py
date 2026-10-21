"""`repair --component policy-overlay` heals the drift doctor reports (repository checkouts)."""

from __future__ import annotations

import importlib.util
import shutil
import subprocess  # nosec B404 - fixed git argv in tests.
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


install = _load("install_policy_overlay_repair", ROOT / "chaos-engine/install.py")
overlay_match = _load("overlay_match_policy_overlay_repair", ROOT / "chaos-engine/overlay_match.py")


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def _project(root: Path) -> Path:
    _write(root / "chaos-engine/install.py", "# source installer\n")
    _write(root / "chaos-engine/identity.md", "new identity\n")
    _write(root / ".chaos-engine/identity.md", "old identity\n")
    git = shutil.which("git") or "git"
    for argv in (["init", "-q"], ["add", "-A"],
                 ["-c", "user.name=t", "-c", "user.email=t@t", "commit", "-qm", "x"]):
        subprocess.run([git, "-C", str(root), *argv], check=True)  # nosec B603
    return root


class PolicyOverlayRepairTest(unittest.TestCase):
    def test_repair_offers_the_component_doctor_reports(self):
        self.assertIn("policy-overlay", install.REPAIRABLE_COMPONENTS)

    def test_doctor_fix_next_names_a_command_that_heals_hash_drift(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = _project(Path(temporary))
            result = {"status": "healthy", "components": {}}
            overlay_match.apply_policy_hash_doctor(result, root)
            self.assertIn("--component policy-overlay", result["components"]["policy-overlay"]["fixNext"])

    def test_repair_reinstalls_from_the_local_source_snapshot(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = _project(Path(temporary))
            calls = []

            def runner(argv, **_kwargs):
                calls.append(argv)
                shutil.copy(root / "chaos-engine/identity.md", root / ".chaos-engine/identity.md")
                return SimpleNamespace(returncode=0, stdout="", stderr="")

            result = install.repair_component(root, "policy-overlay", runner=runner)
            self.assertEqual("repaired", result["status"])
            self.assertEqual("reinstalled-from-local-source", result["action"])
            argv = calls[0]
            self.assertEqual("install", argv[2])
            self.assertIn("--commit", argv)
            snapshot = Path(argv[argv.index("--source") + 1])
            self.assertNotEqual((root / "chaos-engine").resolve(), snapshot.resolve())

    def test_repair_is_a_no_op_without_drift(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = _project(Path(temporary))
            shutil.copy(root / "chaos-engine/identity.md", root / ".chaos-engine/identity.md")
            result = install.repair_component(root, "policy-overlay", runner=None)
            self.assertEqual("verified", result["action"])

    def test_linked_worktree_repair_names_the_materialize_fix(self):
        """#7229: a worktree repair fails with the materialize command, not "reinstall failed"."""
        with tempfile.TemporaryDirectory() as temporary:
            root = _project(Path(temporary) / "primary")
            worktree = Path(temporary) / "wt"
            git = shutil.which("git") or "git"
            subprocess.run([git, "-C", str(root), "worktree", "add", "-q", str(worktree)], check=True)  # nosec B603
            calls = []
            with self.assertRaisesRegex(ValueError, "materialize --primary"):
                install._repair_policy_overlay(worktree, lambda *a, **k: calls.append(a))
            self.assertEqual([], calls)


if __name__ == "__main__":
    unittest.main()
