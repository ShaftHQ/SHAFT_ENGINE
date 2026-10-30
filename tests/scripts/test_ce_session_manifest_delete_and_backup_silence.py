"""Session created-paths manifest allow + backup-remote silence + refusal final."""
from __future__ import annotations

import importlib.util
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DG = ROOT / "chaos-engine" / "delete_guard.py"
# /workspace is not on DEFAULT_ALLOW (unlike tempfile.gettempdir()).
WORKSPACE_SCRATCH = Path("/workspace/ce-cleanup-gate-scratch")


def load():
    spec = importlib.util.spec_from_file_location("ce_dg_session", DG)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _git_repo(base: Path) -> Path:
    git = shutil.which("git")
    assert git is not None
    subprocess.run([git, "init", "-q"], cwd=base, check=True, capture_output=True)
    subprocess.run([git, "config", "user.email", "t@t"], cwd=base, check=True, capture_output=True)
    subprocess.run([git, "config", "user.name", "t"], cwd=base, check=True, capture_output=True)
    (base / "README").write_text("x\n", encoding="utf-8")
    subprocess.run([git, "add", "README"], cwd=base, check=True, capture_output=True)
    subprocess.run([git, "commit", "-qm", "i"], cwd=base, check=True, capture_output=True)
    return base


class SessionManifestDeleteTest(unittest.TestCase):
    def setUp(self) -> None:
        WORKSPACE_SCRATCH.mkdir(parents=True, exist_ok=True)
        self._dirs: list[Path] = []

    def tearDown(self) -> None:
        import shutil
        for d in self._dirs:
            shutil.rmtree(d, ignore_errors=True)

    def _scratch(self) -> Path:
        base = WORKSPACE_SCRATCH / f"case-{os.getpid()}-{len(self._dirs)}"
        base.mkdir(parents=True)
        self._dirs.append(base)
        return base

    def test_manifest_path_allowed_without_code(self) -> None:
        dg = load()
        base = self._scratch()
        target = base / "session-log.txt"
        target.write_text("log\n", encoding="utf-8")
        other = base / "other.txt"
        other.write_text("x\n", encoding="utf-8")
        manifest = base / "created-paths.json"
        os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
        try:
            dg.record_session_created_paths([target], manifest=manifest)
            self.assertTrue(dg.session_created_allows(target, manifest=manifest))
            self.assertEqual("", dg.path_reason(target))
            # Sibling not on manifest and outside allow-list / not pushed → refused.
            reason = dg.path_reason(other)
            self.assertNotEqual("", reason)
        finally:
            os.environ.pop(dg.SESSION_CREATED_MANIFEST_ENV, None)

    def test_never_touch_still_wins(self) -> None:
        dg = load()
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            yt = base / "yt" / "clip.mp4"
            yt.parent.mkdir(parents=True)
            yt.write_text("x", encoding="utf-8")
            manifest = base / "created-paths.json"
            os.environ[dg.SESSION_CREATED_MANIFEST_ENV] = str(manifest)
            try:
                dg.record_session_created_paths([yt], manifest=manifest)
                hit = dg.never_touch(yt)
                self.assertTrue(hit)
                reason, _ = dg.check(f"rm -rf -- {yt}", cwd=base)
                self.assertTrue(reason)
                self.assertIn("never-touch", reason)
            finally:
                os.environ.pop(dg.SESSION_CREATED_MANIFEST_ENV, None)

    def test_refusal_is_final_block_reason_matches_check(self) -> None:
        dg = load()
        base = self._scratch()
        _git_repo(base)
        secret = base / "only-copy.txt"
        secret.write_text("unique\n", encoding="utf-8")
        cmd = f"rm -f -- {secret}"
        via_check = dg.check(cmd, cwd=base)[0]
        via_block = dg.block_reason(cmd, cwd=base)
        self.assertEqual(via_check, via_block)
        self.assertTrue(via_block, via_block)
        doc = (ROOT / "chaos-engine/delete_guard.py").read_text(encoding="utf-8")
        self.assertIn("refusal is final", doc)
        lesson = (
            ROOT
            / "chaos-engine/addons/design-skills/references/lessons/delete-check-refusal-final.md"
        )
        self.assertTrue(lesson.is_file())
        body = lesson.read_text(encoding="utf-8")
        self.assertIn("final", body.casefold())
        self.assertIn("raw", body.casefold())


class BackupRemoteSilenceTest(unittest.TestCase):
    def test_consumer_mode_silent(self) -> None:
        dg = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            marker = project / dg.CONSUMER_MARKER
            marker.parent.mkdir(parents=True)
            marker.write_text("consumer\n", encoding="utf-8")
            _git_repo(project)
            self.assertEqual("", dg.backup_remote_issue(project))

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
