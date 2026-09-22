"""Install of commit C must not be replaced by an older checkout (#6121)."""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import subprocess  # nosec B404 - test git helper, no shell
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
INSTALLER = ROOT / "chaos-engine/install.py"
OVERLAY = ROOT / "chaos-engine/overlay_match.py"
SOURCE = ROOT / "chaos-engine"
MARKER = "\n# commit-byte-marker-6121\n"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    if specification is None or specification.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def git(cwd: Path, *args: str) -> str:
    env = os.environ.copy()
    env.update(
        {
            "GIT_AUTHOR_NAME": "test",
            "GIT_AUTHOR_EMAIL": "test@example.com",
            "GIT_COMMITTER_NAME": "test",
            "GIT_COMMITTER_EMAIL": "test@example.com",
        }
    )
    completed = subprocess.run(  # nosec B603 B607 - test git helper, no shell
        ["git", *args],
        cwd=cwd,
        check=True,
        capture_output=True,
        text=True,
        env=env,
        shell=False,
    )
    return completed.stdout.strip()


def copy_source(destination: Path) -> None:
    ignored = shutil.ignore_patterns("__pycache__", "*.pyc")
    shutil.copytree(SOURCE, destination, ignore=ignored)


class OverlayCommitBytes6121Test(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.install = load(INSTALLER, "ce_install_6121")
        cls.overlay = load(OVERLAY, "ce_overlay_6121")

    def test_owned_tree_differs_from_commit_reports_overlay_drift(self):
        with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as temporary:
            project, new_sha, _source_new = self._repo_with_older_head(Path(temporary))
            shutil.copytree(project / "chaos-engine", project / ".chaos-engine")
            differing = self.overlay.owned_tree_differs_from_commit(
                project, project / ".chaos-engine", new_sha
            )
            self.assertIn("identity.md", differing)
            self.assertIn("commit-only-6121.txt", differing)
            self.assertIsNone(
                self.overlay.owned_tree_differs_from_commit(
                    project, project / ".chaos-engine", "b" * 40
                )
            )

    def test_install_keeps_published_commit_when_head_is_older(self):
        with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as temporary:
            project, new_sha, source_new = self._repo_with_older_head(Path(temporary))
            record = {
                "kind": "git",
                "commit": new_sha,
                "repository": "shafthq/shaft_engine",
                "branch": "main",
            }
            installed = self.install.install(
                project,
                source_new,
                new_sha,
                source_record=record,
                distribution="repository",
            )
            text = (installed / "identity.md").read_text(encoding="utf-8")
            self.assertIn("commit-byte-marker-6121", text)
            self.assertEqual(
                "only-on-commit\n",
                (installed / "commit-only-6121.txt").read_text(encoding="utf-8"),
            )
            manifest = json.loads((installed / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(new_sha, manifest["source"]["commit"])

    def test_doctor_rejects_rewritten_digests_for_a_real_commit(self):
        with tempfile.TemporaryDirectory(ignore_cleanup_errors=True) as temporary:
            project, new_sha, _source_new = self._repo_with_older_head(Path(temporary))
            overlay = project / ".chaos-engine"
            shutil.copytree(project / "chaos-engine", overlay)
            files = {"identity.md": "digest-before-rewrite"}
            (overlay / "manifest.json").write_text(
                json.dumps(
                    {
                        "schemaVersion": 1,
                        "source": {"commit": new_sha, "kind": "local"},
                        "files": files,
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            result = {"status": "healthy", "components": {"core": {"status": "healthy"}}}
            self.overlay.apply_doctor_overlay_match(result, project)
            self.assertEqual("recovery-required", result["status"])
            self.assertEqual(
                "overlay-commit-mismatch", result["components"]["core"]["detail"]
            )
            kept = json.loads((overlay / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(files, kept["files"])

    def _repo_with_older_head(self, root: Path) -> tuple[Path, str, Path]:
        project = root / "repo"
        project.mkdir()
        copy_source(project / "chaos-engine")
        git(project, "init", "-b", "older")
        git(project, "add", "chaos-engine")
        git(project, "commit", "-m", "old")
        git(project, "checkout", "-b", "newer")
        identity = project / "chaos-engine" / "identity.md"
        identity.write_text(identity.read_text(encoding="utf-8") + MARKER, encoding="utf-8")
        (project / "chaos-engine" / "commit-only-6121.txt").write_text(
            "only-on-commit\n", encoding="utf-8"
        )
        git(project, "add", "chaos-engine/identity.md", "chaos-engine/commit-only-6121.txt")
        git(project, "commit", "-m", "new")
        new_sha = git(project, "rev-parse", "HEAD")
        source_new = root / "source-new"
        shutil.copytree(project / "chaos-engine", source_new)
        git(project, "checkout", "older")
        return project, new_sha, source_new


if __name__ == "__main__":
    unittest.main()
