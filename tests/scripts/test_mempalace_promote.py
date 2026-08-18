"""SHAFT MemPalace promote-family listing (#5119)."""

from __future__ import annotations

import importlib.util
import shutil
import subprocess  # nosec B404 - resolved Git and controlled fixture paths.
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools/repository-map/mempalace_promote.py"


class MempalacePromoteTest(unittest.TestCase):
    def load(self):
        self.assertTrue(SCRIPT.is_file(), SCRIPT)
        spec = importlib.util.spec_from_file_location("mempalace_promote", SCRIPT)
        self.assertIsNotNone(spec)
        self.assertIsNotNone(spec.loader)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module

    def test_lists_exact_promote_families_and_skips_pom(self):
        git = shutil.which("git")
        if git is None:
            self.skipTest("git is required")
        module = self.load()
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "repo"
            files = {
                "src/main/resources/properties/custom.properties": "k=v\n",
                "src/main/resources/META-INF/services/com.shaft.plugin.Listener": "impl\n",
                "src/main/resources/META-INF/plugin.xml": "<idea-plugin/>\n",
                "pom.xml": "<project/>\n",
                "samples/login.feature": "Feature: login\n",
                "shaft-mcp/Dockerfile": "FROM scratch\n",
                "shaft-mcp/Dockerfile.fly": "FROM scratch\n",
                "src/ok.py": "print(1)\n",
            }
            root.mkdir()
            for relative, content in files.items():
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(content, encoding="utf-8")

            def run_git(*args):
                return subprocess.run(  # nosec B603 - resolved Git and fixture cwd.
                    [git, *args],
                    cwd=root,
                    check=True,
                    capture_output=True,
                    text=True,
                )

            run_git("init")
            run_git("config", "user.email", "promote@example.invalid")
            run_git("config", "user.name", "Promote Test")
            run_git("add", ".")
            run_git("commit", "-m", "fixture")

            listed = module.list_promote_paths(root)
            self.assertEqual(
                [
                    "samples/login.feature",
                    "shaft-mcp/Dockerfile",
                    "shaft-mcp/Dockerfile.fly",
                    "src/main/resources/META-INF/plugin.xml",
                    "src/main/resources/META-INF/services/com.shaft.plugin.Listener",
                    "src/main/resources/properties/custom.properties",
                ],
                listed,
            )
            self.assertNotIn("pom.xml", listed)
            self.assertNotIn("src/ok.py", listed)
            batches = module.include_ignored_batches(listed, max_chars=80)
            self.assertGreater(len(batches), 1)
            joined = ",".join(batches)
            for path in listed:
                self.assertIn(path, joined)

    def test_missing_git_repo_returns_no_paths(self):
        module = self.load()
        with tempfile.TemporaryDirectory() as temporary:
            self.assertEqual([], module.list_promote_paths(Path(temporary)))


if __name__ == "__main__":
    unittest.main()
