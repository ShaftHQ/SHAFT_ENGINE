from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "ci" / "dependency_review_changes.py"


class DependencyReviewChangesTest(unittest.TestCase):
    def classify(self, before: str, after: str, path: str = "pom.xml") -> str:
        with tempfile.TemporaryDirectory() as directory:
            repository = Path(directory)
            subprocess.run(["git", "init", "--quiet"], cwd=repository, check=True)
            subprocess.run(["git", "config", "user.email", "test@example.invalid"], cwd=repository, check=True)
            subprocess.run(["git", "config", "user.name", "Test"], cwd=repository, check=True)
            target = repository / path
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(before, encoding="utf-8")
            subprocess.run(["git", "add", "."], cwd=repository, check=True)
            subprocess.run(["git", "commit", "--quiet", "-m", "before"], cwd=repository, check=True)
            base = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repository, text=True).strip()
            target.write_text(after, encoding="utf-8")
            subprocess.run(["git", "add", "."], cwd=repository, check=True)
            subprocess.run(["git", "commit", "--quiet", "-m", "after"], cwd=repository, check=True)
            head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repository, text=True).strip()
            result = subprocess.run(
                [sys.executable, str(SCRIPT), base, head],
                cwd=repository,
                capture_output=True,
                text=True,
                check=False,
            )
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.strip()

    def test_release_version_only_pom_edit_skips_dependency_review(self):
        self.assertEqual(
            "false",
            self.classify(
                "<project><version>1.0.0</version></project>",
                "<project><version>1.0.1</version></project>",
            ),
        )

    def test_changed_maven_dependency_requires_dependency_review(self):
        self.assertEqual(
            "true",
            self.classify(
                "<project><dependencies><dependency><groupId>a</groupId><artifactId>b</artifactId><version>1</version></dependency></dependencies></project>",
                "<project><dependencies><dependency><groupId>a</groupId><artifactId>b</artifactId><version>2</version></dependency></dependencies></project>",
            ),
        )
