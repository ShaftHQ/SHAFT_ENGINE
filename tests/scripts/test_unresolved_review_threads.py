"""Unresolved review-thread gate for merge-queue enqueue (#7736)."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "unresolved_review_threads", ROOT / "scripts/ci/unresolved_review_threads.py"
)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


class UnresolvedReviewThreadsTest(unittest.TestCase):
    def test_filters_resolved_and_reports_open(self):
        payload = {
            "data": {
                "repository": {
                    "pullRequest": {
                        "reviewThreads": {
                            "nodes": [
                                {
                                    "isResolved": True,
                                    "isOutdated": False,
                                    "path": "a.java",
                                    "comments": {"nodes": [{"body": "fixed", "author": {"login": "bot"}}]},
                                },
                                {
                                    "isResolved": False,
                                    "isOutdated": True,
                                    "path": "b.java",
                                    "comments": {
                                        "nodes": [
                                            {
                                                "body": "## Useless comparison\n\nalways true",
                                                "author": {"login": "github-code-quality"},
                                            }
                                        ]
                                    },
                                },
                            ]
                        }
                    }
                }
            }
        }
        open_threads = MODULE.unresolved_threads(payload)
        self.assertEqual(len(open_threads), 1)
        self.assertEqual(open_threads[0]["path"], "b.java")
        self.assertEqual(open_threads[0]["author"], "github-code-quality")
        self.assertIn("Useless comparison", open_threads[0]["summary"])

    def test_cli_fixture_exits_1_when_open(self):
        payload = {
            "data": {
                "repository": {
                    "pullRequest": {
                        "reviewThreads": {
                            "nodes": [
                                {
                                    "isResolved": False,
                                    "isOutdated": False,
                                    "path": "x.py",
                                    "comments": {"nodes": [{"body": "open", "author": {"login": "a"}}]},
                                }
                            ]
                        }
                    }
                }
            }
        }
        with tempfile.TemporaryDirectory() as tmp:
            fixture = Path(tmp) / "threads.json"
            fixture.write_text(json.dumps(payload), encoding="utf-8")
            self.assertEqual(MODULE.main(["--fixture", str(fixture)]), 1)
            payload["data"]["repository"]["pullRequest"]["reviewThreads"]["nodes"][0][
                "isResolved"
            ] = True
            fixture.write_text(json.dumps(payload), encoding="utf-8")
            self.assertEqual(MODULE.main(["--fixture", str(fixture)]), 0)


if __name__ == "__main__":
    unittest.main()
