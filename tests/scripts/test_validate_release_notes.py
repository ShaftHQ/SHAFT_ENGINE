"""Release-note configuration and pull-request classification tests."""

from __future__ import annotations

import unittest
from pathlib import Path

from scripts.ci import validate_release_notes


ROOT = Path(__file__).resolve().parents[2]


class ValidateReleaseNotesTest(unittest.TestCase):
    def test_repository_release_configuration_is_curated(self):
        self.assertEqual(
            [],
            validate_release_notes.release_config_errors(ROOT / ".github/release.yml"),
        )

    def test_human_pull_request_requires_exactly_one_classification(self):
        for label in validate_release_notes.CLASSIFICATION_LABELS:
            with self.subTest(label=label):
                self.assertEqual([], validate_release_notes.pull_request_errors(self.event(label)))

        self.assertEqual(1, len(validate_release_notes.pull_request_errors(self.event())))
        self.assertEqual(
            1,
            len(validate_release_notes.pull_request_errors(self.event("bug", "enhancement"))),
        )

    def test_regression_is_supplemental_not_a_classification(self):
        self.assertEqual(
            [], validate_release_notes.pull_request_errors(self.event("bug", "regression"))
        )
        self.assertEqual(
            1, len(validate_release_notes.pull_request_errors(self.event("regression")))
        )

    def test_bots_and_non_pull_request_events_are_exempt(self):
        bot = self.event()
        bot["pull_request"]["user"] = {"login": "dependabot[bot]", "type": "Bot"}
        self.assertEqual([], validate_release_notes.pull_request_errors(bot))
        self.assertEqual([], validate_release_notes.pull_request_errors({"ref": "refs/heads/main"}))

    @staticmethod
    def event(*labels: str) -> dict:
        return {
            "pull_request": {
                "user": {"login": "contributor", "type": "User"},
                "labels": [{"name": label} for label in labels],
            }
        }


if __name__ == "__main__":
    unittest.main()
