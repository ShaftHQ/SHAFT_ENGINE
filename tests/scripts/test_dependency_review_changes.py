from __future__ import annotations

import unittest
from unittest.mock import patch

from scripts.ci import dependency_review_changes

class DependencyReviewChangesTest(unittest.TestCase):
    def test_release_version_only_pom_edit_skips_dependency_review(self):
        with patch("scripts.ci.dependency_review_changes.git", return_value="pom.xml\n"):
            with patch(
                "scripts.ci.dependency_review_changes.content",
                side_effect=[
                    "<project><version>1.0.0</version></project>",
                    "<project><version>1.0.1</version></project>",
                ],
            ):
                self.assertFalse(dependency_review_changes.needs_review("base", "head"))

    def test_unequal_width_child_parent_version_only_edit_skips_dependency_review(self):
        dependency = (
            "<dependencies><dependency><groupId>a</groupId><artifactId>b</artifactId>"
            "<version>1</version></dependency></dependencies>"
        )
        with patch(
            "scripts.ci.dependency_review_changes.git",
            return_value="shaft-engine/pom.xml\n",
        ):
            with patch(
                "scripts.ci.dependency_review_changes.content",
                side_effect=[
                    f"<project><parent><version>10.3.20260809</version></parent>{dependency}</project>",
                    f"<project><parent><version>10.3.202608091</version></parent>{dependency}</project>",
                ],
            ):
                self.assertFalse(dependency_review_changes.needs_review("base", "head"))

    def test_changed_maven_dependency_requires_dependency_review(self):
        with patch("scripts.ci.dependency_review_changes.git", return_value="pom.xml\n"):
            with patch(
                "scripts.ci.dependency_review_changes.content",
                side_effect=[
                    "<project><dependencies><dependency><groupId>a</groupId><artifactId>b</artifactId><version>1</version></dependency></dependencies></project>",
                    "<project><dependencies><dependency><groupId>a</groupId><artifactId>b</artifactId><version>2</version></dependency></dependencies></project>",
                ],
            ):
                self.assertTrue(dependency_review_changes.needs_review("base", "head"))

    def test_changed_ci_python_requirements_requires_dependency_review(self):
        with patch(
            "scripts.ci.dependency_review_changes.git",
            return_value="requirements-ci.txt\n",
        ):
            self.assertTrue(dependency_review_changes.needs_review("base", "head"))
