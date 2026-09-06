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

    def test_property_backed_dependency_version_change_requires_review(self):
        before = """
        <project>
          <properties><google.auth.library.version>1.0.0</google.auth.library.version></properties>
          <dependencies>
            <dependency>
              <groupId>com.google.auth</groupId>
              <artifactId>google-auth-library-oauth2-http</artifactId>
              <version>${google.auth.library.version}</version>
            </dependency>
          </dependencies>
        </project>
        """
        after = before.replace("1.0.0", "1.1.0")
        with patch("scripts.ci.dependency_review_changes.git", return_value="pom.xml\n"):
            with patch(
                "scripts.ci.dependency_review_changes.content",
                side_effect=[before, after],
            ):
                self.assertTrue(dependency_review_changes.needs_review("base", "head"))

    def test_imported_bom_property_version_change_requires_review(self):
        before = """
        <project>
          <properties><bom.version>1.0.0</bom.version></properties>
          <dependencyManagement>
            <dependencies>
              <dependency>
                <groupId>org.example</groupId>
                <artifactId>bom</artifactId>
                <version>${bom.version}</version>
                <type>pom</type>
                <scope>import</scope>
              </dependency>
            </dependencies>
          </dependencyManagement>
        </project>
        """
        after = before.replace("1.0.0", "2.0.0")
        with patch("scripts.ci.dependency_review_changes.git", return_value="pom.xml\n"):
            with patch(
                "scripts.ci.dependency_review_changes.content",
                side_effect=[before, after],
            ):
                self.assertTrue(dependency_review_changes.needs_review("base", "head"))

    def test_unrelated_property_change_skips_dependency_review(self):
        before = """
        <project>
          <properties>
            <google.auth.library.version>1.0.0</google.auth.library.version>
            <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
          </properties>
          <dependencies>
            <dependency>
              <groupId>com.google.auth</groupId>
              <artifactId>google-auth-library-oauth2-http</artifactId>
              <version>${google.auth.library.version}</version>
            </dependency>
          </dependencies>
        </project>
        """
        after = before.replace("UTF-8", "UTF-16")
        with patch("scripts.ci.dependency_review_changes.git", return_value="pom.xml\n"):
            with patch(
                "scripts.ci.dependency_review_changes.content",
                side_effect=[before, after],
            ):
                self.assertFalse(dependency_review_changes.needs_review("base", "head"))

    def test_changed_ci_python_requirements_requires_dependency_review(self):
        with patch(
            "scripts.ci.dependency_review_changes.git",
            return_value="requirements-ci.txt\n",
        ):
            self.assertTrue(dependency_review_changes.needs_review("base", "head"))
