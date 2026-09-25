"""Minimal release-notes renderer tests (issue #6232)."""

from __future__ import annotations

import contextlib
import io
import json
import tempfile
import unittest
import unittest.mock
from pathlib import Path

from scripts.ci import render_release_notes as notes
from scripts.ci import validate_release_notes


ROOT = Path(__file__).resolve().parents[2]
TEMPLATE = (
    "# SHAFT $RELEASE_VERSION\n\n$RELEASE_SUMMARY\n\n"
    "<version>$RELEASE_VERSION</version>\n\n$RELEASE_CHANGES\n\n$RELEASE_CHANGELOG\n"
)
ENGINE_FILE = ("shaft-engine/src/main/java/com/shaft/Foo.java",)


def pull(number, title, labels=(), author="contributor", files=ENGINE_FILE):
    return notes.PullRequest(number, title, author, frozenset(labels), tuple(files))


class CleanTitleTest(unittest.TestCase):
    def test_strips_tags_prefixes_and_trailing_references(self):
        cases = {
            "[CE] Fix win32 installer verify (#5703) (#5704)": "Fix win32 installer verify",
            "feat(mobile): flutter locator parity": "Flutter locator parity",
            "fix: Flutter Allure path (#5721)": "Flutter Allure path",
            "Installer self-heal (#5630/#5629)": "Installer self-heal",
            "chore(deps): bump io.cucumber:cucumber-bom in /": "Bump io.cucumber:cucumber-bom",
            "Fail doctor when overlay differs.": "Fail doctor when overlay differs",
        }
        for raw, expected in cases.items():
            with self.subTest(raw=raw):
                self.assertEqual(expected, notes.clean_title(raw))

    def test_truncates_long_titles(self):
        title = notes.clean_title("feat: " + "x" * 300)
        self.assertEqual(notes.MAX_TITLE_LENGTH, len(title))
        self.assertTrue(title.endswith("…"))


class ClassifyTest(unittest.TestCase):
    def test_groups_user_facing_changes_by_label_and_title(self):
        classified = notes.classify(
            [
                pull(1, "feat: new locator", ["enhancement"]),
                pull(2, "fix(engine): crash", ["bug"]),
                pull(3, "Faster waits", ["enhancement", "performance"]),
                pull(4, "Old API", ["enhancement", "deprecation"]),
                pull(5, "Drop Java 17", ["breaking-change"], files=("pom.xml",)),
                pull(6, "perf: cache drivers"),
            ]
        )
        expected = {
            "feature": [1],
            "fix": [2],
            "performance": [3, 6],
            "deprecation": [4],
            "breaking": [5],
        }
        sections = classified.sections
        actual = {key: [item.number for item in items] for key, items in sections.items()}
        self.assertEqual(expected, actual)
        self.assertEqual([], classified.internal)

    def test_internal_work_is_collapsed(self):
        classified = notes.classify(
            [
                pull(1, "[CE] Installer polish", ["enhancement"]),
                pull(2, "ci(mobile): re-enable Flutter E2E", ["enhancement"]),
                pull(3, "feat(ce): self-improve", ["enhancement"]),
                pull(4, "Tidy harness", ["enhancement", "subsystem:agent-harness"]),
                pull(5, "Fix MemPalace startup", ["bug"]),
                pull(6, "Better workflow", ["enhancement"], files=(".github/workflows/x.yml",)),
                pull(7, "test(engine): stabilize", ["enhancement"]),
            ]
        )
        self.assertEqual({}, classified.sections)
        self.assertEqual([1, 2, 3, 4, 5, 6, 7], [item.number for item in classified.internal])

    def test_skipped_and_release_preparation_pull_requests_are_dropped(self):
        classified = notes.classify(
            [
                pull(1, "Revert thing", ["skip-release-notes"]),
                pull(2, "Prepare SHAFT Engine release 1.2.3", ["release"], "app/github-actions"),
            ]
        )
        self.assertEqual({}, classified.sections)
        self.assertEqual([], classified.internal)
        self.assertEqual(0, classified.dependency_updates)

    def test_dependency_bumps_are_counted_unless_notable(self):
        classified = notes.classify(
            [
                pull(1, "chore(deps): bump com.google.guava:guava in /", author="app/dependabot"),
                pull(2, "Bump org.slf4j:slf4j-bom from 2.0.1 to 2.0.2", ["dependencies"]),
                pull(3, "Bump org.seleniumhq.selenium:selenium-java from 4.30.0 to 5.0.0"),
                pull(4, "Bump io.appium:java-client from 9.1.0 to 9.2.0", author="dependabot[bot]"),
                pull(5, "Bump com.microsoft.playwright:playwright in /", author="dependabot"),
                pull(6, "Bump jackson from 2.0 to 2.1", ["dependencies", "security"]),
            ]
        )
        self.assertEqual([3, 5, 6], [item.number for item in classified.sections["dependency"]])
        self.assertEqual(3, classified.dependency_updates)


class RenderTest(unittest.TestCase):
    def test_minimal_body_has_summary_snippet_sections_and_collapsed_internals(self):
        body = notes.render_release(
            "1.2.3",
            [
                pull(10, "feat(mobile): Flutter locator parity (#9)", ["enhancement"]),
                pull(11, "fix: Allure path", ["bug"]),
                pull(12, "[CE] Installer polish", ["enhancement"]),
                pull(13, "chore(deps): bump guava in /", author="dependabot"),
            ],
            "1.2.2",
            template=TEMPLATE,
        )
        summary = "# SHAFT 1.2.3\n\nThis release brings 1 new feature and 1 fix"
        self.assertTrue(body.startswith(summary))
        self.assertIn("<version>1.2.3</version>", body)
        self.assertIn("## New features\n\n- Flutter locator parity (#10)", body)
        self.assertIn("## Fixes\n\n- Allure path (#11)", body)
        self.assertNotIn("## Performance", body)
        self.assertIn("<summary>Internal changes (2)", body)
        self.assertIn("- Installer polish (#12)\n- 1 dependency update\n", body)
        self.assertIn("compare/1.2.2...1.2.3", body)
        self.assertNotIn("@", body)
        self.assertNotIn("$RELEASE", body)
        self.assertNotIn("https://github.com/ShaftHQ/SHAFT_ENGINE/pull/", body)

    def test_breaking_changes_come_first_with_upgrade_guide(self):
        body = notes.render_release(
            "2.0.0",
            [pull(1, "feat: x", ["enhancement"]), pull(2, "Remove old API", ["breaking-change"])],
            "1.9.0",
            template=TEMPLATE,
        )
        self.assertIn("**Heads-up:** 1 breaking change", body)
        self.assertLess(body.index("## Breaking changes"), body.index("## New features"))
        self.assertIn(notes.UPGRADE_GUIDE, body)

    def test_duplicate_titles_merge_into_one_line(self):
        body = notes.render_release(
            "1.0.1",
            [pull(1, "fix: flaky click", ["bug"]), pull(2, "fix: flaky click (#1)", ["bug"])],
            "1.0.0",
            template=TEMPLATE,
        )
        self.assertIn("- Flaky click (#1, #2)", body)

    def test_maintenance_release_and_highlights(self):
        maintenance = notes.render_summary(notes.classify([pull(1, "[CE] x", ["enhancement"])]))
        self.assertTrue(maintenance.startswith("Maintenance release"))
        busy = notes.render_summary(
            notes.classify([pull(n, f"feat: feature {n}", ["enhancement"]) for n in range(1, 7)])
        )
        self.assertIn("6 new features", busy)
        self.assertIn("Highlights: Feature 1; Feature 2; Feature 3.", busy)
        self.assertLessEqual(len(busy.splitlines()), 3)

    def test_repository_template_renders_cleanly(self):
        body = notes.render_release("9.9.9", [pull(1, "feat: x", ["enhancement"])], "9.9.8")
        self.assertIn('implementation("io.github.shafthq:shaft-engine:9.9.9")', body)
        self.assertNotIn("$RELEASE", body)
        self.assertNotIn("\n\n\n", body)


class CollectTest(unittest.TestCase):
    def fake_runner(self, calls):
        graphql = {
            "data": {
                "repository": {
                    "pr5": {
                        "number": 5,
                        "title": "feat: a",
                        "author": {"login": "someone"},
                        "labels": {"nodes": [{"name": "enhancement"}]},
                        "files": {"nodes": [{"path": ENGINE_FILE[0]}]},
                    },
                    "pr7": None,
                }
            }
        }

        def runner(command):
            calls.append(list(command))
            if command[:2] == ["gh", "release"]:
                return "1.0.0\n"
            if command[0] == "git":
                return "feat: a (#5)\nMerge pull request #7 from f/b\nno ref\nfeat: a (#5)\n"
            return json.dumps(graphql)

        return runner

    def test_collects_first_parent_pull_requests_since_latest_release(self):
        calls = []
        with tempfile.TemporaryDirectory() as temp_dir:
            template = Path(temp_dir) / "template.md"
            template.write_text(TEMPLATE, encoding="utf-8")
            body, fallback = notes.build_release_body(
                notes.ReleaseRequest("1.1.0", template, "Owner/Repo"), self.fake_runner(calls)
            )
        self.assertFalse(fallback)
        self.assertIn("- A (#5)", body)
        self.assertIn("compare/1.0.0...1.1.0", body)
        git_log = ["git", "log", "--first-parent", "--reverse", "--format=%s", "1.0.0..HEAD"]
        self.assertEqual(git_log, calls[1])
        self.assertIn("pr5: pullRequest(number: 5)", calls[2][4])
        self.assertIn("pr7: pullRequest(number: 7)", calls[2][4])

    def test_falls_back_to_generated_notes_when_collection_fails(self):
        def failing(_command):
            raise RuntimeError("gh unavailable")

        with tempfile.TemporaryDirectory() as temp_dir, contextlib.redirect_stderr(io.StringIO()):
            template = Path(temp_dir) / "template.md"
            template.write_text(TEMPLATE, encoding="utf-8")
            body, fallback = notes.build_release_body(
                notes.ReleaseRequest("1.1.0", template, previous_tag="1.0.0"), failing
            )
        self.assertTrue(fallback)
        self.assertIn("generated change list", body)
        self.assertIn("compare/1.0.0...1.1.0", body)

    def test_main_writes_body_and_github_output(self):
        offline = unittest.mock.Mock(side_effect=RuntimeError("offline"))
        with tempfile.TemporaryDirectory() as temp_dir, unittest.mock.patch.object(
            notes, "run_command", offline
        ), contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            output = Path(temp_dir) / "body.md"
            github_output = Path(temp_dir) / "out.txt"
            arguments = ["--version", "1.0.0", "--previous-tag", "0.9.0", "--output", str(output)]
            exit_code = notes.main([*arguments, "--github-output", str(github_output)])
            body = output.read_text(encoding="utf-8")
            flag = github_output.read_text(encoding="utf-8")
        self.assertEqual(0, exit_code)
        self.assertIn("# SHAFT 1.0.0", body)
        self.assertEqual("fallback=true\n", flag)


class SlackAnnouncementTest(unittest.TestCase):
    """#6241: Slack reuses the rendered summary line plus a link."""

    URL = "https://github.com/ShaftHQ/SHAFT_ENGINE/releases/tag/1.2.3"

    def test_summary_is_the_text_between_heading_and_snippet(self):
        body = notes.render_release(
            "1.2.3", [pull(1, "feat: x", ["enhancement"])], "1.2.2", template=TEMPLATE
        )
        line = "This release brings 1 new feature for SHAFT users."
        self.assertEqual(line, notes.release_summary(f"# SHAFT 1.2.3\n\n{line}\n\n```xml\n"))
        self.assertEqual("", notes.release_summary("# SHAFT 1.2.3\n\n```xml\n"))
        self.assertIn("1 new feature", notes.release_summary(body))

    def test_payload_uses_summary_and_link_without_contributor_promise(self):
        body = "# SHAFT 1.2.3\n\n**Heads-up:** 1 breaking change.\nHighlights: X.\n\n```xml\n"
        payload = notes.slack_payload("1.2.3", self.URL, body)
        section = payload["blocks"][1]["text"]["text"]
        expected = f"*Heads-up:* 1 breaking change. Highlights: X. <{self.URL}|Release notes>"
        self.assertEqual(expected, section)
        self.assertEqual(self.URL, payload["blocks"][2]["elements"][0]["url"])
        self.assertNotIn("contributor", json.dumps(payload))

    def test_fallback_body_gets_a_neutral_summary(self):
        fallback = notes.render_fallback("1.2.3", "1.2.2")
        section = notes.slack_payload("1.2.3", self.URL, fallback)["blocks"][1]["text"]["text"]
        self.assertTrue(section.startswith(notes.SLACK_FALLBACK_SUMMARY))

    def test_release_workflow_uses_the_shared_helper(self):
        workflow = (ROOT / ".github/workflows/mavenCentral_cd.yml").read_text(encoding="utf-8")
        self.assertIn("from scripts.ci.render_release_notes import slack_payload", workflow)
        self.assertIn('Path("/tmp/release_body.md")', workflow)
        self.assertNotIn("new contributors", workflow)


class GovernanceParityTest(unittest.TestCase):
    """The renderer and the Release-note governance check share one label taxonomy."""

    def test_renderer_labels_are_governed(self):
        validator = validate_release_notes
        governed = validator.CLASSIFICATION_LABELS | validator.SUPPLEMENTAL_LABELS
        section_labels = notes.CLASSIFICATION_SECTIONS + notes.SUPPLEMENTAL_SECTION_LABELS
        self.assertLessEqual({label for label, _section in section_labels}, governed)
        self.assertIn(notes.SKIP_LABEL, validator.CLASSIFICATION_LABELS)
        supplemental = {label for label, _section in notes.SUPPLEMENTAL_SECTION_LABELS}
        self.assertEqual(supplemental | {"regression"}, set(validator.SUPPLEMENTAL_LABELS))

    def test_repository_template_is_valid(self):
        template = ROOT / ".github/RELEASE_BODY_TEMPLATE.md"
        self.assertEqual([], validate_release_notes.release_template_errors(template))


if __name__ == "__main__":
    unittest.main()
