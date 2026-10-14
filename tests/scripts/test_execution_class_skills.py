"""The execution-class lessons are in every skill the issues name."""

from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
SKILLS = ROOT / "shaft-skills"


def text(relative: str) -> str:
    return (SKILLS / relative).read_text(encoding="utf-8")


class ExecutionClassSkillTest(unittest.TestCase):
    def test_named_skills_state_each_rule(self):
        required = {
            "shaft-test-execution/references/playbook.md": [
                "default run is the test being fixed",
                "consumer contract",
                "Do not restart a cohort",
                "Surefire or TestNG report",
                "Maven exit 0 is not success",
                "comma-separated class list",
                "Do not add a TestNG XML",
                "One watcher",
                "unclassified skip",
                "pressure-or-wedge",
                "one Appium session and one Maven run",
                "allure.cleanResultsDirectory",
                "target/allure-results-kept",
                "Decode base64 report blobs",
                "must not echo the secret",
                "plaintext grep is not success",
                "browserstack.yml",
                "all-green filter must not",
                "Done",
            ],
            "shaft-test-monitoring/references/playbook.md": [
                "default run is the test being fixed",
                "consumer contract",
                "Do not restart a cohort",
                "Surefire or TestNG report",
                "One watcher",
                "Do not add a TestNG XML",
            ],
            "shaft-developer/SKILL.md": [
                "exactly one",
                "no skill",
                "shaft-web-actions",
                "Do not add a TestNG XML",
                "-Dtest",
            ],
            "shaft-developer/references/routing.md": [
                "no skill",
                "shaft-web-actions",
                "Load exactly this skill",
            ],
            "shaft-cli/references/playbook.md": [
                "pressure-or-wedge",
                "-no-snapshot-load",
                "-no-snapshot-save` is not a cold boot",
                "one Appium session and one Maven run",
                "No host qemu time-versus-memory measurement",
            ],
            "shaft-test-environment/references/playbook.md": [
                "pressure-or-wedge",
                "versionCode",
                "Do not compile the app",
                "iphonesimulator",
                "immutable uploaded build id",
                "x86_64",
                "one Appium session and one Maven run",
            ],
            "shaft-fluent-api/references/playbook.md": [
                "part of the thrown `AssertionError`",
                "accumulated soft-assert",
                "forceFail",
                "does not throw",
            ],
            "shaft-locator-design/references/playbook.md": [
                "not a skip",
                "Content description",
                "not(...)",
            ],
            "shaft-mobile-actions/references/playbook.md": [
                "not a skip",
                "not(...)",
                "in-memory count",
                "#6940",
                "Do not claim that it did",
                "#6908",
                "visible `text`",
                "content-desc",
                "hideKeyboard",
                "unguarded system Back",
                "screenshot",
                "isPresent(By, Duration)",
                "getElementsCount",
            ],
            "shaft-test-data-design/references/playbook.md": [
                "in-memory count",
                "when save is the scenario",
            ],
            "shaft-failure-analysis/references/playbook.md": [
                "#6940",
                "Do not claim that it did",
                "#6908",
                "null container",
            ],
            "shaft-assertions/references/playbook.md": [
                "visible `text`",
                "matchesReferenceImageWhenStable",
                "matchesReferenceImage",
                "one resolution",
                "isPresent(By, Duration)",
                "whitespace-only text is not non-empty",
                "before state",
                "stated way to fail",
            ],
            "shaft-execution-reporting/references/playbook.md": [
                "historyId",
                "testCaseId",
                "whole tokens",
                "Decode base64 report blobs",
                "must not echo the secret",
                "suite widgets may be base64",
                "plaintext miss is not an empty report",
                "#3407",
                "allure.cleanResultsDirectory",
                "target/allure-results-kept",
                "lists each test name",
                "does not match the visible names",
            ],
            "shaft-defect-reporting/references/playbook.md": [
                "whole tokens",
                "#6947",
            ],
            "ce-pack/references/shaft-mastery/allure-internals.md": [
                "include suite identity",
                "suite widgets may be base64",
                "plaintext miss is not an empty report",
                "#3407",
                "Do not `Files.readString`",
            ],
            "shaft-stakeholder-reporting/references/playbook.md": [
                "failed with a named id",
                "all-green filter must not",
                "passing run",
                "lists each test name",
                "does not match the visible names",
            ],
            "shaft-test-case-design/references/journey-coverage-map.md": [
                "delete",
                "cancel as a final action",
                "state-changing API",
                "Done needs a passing run with platform, language, and date.",
            ],
        }
        for relative, phrases in required.items():
            body = text(relative)
            for phrase in phrases:
                self.assertIn(phrase, body, f"{relative} missing {phrase!r}")

    def test_developer_entry_does_not_link_other_specialists(self):
        body = text("shaft-developer/SKILL.md")
        self.assertNotIn("](../shaft-", body)
        self.assertIn("references/routing.md", body)

    def test_no_shipped_skill_tells_the_agent_to_add_a_testng_xml(self):
        for path in SKILLS.rglob("*.md"):
            for line in path.read_text(encoding="utf-8").splitlines():
                lowered = line.lower()
                if "testng" not in lowered or "xml" not in lowered:
                    continue
                instructs_new_file = (
                    "add a testng xml" in lowered
                    or "add a new testng" in lowered
                    or "create a testng xml" in lowered
                    or "create a new testng" in lowered
                    or "new testng xml" in lowered
                )
                negated = "do not" in lowered or "must not" in lowered or "never " in lowered
                self.assertFalse(instructs_new_file and not negated, f"{path.relative_to(ROOT)}: {line}")


if __name__ == "__main__":
    unittest.main()
