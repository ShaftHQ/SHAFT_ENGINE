import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
AUTHORING = ROOT / "shaft-skills" / "shaft-automated-test-authoring" / "references" / "playbook.md"
LOCATOR = ROOT / "shaft-skills" / "shaft-locator-design" / "references" / "playbook.md"
PAGE_OBJECTS = ROOT / "shaft-skills" / "shaft-page-objects" / "references" / "playbook.md"
CODEGEN = ROOT / "shaft-skills" / "shaft-recording-codegen" / "references" / "playbook.md"
BANNED_RECOMMENDED = (
    "SHAFT.GUI.Locator.xpath(",
    "SHAFT.GUI.Locator.id(",
    "SHAFT.GUI.Locator.name(",
    "SHAFT.GUI.Locator.cssSelector(",
    "SHAFT.GUI.Locator.className(",
    "SHAFT.GUI.Locator.tagName(",
)


def _valid_examples(content: str) -> str:
    marker = "## Valid examples"
    start = content.index(marker)
    boundary = content.find("## Boundary", start)
    return content[start:boundary]


class AutomatedTestAuthoringContentTest(unittest.TestCase):
    def test_authoring_is_grounded_guarded_and_free_of_raw_driver_shortcuts(self):
        content = AUTHORING.read_text(encoding="utf-8")

        self.assertIn("Ground unfamiliar SHAFT syntax with `shaft_guide_search`", content)
        self.assertIn("never add `Thread.sleep`, raw driver calls", content)
        self.assertIn("Run `test_code_guardrails_check`", content)


class LocatorAndCodegenContentTest(unittest.TestCase):
    def test_locator_playbook_teaches_the_three_tier_builder(self):
        content = LOCATOR.read_text(encoding="utf-8")
        self.assertIn("hasAnyTagName().hasId(", content)
        self.assertIn("hasRole(", content)
        self.assertIn("By.xpath(", content)
        self.assertIn("test_code_guardrails_check", content)
        examples = _valid_examples(content)
        for banned in BANNED_RECOMMENDED:
            self.assertNotIn(banned, examples, banned)

    def test_page_objects_keep_locators_on_the_three_tier_builder(self):
        content = PAGE_OBJECTS.read_text(encoding="utf-8")
        self.assertIn("hasAnyTagName().hasId(", content)
        self.assertIn("hasRole(", content)
        self.assertNotIn("SHAFT.GUI.Locator.xpath(", content)

    def test_recording_codegen_still_requires_guardrails(self):
        content = CODEGEN.read_text(encoding="utf-8")
        self.assertIn("test_code_guardrails_check", content)


if __name__ == "__main__":
    unittest.main()
