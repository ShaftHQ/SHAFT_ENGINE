"""Shipped lesson statements for the non-blocked harness issues.

The needles below are the acceptance phrases. The text under test is the
file each issue named, read from this checkout, not a fixture copy.
"""

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def read(relative):
    return " ".join((ROOT / relative).read_text(encoding="utf-8").split())


class NonblockedLessonTests(unittest.TestCase):
    def test_skill_forbids_a_repository_that_answers_for_other_coordinates(self):
        skill = read("chaos-engine/skills/chaos-engine/SKILL.md")
        self.assertIn(
            "forbids a maven repository that returns 200 for any coordinate "
            "other than the intended artifact",
            skill,
        )

    def test_personality_rejects_an_in_tree_copy_as_an_upstream_adapter(self):
        phrase = "An in-tree copy is not a resolved upstream adapter"
        for relative in (
            "chaos-engine/identity.md",
            "chaos-engine/companions/caveman-ultra.md",
            "chaos-engine/companions/ponytail-ultra.md",
        ):
            with self.subTest(relative=relative):
                self.assertIn(phrase, read(relative))
        identity = read("chaos-engine/identity.md")
        truth = identity.split("<!-- CHAOSENGINE-IDENTITY-TRUTH:START -->", 1)[1]
        truth = truth.split("<!-- CHAOSENGINE-IDENTITY-TRUTH:END -->", 1)[0]
        self.assertNotIn(phrase, truth)

    def test_kernel_check_rejects_a_warm_maven_cache(self):
        kernel = read("chaos-engine/hooks/kernel.py")
        self.assertIn(
            "A warm maven cache is not proof of a newly declared repository",
            kernel,
        )
        self.assertIn("jitpack.io", kernel)
        self.assertIn("docker-java", kernel)
        self.assertIn('"Check": ("Act", "Blocked")', kernel)

    def test_playbook_records_body_edits_and_the_live_workflow_triggers(self):
        playbook = read("chaos-engine/references/work-github-playbook.md")
        self.assertIn(
            "a body-only edit does not re-run PR Gate Summary or Release-note governance",
            playbook,
        )
        self.assertIn("pr-gate.yml", playbook)
        self.assertIn("has no `edited` type", playbook)
        self.assertIn("release-note-governance.yml", playbook)
        self.assertIn("includes `edited`", playbook)
        self.assertIn("does re-run Release-note governance", playbook)

    def test_java_tests_record_testng_assert_throws_void(self):
        rules = read("shaft-skills/ce-pack/references/playbooks/java-tests.md")
        self.assertIn("org.testng.Assert.assertThrows` returns void", rules)
        self.assertIn("`expectThrows` returns the throwable", rules)

    def test_adapter_publish_records_the_jitpack_single_module_coordinate(self):
        script = read("allure-cucumber8-jvm/prepare_upstream_adapter.py")
        self.assertIn("BUILD SUCCESSFUL", script)
        self.assertIn(
            "com.github.MohabMohie:allure-java:a1df39fffb1389ecbce951d617b5da3c49bb1a3a",
            script,
        )
        self.assertIn("does not add jitpack.io", script)


if __name__ == "__main__":
    unittest.main()
