"""Generated-project workflow templates must not trail the repository's own action versions (#6839)."""

from __future__ import annotations

import tempfile
import unittest
import unittest.mock
from pathlib import Path

from scripts.ci.overlay_pre_push import overlay_pre_push_failures
from scripts.ci.workflow_template_sync import (
    MAIN_WORKFLOWS,
    TEMPLATE_WORKFLOWS,
    stale_template_actions,
    touches_workflows,
)

ROOT = Path(__file__).resolve().parents[2]


def _write(root: Path, directory: str, name: str, *uses: str) -> None:
    folder = root / directory
    folder.mkdir(parents=True, exist_ok=True)
    body = "jobs:\n  build:\n    steps:\n" + "".join(f"      - uses: {item}\n" for item in uses)
    (folder / name).write_text(body, encoding="utf-8")


class WorkflowTemplateSyncTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.root = Path(self._tmp.name)

    def test_older_template_pin_is_reported(self):
        _write(self.root, MAIN_WORKFLOWS, "ci.yml", "actions/upload-artifact@v7.0.2")
        _write(self.root, TEMPLATE_WORKFLOWS, "web.yml", "actions/upload-artifact@v7.0.1")
        self.assertEqual(
            ["actions/upload-artifact: template pins v7.0.1 but the repository now uses v7.0.2"],
            stale_template_actions(self.root),
        )

    def test_sha_pin_compares_its_tag_comment(self):
        _write(self.root, MAIN_WORKFLOWS, "ci.yml", "actions/upload-artifact@" + "b" * 40 + " # v7.0.2")
        _write(self.root, TEMPLATE_WORKFLOWS, "web.yml", "actions/upload-artifact@v7.0.1")
        self.assertEqual(
            ["actions/upload-artifact: template pins v7.0.1 but the repository now uses v7.0.2"],
            stale_template_actions(self.root),
        )

    def test_equal_newer_sha_local_and_template_only_actions_are_ignored(self):
        _write(self.root, MAIN_WORKFLOWS, "ci.yml", "actions/checkout@v7", "actions/cache@" + "a" * 40, "./.github/actions/x")
        _write(self.root, TEMPLATE_WORKFLOWS, "api.yml", "actions/checkout@v7.0.0", "actions/cache@v4", "only/template@v1")
        self.assertEqual([], stale_template_actions(self.root))
        _write(self.root, TEMPLATE_WORKFLOWS, "newer.yml", "actions/checkout@v8")
        self.assertEqual([], stale_template_actions(self.root))

    def test_missing_directories_are_not_a_failure(self):
        self.assertEqual([], stale_template_actions(self.root))

    def test_touch_matrix_covers_both_workflow_directories_only(self):
        self.assertTrue(touches_workflows(".github/workflows/pr-gate.yml"))
        self.assertTrue(touches_workflows(TEMPLATE_WORKFLOWS + "/web.yml"))
        self.assertFalse(touches_workflows(".github/actions/cache-maven-repo/action.yml"))

    def test_pre_push_refuses_a_workflow_diff_that_leaves_templates_stale(self):
        _write(self.root, MAIN_WORKFLOWS, "ci.yml", "actions/upload-artifact@v7.0.2")
        _write(self.root, TEMPLATE_WORKFLOWS, "web.yml", "actions/upload-artifact@v7.0.1")
        failures = overlay_pre_push_failures(self.root, [".github/workflows/ci.yml"])
        self.assertTrue(any("workflow template drift" in item for item in failures), failures)
        self.assertEqual([], overlay_pre_push_failures(self.root, ["README.md"]))

    def test_pre_push_flags_a_script_test_module_the_pr_gate_does_not_select(self):
        from scripts.ci import overlay_pre_push

        ungated = "tests.scripts.test_brand_new_module"
        failures = ["AssertionError: Lists differ: [] != ['" + ungated + "']"]
        with unittest.mock.patch.object(overlay_pre_push, "ungated_script_module_failures", return_value=failures):
            result = overlay_pre_push.overlay_pre_push_failures(self.root, ["tests/scripts/test_brand_new_module.py"])
        self.assertEqual(failures, result)

    def test_live_repository_selects_every_script_test_module(self):
        from scripts.ci.overlay_pre_push import ungated_script_module_failures

        self.assertEqual([], ungated_script_module_failures(ROOT))

    def test_live_repository_templates_are_in_sync(self):
        self.assertEqual([], stale_template_actions(ROOT))


if __name__ == "__main__":
    unittest.main()
