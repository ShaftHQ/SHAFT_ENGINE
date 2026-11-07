"""#7791: Agent Guidance contracts that failed only in CI run at pre-push."""

from __future__ import annotations

import subprocess  # nosec B404 - fixed interpreter argv, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402 - self-locating under python3 -I (#7197)

from scripts.ci import harness_pr_gate as gate  # noqa: E402
from scripts.ci import overlay_pre_push as prepush  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]
GUIDANCE_IDS = {
    "MergeQueueLessonTest.test_docs_and_skill",
    "LeanBootstrapTest.test_mandated_chain_budget_holds_on_every_host",
    "GuardReachabilityTest.test_documented_commands_exist_or_are_marked_repo_only",
    "NoDuplicationTest.test_no_substantive_line_is_repeated_across_guidance_files",
}
README_ID = (
    "ValidateDocumentationBoundariesTest."
    "test_rejects_unapproved_nested_readme_inside_allowed_markdown_tree"
)
JAVA = "shaft-engine/src/main/java/com/shaft/driver/SHAFT.java"
GATED = (
    "tests.scripts.test_ce_pre_push_guidance_7791",
    "tests.scripts.test_ce_harness_retrieve_7792",
    "tests.scripts.test_ce_enhancement_disposition_7793",
)


class GuidanceContractSelectionTest(unittest.TestCase):
    def test_markdown_and_readme_contracts_are_cheap_and_java_does_not_select_them(self) -> None:
        ids = {test_id for _predicate, _module, test_id in prepush.CHEAP_CI_CONTRACTS}
        self.assertLessEqual(GUIDANCE_IDS, ids, sorted(GUIDANCE_IDS - ids))
        self.assertIn(README_ID, ids)
        modules = {module for _predicate, module, _test_id in prepush.CHEAP_CI_CONTRACTS}
        self.assertFalse(any("test_ce_design" in module for module in modules))
        selected = {
            test_id
            for predicate, _module, test_id in prepush.CHEAP_CI_CONTRACTS
            if predicate(JAVA)
        }
        self.assertFalse(GUIDANCE_IDS & selected)
        self.assertNotIn(README_ID, selected)

    def test_new_modules_are_selected_by_a_harness_check(self) -> None:
        registered = {module for check in gate.CHECKS.values() for module in check.modules}
        missing = [name for name in GATED if name not in registered]
        self.assertEqual([], missing)
        self.assertFalse(set(GATED) & set(gate.ungated_script_modules(ROOT)))


class GuidanceContractFailureTest(unittest.TestCase):
    def test_dropping_awaiting_checks_fails_pre_push_with_that_needle(self) -> None:
        path = ROOT / "chaos-engine/references/ci-status-economy.md"
        original = path.read_text(encoding="utf-8")
        try:
            path.write_text(original.replace("AWAITING_CHECKS", "QUEUED_STATE"), encoding="utf-8")
            failures = prepush.cheap_ci_contract_failures(
                ROOT, ["chaos-engine/references/ci-status-economy.md"]
            )
        finally:
            path.write_text(original, encoding="utf-8")
        self.assertIn("AWAITING_CHECKS", "\n".join(failures))

    def test_bot_entry_chain_over_24576_bytes_fails(self) -> None:
        path = ROOT / "chaos-engine/references/bot-entry.md"
        original = path.read_text(encoding="utf-8")
        pad = "<!-- chain-budget-probe-7791 unique mandated-chain padding -->\n" * 800
        try:
            path.write_text(original + pad, encoding="utf-8")
            failures = prepush.cheap_ci_contract_failures(
                ROOT, ["chaos-engine/references/bot-entry.md"]
            )
        finally:
            path.write_text(original, encoding="utf-8")
        self.assertIn("24576", "\n".join(failures))

    def test_nested_chaos_engine_readme_fails_even_without_the_unit_module(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            nested = root / "chaos-engine" / "token-opt" / "README.md"
            nested.parent.mkdir(parents=True)
            nested.write_text("# token\n", encoding="utf-8")
            failures = prepush.cheap_ci_contract_failures(
                root, ["chaos-engine/token-opt/README.md"]
            )
        self.assertTrue(
            any("non-root README is prohibited: chaos-engine/token-opt/README.md" in item for item in failures),
            failures,
        )


class IsolatedProbeTest(unittest.TestCase):
    def test_only_the_changed_test_file_is_probed(self) -> None:
        seen: list[list[str]] = []

        def spy(argv, **kwargs):
            seen.append(list(argv))
            completed = subprocess.CompletedProcess(argv, 1, "", "ModuleNotFoundError: No module named 'scripts'")
            return completed

        with patch.object(prepush.subprocess, "run", spy):
            failures = prepush.isolated_import_probe_failures(
                ROOT,
                ["tests/scripts/test_overlay_pre_push.py", JAVA],
            )
        self.assertEqual(1, len(seen))
        self.assertIn("-I", seen[0])
        self.assertTrue(seen[0][-1].endswith("test_overlay_pre_push.py"))
        self.assertTrue(any("test_overlay_pre_push.py" in item for item in failures))
        with patch.object(prepush.subprocess, "run", spy):
            self.assertEqual([], prepush.isolated_import_probe_failures(ROOT, [JAVA]))

    def test_missing_self_locating_import_fails_the_probe(self) -> None:
        path = ROOT / "tests/scripts/test_ce_probe_missing_import_7791.py"
        path.write_text("import scripts.ci.overlay_pre_push\n", encoding="utf-8")
        try:
            failures = prepush.isolated_import_probe_failures(
                ROOT, ["tests/scripts/test_ce_probe_missing_import_7791.py"]
            )
        finally:
            path.unlink(missing_ok=True)
        self.assertTrue(any("ModuleNotFoundError" in item for item in failures), failures)


class DesignSkillsFailureLogTest(unittest.TestCase):
    def test_red_log_names_the_check_and_keeps_awaiting_checks(self) -> None:
        blob = ("x" * 20000) + "\nAssertionError: 'AWAITING_CHECKS' not found in '" + ("y" * 30000)
        log = gate.failure_log(("design-skills-contract",), blob)
        self.assertTrue(log.startswith("failed=design-skills-contract"), log[:80])
        self.assertIn("AWAITING_CHECKS", log)
        self.assertLessEqual(len(log), 4096)
        payload = {
            "valid": False,
            "surfaces": ["design-skills"],
            "timing": {},
            "checks": [
                {
                    "id": "design-skills-contract",
                    "status": "failed",
                    "protected": False,
                    "tests": ["tests.scripts.test_ce_design_lessons"],
                    "reproduction_command": "python -m unittest tests.scripts.test_ce_design_lessons -v",
                }
            ],
        }
        self.assertTrue(gate.render_text(payload).startswith("failed=design-skills-contract"))


if __name__ == "__main__":
    unittest.main()
