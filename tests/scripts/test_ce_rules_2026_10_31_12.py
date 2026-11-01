"""CE 2026.10.31.12: L82–L97 mega batch + #7678/#7672/L95 residuals (L74)."""

from __future__ import annotations

import importlib.util
import inspect
import os
import re
import sys
import tempfile
import unittest.mock as mock
from pathlib import Path
from types import SimpleNamespace
from unittest import TestCase, main

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DETAIL = CE / "references/details/ce-rules-2026-10-31-12"
sys.path.insert(0, str(ROOT))
from tests.scripts.test_chaos_engine_installer import MODULE  # noqa: E402


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    assert spec is not None and spec.loader is not None
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class VersionFloorTest(TestCase):
    def test_version_at_least_2026_10_31_12(self) -> None:
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        floor = tuple(int(p) for p in "2026.10.31.12".split("."))
        self.assertGreaterEqual(tuple(int(p) for p in version.split(".")), floor)


class CardsPresentTest(TestCase):
    REQUIRED = (
        "verify-dropped.md",
        "mega-pr.md",
        "installer-posix-env.md",
        "voice-asr.md",
        "voice-proper-nouns.md",
        "status-fresh.md",
        "allure-compose-redaction.md",
        "mempalace-sqlite-doctor.md",
        "dump-partial-denominator.md",
        "locator-runtime-xpath.md",
        "highlight-element-id.md",
        "teams-zip-apk.md",
        "stopsubagent-targeted.md",
        "evidence-html-size.md",
        "ce-version-after-upgrade.md",
    )

    def test_all_l82_l97_cards_exist(self) -> None:
        for name in self.REQUIRED:
            path = DETAIL / name
            self.assertTrue(path.is_file(), name)
            body = path.read_text(encoding="utf-8")
            self.assertGreater(len(body), 80, name)

    def test_spec_and_pointer_exist(self) -> None:
        self.assertTrue((CE / "guides/ce-rules-2026-10-31-12-SPEC.md").is_file())
        self.assertTrue((CE / "references/ce-rules-2026-10-31-12-SPEC.md").is_file())


class L86VerifyDroppedTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_verify_dropped", CE / "rules/verify_dropped.py")

    def test_redo_without_receipt_fails(self) -> None:
        result = self.mod.evaluate("Please redo the installer fix")
        self.assertFalse(result["ok"])
        self.assertIn("missing-live-state-receipt", result["hits"])

    def test_redo_with_already_exists_passes(self) -> None:
        result = self.mod.evaluate(
            "VERIFY-DROPPED: redo coverage? already exists https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7679"
        )
        self.assertTrue(result["ok"])

    def test_proven_red_mutant_drops_live_marker(self) -> None:
        source = Path(self.mod.__file__).read_text(encoding="utf-8")
        self.assertIn("already exists", source)
        self.assertIn("VERIFY-DROPPED", source)


class L97MegaPrTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_mega_pr", CE / "rules/mega_pr.py")

    def test_one_pr_per_ticket_fails(self) -> None:
        result = self.mod.evaluate("We will open one PR per ticket for each bug.")
        self.assertFalse(result["ok"])
        self.assertIn("one-pr-per-ticket", result["hits"])

    def test_epic_mega_pr_passes(self) -> None:
        result = self.mod.evaluate(
            "One mega PR for the coverage epic; never one PR per ticket."
        )
        self.assertTrue(result["ok"])

    def test_work_ordering_documents_mega_pr(self) -> None:
        text = (CE / "references/work-ordering.md").read_text(encoding="utf-8")
        self.assertIn("L97 MEGA-PR", text)
        self.assertIn("fewest", text.lower())


class L95JdkProvisionTest(TestCase):
    def test_mutant_preflight_calls_ensure_managed_temurin(self) -> None:
        source = inspect.getsource(MODULE.preflight_maven_tools_jdk)
        self.assertIn("ensure_managed_temurin_jdk", source)
        self.assertNotIn("wrapper is not None", source)

    def test_unusable_jdk_provisions_managed_temurin_success_path(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            bad = root / "jre" / "bin" / ("java.exe" if os.name == "nt" else "java")
            bad.parent.mkdir(parents=True)
            bad.write_text("java\n", encoding="utf-8")
            good_home = root / "temurin"
            good = good_home / "bin" / ("java.exe" if os.name == "nt" else "java")
            good.parent.mkdir(parents=True)
            good.write_text("java\n", encoding="utf-8")
            (good.parent / ("javac.exe" if os.name == "nt" else "javac")).write_text(
                "javac\n", encoding="utf-8"
            )
            provision = mock.Mock(return_value=good)
            hosts = SimpleNamespace(
                java_major=lambda path: 25 if path == good or path.resolve() == good.resolve() else 11,
                java_compiler_present=lambda path: path.resolve() == good.resolve(),
                ensure_managed_temurin_jdk=provision,
            )
            # Force candidate unusable via major
            hosts.java_major = lambda path: (
                25 if "temurin" in str(path) else 11
            )
            hosts.java_compiler_present = lambda path: "temurin" in str(path)
            resolved = MODULE.preflight_maven_tools_jdk(
                hosts, bad, required_major=25, specification={"dependencies": {}}
            )
            self.assertEqual(str(good_home.resolve()), resolved)
            provision.assert_called_once()

    def test_posix_env_sh_documents_source_hint(self) -> None:
        source = inspect.getsource(MODULE._persist_posix_user_jdk)
        self.assertIn("env.sh", source)
        self.assertIn("source", source)
        self.assertNotIn(".bashrc", source)


class L93L96VoiceAsrTest(TestCase):
    def setUp(self) -> None:
        self.qc = _load("ce_design_qc_l93", CE / "addons/design-skills/scripts/design_qc.py")

    def test_default_max_wer_is_fifteen_percent(self) -> None:
        source = (CE / "addons/design-skills/scripts/design_qc.py").read_text(encoding="utf-8")
        self.assertIn("default=0.15", source)
        self.assertIn("--proper-noun", source)
        self.assertIn("char_error_rate", source)

    def test_proper_noun_miss_fails(self) -> None:
        self.assertEqual(
            ["Mohab"],
            self.qc.proper_noun_misses(
                "Hello Mohab from Automatest",
                "Hello Moheb from Automatest",
                ["Mohab", "Automatest"],
            ),
        )

    def test_cer_and_unk_helpers(self) -> None:
        self.assertEqual(0.0, self.qc.char_error_rate("abc", "abc"))
        self.assertGreater(self.qc.char_error_rate("abc", "axc"), 0.0)
        self.assertEqual(0.5, self.qc.tokenizer_unk_rate(["a", "<unk>"]))


class SkillSpectorReadmeBadgeTest(TestCase):
    def test_readme_has_skillspector_workflow_badge(self) -> None:
        text = (ROOT / "README.md").read_text(encoding="utf-8")
        self.assertIn("skillspector.yml", text)
        self.assertIn("label=skillspector", text)

    def test_badge_budget_allows_five_status_badges(self) -> None:
        source = (ROOT / "scripts/ci/readme_contract.py").read_text(encoding="utf-8")
        self.assertIn("STATUS_BADGE_BUDGET = 5", source)


class CopilotWindowsHookFieldTest(TestCase):
    def test_repository_hook_run_selects_windows_field(self) -> None:
        source = (ROOT / "tests/scripts/test_copilot_cli_hook.py").read_text(encoding="utf-8")
        self.assertIn("windows = stem not in", source)
        self.assertIn("#7678", source)


class ShippedLessonsNotDuplicatedTest(TestCase):
    """VERIFY-DROPPED: L78–L81 and L94 already shipped — cards remain under older SPECs."""

    def test_prior_specs_still_present(self) -> None:
        self.assertTrue((CE / "guides/ce-rules-2026-10-31-8-SPEC.md").is_file())
        self.assertTrue((CE / "guides/ce-rules-2026-10-31-11-SPEC.md").is_file())
        self.assertTrue(
            (CE / "references/details/ce-rules-2026-10-31-8/review-scratch-cleanup.md").is_file()
        )


if __name__ == "__main__":
    main()
