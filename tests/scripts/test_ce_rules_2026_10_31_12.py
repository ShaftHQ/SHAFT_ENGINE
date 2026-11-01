"""CE 2026.10.31.12: L82–L97 mega batch + #7678/#7672/L95 residuals (L74)."""

from __future__ import annotations

import importlib.util
import inspect
import os
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
            "VERIFY-DROPPED: redo coverage? board owner Chaos; "
            "already exists https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7679"
        )
        self.assertTrue(result["ok"])

    def test_dropped_without_board_owner_fails(self) -> None:
        result = self.mod.evaluate(
            "VERIFY-DROPPED: redo coverage? already exists "
            "https://github.com/ShaftHQ/SHAFT_ENGINE/pull/7679"
        )
        self.assertFalse(result["ok"])
        self.assertIn("missing-board-owner-check", result["hits"])

    def test_proven_red_mutant_drops_live_marker(self) -> None:
        source = Path(self.mod.__file__).read_text(encoding="utf-8")
        self.assertIn("already exists", source)
        self.assertIn("VERIFY-DROPPED", source)
        self.assertIn("board owner", source)


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
        self.assertIn("extract_proper_nouns", source)
        self.assertIn("tokenizer tokens required (L93)", source)
        self.assertIn("language_id required (L93)", source)
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




class L82L92ProcessGatesTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_lessons_l82_l92", CE / "rules/lessons_l82_l92.py")

    def test_l82_evidence_compose_proven_red(self) -> None:
        bad = self.mod.evaluate_evidence_compose("zip the allure results")
        self.assertFalse(bad["ok"])
        self.assertTrue(bad["hits"])
        good = self.mod.evaluate_evidence_compose(
            "redact secrets, slim under 25 MB size cap, checkpoint before zip, "
            "preflight HEAD equals commit"
        )
        self.assertTrue(good["ok"], good)

    def test_l83_status_fresh_proven_red(self) -> None:
        bad = self.mod.evaluate_status_fresh("looks fine from memory")
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_status_fresh(
            "fresh status via gh pr view; VERIFY-DROPPED: already exists"
        )
        self.assertTrue(good["ok"], good)

    def test_l84_sqlite_and_empty_previous_proven_red(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            palace = root / "mempalace"
            palace.mkdir()
            miss = self.mod.evaluate_mempalace_sqlite(palace)
            self.assertFalse(miss["ok"])
            self.assertIn("missing-sqlite_exact", miss["hits"])
            (palace / "sqlite_exact.sqlite3").write_bytes(b"ok")
            self.assertTrue(self.mod.evaluate_mempalace_sqlite(palace)["ok"])
            empty = root / "mempalace.previous-20260101010101"
            empty.mkdir()
            bad = self.mod.evaluate_no_empty_previous(root)
            self.assertFalse(bad["ok"])
            removed = self.mod.remove_empty_previous_dirs(root)
            self.assertEqual(["mempalace.previous-20260101010101"], removed)
            self.assertTrue(self.mod.evaluate_no_empty_previous(root)["ok"])

    def test_l85_dump_partial_denominator_proven_red(self) -> None:
        bad = self.mod.evaluate_dump_partial({"count_ok": True})
        self.assertFalse(bad["ok"])
        self.assertIn("count_ok-without-shard", bad["hits"])
        partial = self.mod.evaluate_dump_partial({"partial": True})
        self.assertFalse(partial["ok"])
        good = self.mod.evaluate_dump_partial(
            {"count_ok": True, "own_shard": ["a"], "partial": True, "denominator": 4}
        )
        self.assertTrue(good["ok"], good)

    def test_l87_locator_runtime_xpath_proven_red(self) -> None:
        bad = self.mod.evaluate_locator_inventory(
            [{"id": "x1", "xpath": "concat('a','b')", "status": "broken"}]
        )
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_locator_inventory(
            [{"id": "x1", "xpath": "concat('a','b')", "status": "broken", "resolved": "//a"}]
        )
        self.assertTrue(good["ok"], good)

    def test_l88_highlight_identity_proven_red(self) -> None:
        bad = self.mod.evaluate_highlight_identity(
            {"intended_id": "btn-ok", "highlighted_id": "btn-cancel"}
        )
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_highlight_identity(
            {"intended_id": "btn-ok", "highlighted_id": "btn-ok"}
        )
        self.assertTrue(good["ok"], good)

    def test_l89_teams_zip_apk_proven_red(self) -> None:
        bad = self.mod.evaluate_share_paths(["build/app-release.apk"])
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_share_paths(["build/app-release.apk.zip"])
        self.assertTrue(good["ok"], good)

    def test_l90_stop_subagent_all_denied(self) -> None:
        bad = self.mod.evaluate_stop_subagent({"all": True})
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_stop_subagent({"id": "agent-1"})
        self.assertTrue(good["ok"], good)

    def test_l91_evidence_html_size_proven_red(self) -> None:
        bad = self.mod.evaluate_evidence_html_paths(
            [("reports/evidence.html", 100), ("blob.bin", 21 * 1024 * 1024)]
        )
        self.assertFalse(bad["ok"])
        self.assertTrue(any(h.startswith("html:") for h in bad["hits"]))
        self.assertTrue(any(h.startswith("oversize:") for h in bad["hits"]))
        good = self.mod.evaluate_evidence_html_paths(
            [("reports/evidence.html.zip", 100), ("blob.bin.zip", 21 * 1024 * 1024)]
        )
        self.assertTrue(good["ok"], good)

    def test_l92_version_match_proven_red(self) -> None:
        bad = self.mod.evaluate_version_match("2026.10.31.11", "2026.10.31.12")
        self.assertFalse(bad["ok"])
        good = self.mod.evaluate_version_match("2026.10.31.12", "2026.10.31.12")
        self.assertTrue(good["ok"], good)


class L84DoctorProductTest(TestCase):
    def test_sqlite_missing_finding_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = project / ".chaos-engine-state" / "mempalace"
            palace.mkdir(parents=True)
            finding = MODULE._mempalace_sqlite_missing_finding(project)
            self.assertIsNotNone(finding)
            self.assertEqual("invalid", finding["status"])
            (palace / "sqlite_exact.sqlite3").write_bytes(b"x")
            self.assertIsNone(MODULE._mempalace_sqlite_missing_finding(project))

    def test_empty_previous_finding_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            parent = project / ".chaos-engine-state"
            parent.mkdir()
            empty = parent / "mempalace.previous-20990101010101"
            empty.mkdir()
            finding = MODULE._mempalace_empty_previous_finding(project)
            self.assertIsNotNone(finding)
            self.assertEqual("degraded", finding["status"])


class L92DoctorProductTest(TestCase):

    def test_version_mismatch_folds_into_core(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            target = project / "src-ce"
            target.mkdir()
            installed = project / ".chaos-engine"
            installed.mkdir()
            (installed / "VERSION").write_text("2026.10.31.11\n", encoding="utf-8")
            (target / "VERSION").write_text("2026.10.31.12\n", encoding="utf-8")
            components = {"core": {"status": "healthy", "owner": "installer", "scope": "project",
                                   "lifecycle": "receipt-owned", "taskImpact": "required"}}
            MODULE._attach_l84_l92_doctor_findings(project, target, components)
            self.assertNotIn("ce-version", components)
            self.assertEqual("invalid", components["core"]["status"])

    def test_version_mismatch_finding_fail_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            target = project / "src-ce"
            target.mkdir()
            installed = project / ".chaos-engine"
            installed.mkdir()
            (installed / "VERSION").write_text("2026.10.31.11\n", encoding="utf-8")
            (target / "VERSION").write_text("2026.10.31.12\n", encoding="utf-8")
            finding = MODULE._ce_version_after_upgrade_finding(project, target)
            self.assertIsNotNone(finding)
            self.assertEqual("invalid", finding["status"])
            (installed / "VERSION").write_text("2026.10.31.12\n", encoding="utf-8")
            ok = MODULE._ce_version_after_upgrade_finding(project, target)
            self.assertEqual("healthy", ok["status"])


class L93L96FailClosedCmdTtsTest(TestCase):
    def setUp(self) -> None:
        self.qc = _load("ce_design_qc_l93b", CE / "addons/design-skills/scripts/design_qc.py")

    def test_extract_proper_nouns_mid_sentence(self) -> None:
        nouns = self.qc.extract_proper_nouns("Hello Mohab from Automatest today")
        self.assertIn("Mohab", nouns)
        self.assertIn("Automatest", nouns)

    def test_cmd_tts_proper_noun_miss_without_flag(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            script = root / "s.txt"
            heard = root / "h.txt"
            tokens = root / "tok.txt"
            script.write_text("Hello Mohab from Automatest", encoding="utf-8")
            heard.write_text("Hello Moheb from Automatest", encoding="utf-8")
            tokens.write_text("Hello Mohab from Automatest", encoding="utf-8")
            ns = SimpleNamespace(
                script=str(script),
                transcript=str(heard),
                fold=[],
                max_wer=0.5,  # WER alone would pass
                lang="en",
                cer=False,
                max_cer=0.15,
                tokens=str(tokens),
                max_unk=0.05,
                language_id="en",
                expect_language=None,
                proper_noun=[],
                duration=None,
                min_wpm=140,
                max_wpm=160,
            )
            code = self.qc.cmd_tts(ns)
            self.assertEqual(1, code)

    def test_cmd_tts_requires_tokens_and_language_id(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            script = root / "s.txt"
            heard = root / "h.txt"
            script.write_text("hello world today here", encoding="utf-8")
            heard.write_text("hello world today here", encoding="utf-8")
            ns = SimpleNamespace(
                script=str(script),
                transcript=str(heard),
                fold=[],
                max_wer=0.15,
                lang="en",
                cer=False,
                max_cer=0.15,
                tokens=None,
                max_unk=0.05,
                language_id=None,
                expect_language=None,
                proper_noun=[],
                duration=None,
                min_wpm=140,
                max_wpm=160,
            )
            self.assertEqual(1, self.qc.cmd_tts(ns))


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
