"""Phase 1 CE memory abstraction (no overlap with sibling video sign-off gates)."""

from __future__ import annotations

import importlib.util
import json
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DESIGN = CE / "addons/design-skills"
LESSONS = DESIGN / "references/lessons"
SCOPE = DESIGN / "scripts/title_scope_gate.py"
WORKER = CE / "skills/local-agency/scripts/worker_ce_entry.py"


def _text(path: Path) -> str:
    return " ".join(path.read_text(encoding="utf-8").split())


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod


class ProcessCardsTest(unittest.TestCase):
    CARDS = {
        "worker-keep-alive.md": ("KEEP-ALIVE", "same turn", "independent"),
        "checkpoint-resume.md": ("checkpoint", "interrupted", "resume"),
        "cleanup-at-close.md": ("BLOCKED-INFRA", "80%", "Outdated YouTube"),
        "ce-upgrade-on-release.md": ("CE upgrade", "VERSION", "ce_load"),
        "automerge-rearm.md": ("re-arm", "soft-fail", "eviction"),
        "sequential-wip.md": ("one", "In progress", "adversarial"),
        "mobile-client-automation.md": ("Read-only", "Secret-scan", "NamedBlock"),
        "design-review-video-checklist.md": ("Independent adversarial", "bidisolate", "LRI"),
    }

    def test_cards_have_rule_why_check(self) -> None:
        for name, needles in self.CARDS.items():
            path = CE / "references" / name
            raw = path.read_text(encoding="utf-8")
            with self.subTest(card=name):
                for section in ("## Rule", "## Why", "## Check"):
                    self.assertIn(section, raw)
                text = _text(path)
                for needle in needles:
                    self.assertIn(needle, text)
                self.assertNotIn("<CLIENT>", raw)
                self.assertNotIn("elmenus", raw.lower())
                self.assertNotRegex(raw, r"(?i)\ballure\b")
                self.assertNotRegex(raw.lower(), r"\bcoach\b")

    def test_keep_alive_does_not_claim_l39(self) -> None:
        """Sibling video sign-off lesson owns design L39; keep-alive is unnumbered."""
        raw = (CE / "references/worker-keep-alive.md").read_text(encoding="utf-8")
        self.assertNotIn("L39", raw)

    def test_bot_entry_requires_ce_load(self) -> None:
        bot = (CE / "references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("ce_load: required", bot)
        self.assertIn("worker_ce_entry.py", bot)
        self.assertIn("worker-keep-alive.md", bot)
        self.assertIn("checkpoint-resume.md", bot)
        self.assertIn("ce-upgrade-on-release.md", bot)
        self.assertIn("cleanup-at-close.md", bot)
        self.assertIn("KEEP-ALIVE", bot)

    def test_version_file_broadcastable(self) -> None:
        raw = (CE / "VERSION").read_text(encoding="utf-8").strip().splitlines()[0]
        self.assertRegex(raw, r"^\d{4}\.\d{2}\.\d{2}(\.\d+)?$")

    def test_parallel_streams_brief_marker(self) -> None:
        ps = (CE / "references/parallel-streams.md").read_text(encoding="utf-8")
        self.assertIn("ce_load: required", ps)
        self.assertIn("worker_ce_entry.py lint", ps)

    def test_tip_churn_links_new_refs(self) -> None:
        tip = (CE / "references/tip-churn-preflight.md").read_text(encoding="utf-8")
        for name in (
            "worker-keep-alive.md",
            "checkpoint-resume.md",
            "cleanup-at-close.md",
            "ce-upgrade-on-release.md",
            "automerge-rearm.md",
            "sequential-wip.md",
            "mobile-client-automation.md",
            "design-review-video-checklist.md",
            "worker_ce_entry.py",
        ):
            self.assertIn(name, tip)


class DesignLessonFilesPhase1Test(unittest.TestCase):
    """Lesson files exist; indexed via index-extension.md (primary index: sibling L39 tip)."""

    CARDS = {
        "youtube-upload-language-order.md": ("L40", "video language", "auto-dub", "captions"),
        "spoken-lock-scope-words.md": ("L41", "scope", "title_scope_gate", "hypernym"),
        "chrome-follows-title.md": ("L42", "burned-in", "endcard", "body only"),
        "lq-vs-short-dup.md": ("L43", "3×", "superset", "fail closed"),
    }

    def test_cards_shape(self) -> None:
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        for name, needles in self.CARDS.items():
            path = LESSONS / name
            raw = path.read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertTrue(raw.startswith("---\nname:"))
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                for section in ("## Use when", "## Rules", "## Verify", "## Sources"):
                    self.assertIn(section, raw)
                for needle in needles:
                    self.assertIn(needle, _text(path))
                self.assertNotRegex(raw, r"#\d{3,}")
                self.assertNotIn("L39", raw)
                self.assertNotRegex(raw.lower(), r"\bcoach\b")
        extension = (LESSONS / "index-extension.md").read_text(encoding="utf-8")
        for name in self.CARDS:
            self.assertIn(f"]({name})", extension)
        self.assertFalse((LESSONS / "viewer-video-qc.md").exists())
        self.assertNotIn("viewer-video-qc.md", index)
        self.assertNotRegex(
            (LESSONS / "caption-phonetic-mute-gate.md").read_text(encoding="utf-8").lower(),
            r"\bcoach\b",
        )

    def test_l38_n5_documented_code_deferred(self) -> None:
        raw = (LESSONS / "caption-phonetic-mute-gate.md").read_text(encoding="utf-8")
        self.assertIn("بايبلاين", raw)
        self.assertIn("سكريبت", raw)
        self.assertIn("video sign-off", raw)


class WorkerCeEntryTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load("worker_ce_entry", WORKER)

    def test_lint_fails_without_markers(self) -> None:
        fails = self.mod.lint_brief("goal: fix a bug\n")
        self.assertTrue(any("ce_load" in f for f in fails))
        self.assertTrue(any("entry" in f for f in fails))

    def test_lint_passes_with_markers(self) -> None:
        text = "ce_load: required\nRun: python3 .chaos-engine/tool.py entry\ngoal: x\n"
        self.assertEqual([], self.mod.lint_brief(text))

    def test_cli_fail_and_pass(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            bad = Path(tmp) / "bad.md"
            good = Path(tmp) / "good.md"
            bad.write_text("no markers\n", encoding="utf-8")
            good.write_text(
                "ce_load: required\npython3 .chaos-engine/tool.py entry --full\n",
                encoding="utf-8",
            )
            bad_p = subprocess.run(  # nosec B603
                [sys.executable, str(WORKER), "lint", str(bad)],
                capture_output=True, text=True, check=False,
            )
            good_p = subprocess.run(  # nosec B603
                [sys.executable, str(WORKER), "lint", str(good)],
                capture_output=True, text=True, check=False,
            )
        self.assertEqual(1, bad_p.returncode)
        self.assertEqual(0, good_p.returncode)


class TitleScopeGateTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load("title_scope_gate", SCOPE)

    def test_missing_scope_fails(self) -> None:
        hits = self.mod.title_scope_hits(
            "penetration testing basics",
            "1\n00:00:00,000 --> 00:00:01,000\nbasics of the craft\n",
        )
        self.assertTrue(any("penetration" in h for h in hits))

    def test_scope_present_passes(self) -> None:
        hits = self.mod.title_scope_hits(
            "penetration testing basics",
            "1\n00:00:00,000 --> 00:00:02,000\npenetration testing basics explained\n",
        )
        self.assertEqual([], hits)


    def test_stem_and_arabic_and_brand(self) -> None:
        """N2: stemming / Arabic ال- / brand allowlist reduce false positives."""
        self.assertEqual(
            [],
            self.mod.title_scope_hits(
                "testing craft",
                "1\n00:00:00,000 --> 00:00:01,000\ntests of the craft\n",
            ),
        )
        self.assertEqual(
            [],
            self.mod.title_scope_hits(
                "الاختبار الآلي",
                "1\n00:00:00,000 --> 00:00:01,000\nاختبار آلي للموقع\n",
            ),
        )
        self.assertEqual(
            [],
            self.mod.title_scope_hits(
                "YouTube tips",
                "1\n00:00:00,000 --> 00:00:01,000\nuseful tips for the suite\n",
            ),
        )
        # still fail when a real scope noun is absent
        hits = self.mod.title_scope_hits(
            "penetration testing basics",
            "1\n00:00:00,000 --> 00:00:01,000\nbasics of the craft\n",
        )
        self.assertTrue(any("penetration" in h for h in hits))

    def test_cli_titlescope(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            srt = Path(tmp) / "a.srt"
            srt.write_text(
                "1\n00:00:00,000 --> 00:00:01,000\nonly testing here\n",
                encoding="utf-8",
            )
            proc = subprocess.run(  # nosec B603
                [
                    sys.executable, str(SCOPE), "check",
                    "--title", "penetration testing",
                    "--srt", str(srt),
                ],
                capture_output=True, text=True, check=False,
            )
        self.assertEqual(1, proc.returncode)
        payload = json.loads(proc.stdout)
        self.assertTrue(payload["hits"])


class NoVideoGateOverlapTest(unittest.TestCase):
    def test_no_viewer_video_qc_script(self) -> None:
        self.assertFalse((DESIGN / "scripts/viewer_video_qc.py").exists())

    def test_craft_qc_untouched_for_phase1_gates(self) -> None:
        """Phase 1 must not add titlescope/N5 into craft_qc (sibling video tip owns that surface)."""
        craft = (DESIGN / "scripts/craft_qc.py").read_text(encoding="utf-8")
        self.assertNotIn("def cmd_titlescope", craft)
        self.assertNotIn("بايبلاين", craft)
        # Do NOT assert absence of cmd_bidisolate: sibling video tip may land first.


if __name__ == "__main__":
    unittest.main()
