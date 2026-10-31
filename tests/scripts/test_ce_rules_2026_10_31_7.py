"""CE 2026.10.31.7: WoW + memory dump + L65-fu/L66–L77 (L74 proven-red)."""
from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    sys.modules[name] = mod  # dataclasses need the module registered
    spec.loader.exec_module(mod)
    return mod


class VersionBumpTest(unittest.TestCase):
    def test_version_is_31_7(self) -> None:
        self.assertEqual(
            "2026.10.31.7",
            (CE / "VERSION").read_text(encoding="utf-8").strip(),
        )


class FileClaimLockTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load(
            "ce_file_claim_lock", ROOT / "scripts/ci/file_claim_lock.py"
        )

    def test_overlap_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "claims.json"
            self.mod.claim("a", "w1", ["chaos-engine/foo.py"], path=path, kind="ce-rules")
            with self.assertRaises(PermissionError):
                self.mod.claim(
                    "b", "w2", ["chaos-engine/foo.py"], path=path, kind="other"
                )

    def test_same_kind_refuses(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "claims.json"
            self.mod.claim("a", "w1", ["a/one.py"], path=path, kind="ce-rules")
            with self.assertRaises(PermissionError):
                self.mod.claim(
                    "b", "w2", ["b/two.py"], path=path, kind="ce-rules"
                )

    def test_release_allows_reclaim(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "claims.json"
            self.mod.claim("a", "w1", ["x/y.py"], path=path, kind="k")
            self.mod.release("a", path=path)
            self.mod.claim("b", "w2", ["x/y.py"], path=path, kind="k")


class MemoryFullDumpTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_memory_full_dump", CE / "rules/memory_full_dump.py")

    def test_count_ok_when_unique_equals_total(self) -> None:
        batches = [
            self.mod.QueryBatch("q1", ["a", "b", "c"], top_n=50),
            self.mod.QueryBatch("q2", ["b", "d"], top_n=50),
        ]
        texts, report = self.mod.merge_batches(batches, host_total=4)
        self.assertEqual(["a", "b", "c", "d"], texts)
        self.assertTrue(report.count_ok)

    def test_cap_incomplete_fails_count(self) -> None:
        hits = [f"m{i}" for i in range(50)]
        batches = [self.mod.QueryBatch("q", hits, top_n=50)]
        _, report = self.mod.merge_batches(batches, host_total=200)
        self.assertTrue(report.cap_incomplete)
        self.assertFalse(report.count_ok)


class L77RemoteCredentialTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load(
            "ce_remote_credential_guard", CE / "rules/remote_credential_guard.py"
        )

    def test_red_fake_ghp_fixture(self) -> None:
        fake = f"https://x-access-token:{self.mod.FAKE_GHP}@github.com/org/repo.git"
        reason = self.mod.url_embeds_credentials(fake)
        self.assertIsNotNone(reason)
        findings = self.mod.scan_remote_urls([("origin", fake)])
        self.assertEqual(1, len(findings))
        blob = json.dumps(findings)
        self.assertNotIn(self.mod.FAKE_GHP, blob)
        self.assertNotIn("ghp_0000", blob)  # full fake not echoed

    def test_red_user_pass_and_pat(self) -> None:
        self.assertIsNotNone(
            self.mod.url_embeds_credentials(
                "https://user:s3cret@github.com/org/repo.git"
            )
        )
        pat = f"https://github.com/org/repo.git?token={self.mod.FAKE_PAT}"
        self.assertIsNotNone(self.mod.url_embeds_credentials(pat))

    def test_clean_urls_pass(self) -> None:
        for url in (
            "https://github.com/org/repo.git",
            "git@github.com:org/repo.git",
            "ssh://git@github.com/org/repo.git",
        ):
            self.assertIsNone(self.mod.url_embeds_credentials(url), url)

    def test_redact_never_keeps_userinfo(self) -> None:
        fake = f"https://x-access-token:{self.mod.FAKE_GHP}@github.com/org/repo.git"
        red = self.mod.redact_url(fake)
        self.assertNotIn(self.mod.FAKE_GHP, red)
        self.assertNotIn("x-access-token", red.casefold())


class ProvenRedMixin:
    """L74: every new gate proves FAIL on a known-bad fixture first."""

    def _prove(self, gate_path: Path, bad: dict, hit_sub: str) -> None:
        proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")
        gate_mod = _load("ce_gate_under_test", gate_path)
        proven.prove_red(gate_mod.evaluate, bad, expected_hit_substr=hit_sub)


class L66WebUiProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red(self) -> None:
        self._prove(
            CE / "rules/web_ui_gate.py",
            {"bidi_ok": False, "schema_only_done": True},
            "bidi",
        )


class L70CoachProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red(self) -> None:
        self._prove(
            CE / "rules/coach_pre_publish.py",
            {"live_publish": True, "surface": "web"},
            "coach-gate",
        )


class L71AvatarProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red(self) -> None:
        self._prove(
            CE / "rules/avatar_likeness_gate.py",
            {"from_scratch": True, "glasses_present": False},
            "glasses",
        )


class L75NoProbeProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red(self) -> None:
        self._prove(
            CE / "rules/no_probe_live.py",
            {
                "action": "update_file_content",
                "path": "/live/Drive/cv.docx",
                "live_roots": ["/live/"],
            },
            "live",
        )


class L76PreviewProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red(self) -> None:
        self._prove(
            CE / "rules/preview_at_real_width.py",
            {
                "coach_pass": True,
                "preview_width_px": 1440,
                "real_container_width_px": 320,
                "live_paste": True,
            },
            "preview-width-mismatch",
        )


class L65FollowupsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.mod = _load(
            "ce_mggate",
            CE / "addons/design-skills/scripts/motion_graphics_gate.py",
        )

    def _base_obs(self, asr_path: Path) -> dict:
        return {
            "transcript_manually_verified": True,
            "reviewer_brief_lists_mg_checks": True,
            "kind": "kinetic",
            "static_text_blocks": [],
            "ocr_frames": [{"text": "hi", "expected": "hi"}],
            "diagrams": [{"animation": "draw-on"}],
            "asr_path": str(asr_path),
            "asr_sha256": "abc",
            "asr_tool": "whisper-fixture",
            "asr_independent_proof": True,
            "captions": [
                {"start": 0.0, "lines": ["hello"], "phrase_split": True},
            ],
        }

    def test_proven_red_inline_asr_without_path(self) -> None:
        proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")
        bad = {
            "transcript_manually_verified": True,
            "reviewer_brief_lists_mg_checks": True,
            "kind": "kinetic",
            "static_text_blocks": [],
            "ocr_frames": [{"text": "hi", "expected": "hi"}],
            "diagrams": [{"animation": "draw-on"}],
            "asr_words": [{"word": "hello", "start": 0.0}],
            "captions": [{"start": 0.0, "lines": ["hello"]}],
        }
        proven.prove_red(
            self.mod.evaluate, bad, expected_hit_substr="missing-asr-path"
        )

    def test_unmatched_cue_fails_no_nearest(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            asr = Path(tmp) / "asr.json"
            asr.write_text(
                json.dumps([{"word": "alpha", "start": 0.0}]), encoding="utf-8"
            )
            obs = self._base_obs(asr)
            obs["captions"] = [{"start": 0.0, "lines": ["omega"]}]
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("unmatched-asr-word" in h for h in result["hits"])
            )

    def test_sequential_duplicate_tokens(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            asr = Path(tmp) / "asr.json"
            asr.write_text(
                json.dumps(
                    [
                        {"word": "go", "start": 0.0},
                        {"word": "go", "start": 1.0},
                    ]
                ),
                encoding="utf-8",
            )
            obs = self._base_obs(asr)
            obs["captions"] = [
                {"start": 0.0, "lines": ["go"]},
                {"start": 1.0, "lines": ["go"]},
            ]
            result = self.mod.evaluate(obs)
            self.assertTrue(result["ok"], result["hits"])

    def test_cue_aligned_source_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            asr = Path(tmp) / "asr.json"
            asr.write_text(
                json.dumps([{"word": "hello", "start": 0.0}]), encoding="utf-8"
            )
            obs = self._base_obs(asr)
            obs["asr_source"] = "cue-aligned"
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("cue-aligned" in h for h in result["hits"])
            )

    def test_perfect_zero_without_proof_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            asr = Path(tmp) / "asr.json"
            asr.write_text(
                json.dumps([{"word": "hello", "start": 0.0}]), encoding="utf-8"
            )
            obs = self._base_obs(asr)
            obs["asr_independent_proof"] = False
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("perfect-zero" in h for h in result["hits"])
            )


class SpecAndDocsTest(unittest.TestCase):
    def test_spec_and_wow_exist(self) -> None:
        for rel in (
            "references/ce-rules-2026-10-31-7-SPEC.md",
            "guides/ce-rules-2026-10-31-7-SPEC.md",
            "references/details/ce-rules-2026-10-31-7/ways-of-working.md",
            "references/details/ce-rules-2026-10-31-7/memory-full-dump.md",
            "references/details/ce-rules-2026-10-31-7/no-token-in-remote.md",
            "references/details/ce-rules-2026-10-31-7/l67-reject-to-rule.md",
            "references/details/ce-rules-2026-10-31-7/l72-design-fix-loop.md",
        ):
            self.assertTrue((CE / rel).is_file(), rel)

    def test_no_owner_email_in_new_portable_refs(self) -> None:
        banned = ["@gmail.com", "mohab", "Mohab"]
        for path in (CE / "references").glob("*2026-10-31-7*"):
            text = path.read_text(encoding="utf-8")
            for needle in banned:
                self.assertNotIn(needle, text, path.name)
        # guides SPEC may name remotes but not personal email
        guides = (CE / "guides/ce-rules-2026-10-31-7-SPEC.md").read_text(
            encoding="utf-8"
        )
        self.assertNotIn("@mail.", guides)
        self.assertNotIn("mohab", guides.casefold())


class VersionPinFloorTest(unittest.TestCase):
    """Soft floor: prior bugfix/retro pins accept >= 2026.10.31.5 still."""

    def test_floor_still_soft(self) -> None:
        floor = tuple(int(p) for p in "2026.10.31.5".split("."))
        current = tuple(
            int(p)
            for p in (CE / "VERSION").read_text(encoding="utf-8").strip().split(".")
        )
        self.assertGreaterEqual(current, floor)


if __name__ == "__main__":
    unittest.main()
