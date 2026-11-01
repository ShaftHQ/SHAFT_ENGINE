"""CE 2026.10.31.7: WoW + memory dump + L65-fu/L66–L77 (L74 proven-red)."""
from __future__ import annotations

import hashlib
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
    def test_version_is_at_least_31_7(self) -> None:
        # Soft floor: later CE rules PRs bump VERSION; keep .7 artifacts present.
        version = (CE / "VERSION").read_text(encoding="utf-8").strip()
        floor = tuple(int(p) for p in "2026.10.31.7".split("."))
        self.assertGreaterEqual(tuple(int(p) for p in version.split(".")), floor)


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
    def test_proven_red_bidi(self) -> None:
        self._prove(
            CE / "rules/web_ui_gate.py",
            {"bidi_ok": False, "schema_only_done": True},
            "bidi",
        )

    def test_proven_red_overflow_fluid_engine(self) -> None:
        """L74: prove overflow/clip FE off-centre defect, not just bidi."""
        bad = {
            "bidi_ok": True,
            "no_raw_urls_in_copy": True,
            "min_contrast_ratio": 4.5,
            "alignment_ok": True,
            "render_widths_px": [390, 1440],
            "independent_visual_review": True,
            "viewport": {"x": 0, "y": 0, "w": 390, "h": 800},
            "container": {"x": 0, "y": 0, "w": 190, "h": 400},
            "elements": [
                {
                    "id": "fe-cell",
                    "engine": "fluid",
                    "style": "left: calc(50% - 95px)",
                    "bbox": {"x": -40, "y": 0, "w": 200, "h": 100},
                    "scrollWidth": 220,
                    "clientWidth": 190,
                    "clipping_ancestors": [
                        {"bbox": {"x": 0, "y": 0, "w": 190, "h": 400}}
                    ],
                }
            ],
        }
        self._prove(CE / "rules/web_ui_gate.py", bad, "overflow-scrollwidth")


class L70CoachProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red(self) -> None:
        self._prove(
            CE / "rules/coach_pre_publish.py",
            {"live_publish": True, "surface": "web"},
            "coach-gate",
        )

    def test_proven_red_linkedin_without_coach(self) -> None:
        self._prove(
            CE / "rules/coach_pre_publish.py",
            {"surface": "linkedin"},
            "coach-gate",
        )


class L71AvatarProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red_glasses(self) -> None:
        self._prove(
            CE / "rules/avatar_likeness_gate.py",
            {"from_scratch": True, "glasses_present": False},
            "glasses",
        )

    def test_proven_red_sface_below_threshold(self) -> None:
        """L74: prove SFace threshold defect."""
        bad = {
            "reference_conditioned": True,
            "real_photo_edit": True,
            "source_ref_photo_path": "/refs/real.jpg",
            "edit_provenance": "masked-edit-v1",
            "glasses_present": True,
            "age_within_refs": True,
            "sface_mean": 0.50,
            "sface_min": 0.50,
            "identity_score": 0.50,
            "fresh_side_by_side_review": True,
            "license": "mit",
            "stack": "opencv pillow",
            "tools_receipt": ["opencv", "pillow"],
        }
        self._prove(CE / "rules/avatar_likeness_gate.py", bad, "sface-mean-below")


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
                "coach_accepted": True,
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
        digest = hashlib.sha256(asr_path.read_bytes()).hexdigest()
        return {
            "transcript_manually_verified": True,
            "reviewer_brief_lists_mg_checks": True,
            "kind": "kinetic",
            "static_text_blocks": [],
            "ocr_frames": [{"text": "hi", "expected": "hi"}],
            "diagrams": [{"animation": "draw-on"}],
            "asr_path": str(asr_path),
            "asr_sha256": digest,
            "asr_tool": "whisper-fixture",
            "asr_source": "whisper",
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
                        {"word": "go", "start": 0.05},
                        {"word": "go", "start": 1.05},
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

    def test_forged_independent_proof_cue_synced_fails(self) -> None:
        """L73: builder asr_independent_proof ignored; cue-synced ASR FAILS."""
        with tempfile.TemporaryDirectory() as tmp:
            asr = Path(tmp) / "asr.json"
            asr.write_text(
                json.dumps([{"word": "hello", "start": 0.0}]), encoding="utf-8"
            )
            obs = self._base_obs(asr)
            obs["asr_independent_proof"] = True  # forged — must not help
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("perfect-zero" in h or "cue-correlated" in h for h in result["hits"]),
                result["hits"],
            )

    def test_forged_asr_sha256_fails(self) -> None:
        """Adversarial #2 / L73: claimed hash must match file bytes."""
        with tempfile.TemporaryDirectory() as tmp:
            asr = Path(tmp) / "asr.json"
            asr.write_text(
                json.dumps([{"word": "hello", "start": 0.05}]), encoding="utf-8"
            )
            obs = self._base_obs(asr)
            obs["asr_sha256"] = "deadbeef" * 8
            obs["captions"] = [{"start": 0.0, "lines": ["hello"], "phrase_split": True}]
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("asr-sha256-mismatch" in h for h in result["hits"]),
                result["hits"],
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



class AdversarialDoctorL77Test(unittest.TestCase):
    def test_doctor_path_loads_and_missing_fails_closed(self) -> None:
        install = _load("ce_install_doctor", CE / "install.py")
        # Correct path must exist under chaos-engine/rules/
        # When module missing → fail (not pass/unavailable)
        missing = install.remote_credential_status(
            ROOT, root=Path("/nonexistent-ce-root-for-test")
        )
        self.assertEqual("fail", missing[0])
        self.assertTrue(
            any("missing" in str(x).casefold() for x in missing[1:]),
            missing,
        )
        # Real load with clean remotes should be pass or fail on actual remotes —
        # at least must not return pass+unavailable from doubled rules/ path.
        real = install.remote_credential_status(ROOT, root=CE)
        joined = " ".join(real)
        self.assertNotIn("unavailable", joined)
        self.assertTrue((CE / "rules/remote_credential_guard.py").is_file())


class AdversarialNoProbeEscapeTest(unittest.TestCase):
    def test_dotdot_escape_fails(self) -> None:
        mod = _load("ce_no_probe_live", CE / "rules/no_probe_live.py")
        ok, reason = mod.probe_allowed(
            "update_file_content",
            "/workspace/../home/user/secret.docx",
            scratch_roots=["/workspace/"],
        )
        self.assertFalse(ok)
        self.assertTrue(
            "escape" in reason or "outside" in reason or "live" in reason,
            reason,
        )
        proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")
        proven.prove_red(
            mod.evaluate,
            {
                "action": "write",
                "path": "/workspace/../etc/passwd",
                "scratch_roots": ["/workspace/"],
            },
            expected_hit_substr="escape",
        )


class AdversarialFileClaimRelAbsTest(unittest.TestCase):
    def test_rel_abs_same_file_overlaps(self) -> None:
        mod = _load("ce_file_claim_lock", ROOT / "scripts/ci/file_claim_lock.py")
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            target = root / "chaos-engine" / "foo.py"
            target.parent.mkdir(parents=True)
            target.write_text("x", encoding="utf-8")
            claims = root / "claims.json"
            mod.claim(
                "a", "w1", ["chaos-engine/foo.py"],
                path=claims, kind="k1", project_root=root,
            )
            with self.assertRaises(PermissionError):
                mod.claim(
                    "b", "w2", [str(target.resolve())],
                    path=claims, kind="k2", project_root=root,
                )


class AdversarialMemoryHostTotalTest(unittest.TestCase):
    def test_host_total_none_fails_count(self) -> None:
        mod = _load("ce_memory_full_dump", CE / "rules/memory_full_dump.py")
        batches = [mod.QueryBatch("q1", ["a", "b"], top_n=50)]
        _, report = mod.merge_batches(batches, host_total=None)
        self.assertFalse(report.count_ok)


class AdversarialAvatarRealPhotoTest(unittest.TestCase):
    def test_missing_real_photo_edit_fails(self) -> None:
        mod = _load("ce_avatar", CE / "rules/avatar_likeness_gate.py")
        proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")
        proven.prove_red(
            mod.evaluate,
            {
                "reference_conditioned": True,
                "glasses_present": True,
                "age_within_refs": True,
                "sface_mean": 0.9,
                "sface_min": 0.8,
                "identity_score": 0.9,
                "fresh_side_by_side_review": True,
                "license": "mit",
                "stack": "opencv",
                "tools_receipt": ["opencv"],
                # missing real_photo_edit + ref path
            },
            expected_hit_substr="real-photo",
        )


class DesignerL72ProvenRedTest(unittest.TestCase, ProvenRedMixin):
    def test_proven_red_cards_after_cap2(self) -> None:
        self._prove(
            CE / "rules/design_fix_loop.py",
            {"full_regen_failures": 2, "localized_masked_edit": False},
            "localized-masked-edit",
        )


class DesignerAvatarLicenseBannedTest(unittest.TestCase):
    def test_banned_stack_and_bad_license_fail(self) -> None:
        mod = _load("ce_avatar2", CE / "rules/avatar_likeness_gate.py")
        obs = {
            "reference_conditioned": True,
            "real_photo_edit": True,
            "source_ref_photo_path": "/refs/a.jpg",
            "edit_provenance": "edit",
            "glasses_present": True,
            "age_within_refs": True,
            "sface_mean": 0.9,
            "sface_min": 0.8,
            "identity_score": 0.9,
            "fresh_side_by_side_review": True,
            "license": "proprietary",
            "stack": "insightface",
            "tools_receipt": ["insightface"],
        }
        result = mod.evaluate(obs)
        self.assertFalse(result["ok"])
        self.assertTrue(any("license-not-allowed" in h for h in result["hits"]))
        self.assertTrue(any("banned-stack" in h for h in result["hits"]))

    def test_missing_tools_receipt_fail_closed(self) -> None:
        mod = _load("ce_avatar3", CE / "rules/avatar_likeness_gate.py")
        obs = {
            "reference_conditioned": True,
            "real_photo_edit": True,
            "source_ref_photo_path": "/refs/a.jpg",
            "edit_provenance": "edit",
            "glasses_present": True,
            "age_within_refs": True,
            "sface_mean": 0.9,
            "sface_min": 0.8,
            "identity_score": 0.9,
            "fresh_side_by_side_review": True,
            "license": "mit",
            # no stack, no tools_receipt
        }
        result = mod.evaluate(obs)
        self.assertFalse(result["ok"])
        self.assertTrue(any("missing-tools-receipt" in h for h in result["hits"]))




class AdversarialWebUiArtifactsTest(unittest.TestCase):
    """r2: missing/[] render_artifacts FAIL; 390+1440 path+sha256 vs bytes."""

    def setUp(self) -> None:
        self.mod = _load("ce_web_ui_r2", CE / "rules/web_ui_gate.py")

    def _receipt(self) -> dict:
        return {
            "bidi_ok": True,
            "no_raw_urls_in_copy": True,
            "min_contrast_ratio": 4.5,
            "alignment_ok": True,
            "render_widths_px": [390, 1440],
            "independent_visual_review": True,
            "viewport": {"x": 0, "y": 0, "w": 390, "h": 800},
            "container": {"x": 0, "y": 0, "w": 390, "h": 800},
            "elements": [
                {
                    "id": "ok",
                    "bbox": {"x": 10, "y": 10, "w": 100, "h": 40},
                    "scrollWidth": 100,
                    "clientWidth": 100,
                }
            ],
        }

    def _write_png(self, path: Path, tag: bytes) -> str:
        path.write_bytes(b"PNGFIXTURE-" + tag)
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def test_missing_render_artifacts_fails(self) -> None:
        obs = self._receipt()
        result = self.mod.evaluate(obs)
        self.assertFalse(result["ok"])
        self.assertTrue(
            any("missing-render-artifacts" in h for h in result["hits"]),
            result["hits"],
        )

    def test_empty_render_artifacts_fails(self) -> None:
        obs = self._receipt()
        obs["render_artifacts"] = []
        result = self.mod.evaluate(obs)
        self.assertFalse(result["ok"])
        self.assertTrue(
            any("missing-render-artifacts" in h for h in result["hits"]),
            result["hits"],
        )

    def test_sha256_mismatch_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            p390 = root / "w390.png"
            p1440 = root / "w1440.png"
            digest390 = self._write_png(p390, b"390")
            digest1440 = self._write_png(p1440, b"1440")
            obs = self._receipt()
            obs["render_artifacts"] = [
                {"width_px": 390, "path": str(p390), "sha256": "deadbeef" * 8},
                {"width_px": 1440, "path": str(p1440), "sha256": digest1440},
            ]
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("sha256-mismatch" in h for h in result["hits"]),
                result["hits"],
            )
            # sanity: real digests differ from forged
            self.assertNotEqual(digest390, "deadbeef" * 8)

    def test_missing_width_pair_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            only = root / "w390.png"
            digest = self._write_png(only, b"only390")
            obs = self._receipt()
            obs["render_artifacts"] = [
                {"width_px": 390, "path": str(only), "sha256": digest},
            ]
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("missing-390-or-1440-render-artifact" in h for h in result["hits"]),
                result["hits"],
            )

    def test_good_artifacts_pass(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            p390 = root / "w390.png"
            p1440 = root / "w1440.png"
            obs = self._receipt()
            obs["render_artifacts"] = [
                {
                    "width_px": 390,
                    "path": str(p390),
                    "sha256": self._write_png(p390, b"390"),
                },
                {
                    "width_px": 1440,
                    "path": str(p1440),
                    "sha256": self._write_png(p1440, b"1440"),
                },
            ]
            result = self.mod.evaluate(obs)
            self.assertTrue(result["ok"], result["hits"])


class CraftQcWebUiCommandTest(unittest.TestCase):
    """r2: craft_qc web_ui wires rules/web_ui_gate (mggate pattern)."""

    def test_web_ui_command_fails_on_empty_artifacts(self) -> None:
        import subprocess

        design_qc = CE / "addons/design-skills/scripts/design_qc.py"
        self.assertTrue(design_qc.is_file())
        self.assertTrue(
            (CE / "addons/design-skills/scripts/craft_qc.py").is_file()
        )
        with tempfile.TemporaryDirectory() as tmp:
            obs_path = Path(tmp) / "obs.json"
            obs_path.write_text(
                json.dumps(
                    {
                        "bidi_ok": True,
                        "no_raw_urls_in_copy": True,
                        "min_contrast_ratio": 4.5,
                        "alignment_ok": True,
                        "render_widths_px": [390, 1440],
                        "independent_visual_review": True,
                        "render_artifacts": [],
                    }
                ),
                encoding="utf-8",
            )
            proc = subprocess.run(  # nosec B603
                [sys.executable, str(design_qc), "web_ui", str(obs_path)],
                check=False,
                capture_output=True,
                text=True,
                cwd=str(ROOT),
            )
            self.assertNotEqual(0, proc.returncode, proc.stdout + proc.stderr)
            payload = {}
            for line in (proc.stdout or "").splitlines():
                line = line.strip()
                if line.startswith("{") and line.endswith("}"):
                    try:
                        payload = json.loads(line)
                    except json.JSONDecodeError:
                        continue
            hits = payload.get("hits") or []
            self.assertTrue(
                any("missing-render-artifacts" in str(h) for h in hits),
                payload,
            )



if __name__ == "__main__":
    unittest.main()
