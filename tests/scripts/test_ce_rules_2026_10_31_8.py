"""CE 2026.10.31.8: L78–L81 + install perf + trainee 600+ + dump/install harden (L74)."""
from __future__ import annotations

import hashlib
import importlib.util
import json
import sys
import tempfile
from pathlib import Path
from unittest import TestCase, main, mock

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


class VersionBumpTest(TestCase):
    def test_version_is_31_8(self) -> None:
        self.assertEqual(
            "2026.10.31.8",
            (CE / "VERSION").read_text(encoding="utf-8").strip(),
        )


class L74ProvenRedHelperTest(TestCase):
    def setUp(self) -> None:
        self.proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")

    def test_helper_requires_fail(self) -> None:
        with self.assertRaises(AssertionError):
            self.proven.prove_red(lambda _obs: {"ok": True, "hits": []}, {})


class L78ReviewScratchTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load(
            "ce_review_scratch", ROOT / "scripts/ci/review_scratch.py"
        )
        self.proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")

    def test_proven_red_unpushed_tip(self) -> None:
        with tempfile.TemporaryDirectory(prefix="ce-review-") as tmp:
            bad = {
                "kind": "detached-pushed",
                "pushed": False,
                "path": tmp,
                "repo": str(ROOT),
                "sha": "0" * 40,
                "unpushed_tip": True,
            }
            result = self.proven.prove_red(
                self.mod.evaluate, bad, expected_hit_substr="unpushed"
            )
            self.assertFalse(result["ok"])

    def test_proven_red_wrong_kind(self) -> None:
        with tempfile.TemporaryDirectory(prefix="ce-review-") as tmp:
            bad = {
                "kind": "linked-dirty",
                "pushed": True,
                "path": tmp,
                "repo": str(ROOT),
                "sha": "0" * 40,
            }
            result = self.proven.prove_red(
                self.mod.evaluate, bad, expected_hit_substr="scratch-kind"
            )
        self.assertIn("scratch-kind-not-archive-or-detached-pushed", result["hits"])

    def test_proven_red_forged_pushed_missing_path(self) -> None:
        """L74: evaluate must not trust pushed:True for a nonexistent path."""
        bad = {
            "kind": "archive",
            "pushed": True,
            "path": str(Path(tempfile.gettempdir()) / "ce-review-does-not-exist-xyz"),
            "repo": str(ROOT),
            "sha": "0" * 40,
        }
        with mock.patch.object(self.mod, "sha_on_origin", return_value=True):
            result = self.proven.prove_red(
                self.mod.evaluate, bad, expected_hit_substr="scratch-path-missing"
            )
        self.assertFalse(result["ok"])

    def test_proven_red_detached_without_git(self) -> None:
        with tempfile.TemporaryDirectory(prefix="ce-review-") as tmp:
            bad = {
                "kind": "detached-pushed",
                "pushed": True,
                "path": tmp,
                "repo": str(ROOT),
                "sha": "0" * 40,
            }
            with mock.patch.object(self.mod, "sha_on_origin", return_value=True):
                result = self.proven.prove_red(
                    self.mod.evaluate,
                    bad,
                    expected_hit_substr="detached-scratch-missing-git",
                )
        self.assertFalse(result["ok"])

    def test_proven_red_forged_pushed_not_on_origin(self) -> None:
        with tempfile.TemporaryDirectory(prefix="ce-review-") as tmp:
            bad = {
                "kind": "archive",
                "pushed": True,
                "path": tmp,
                "repo": str(ROOT),
                "sha": "deadbeef" + "0" * 32,
            }
            with mock.patch.object(self.mod, "sha_on_origin", return_value=False):
                result = self.proven.prove_red(
                    self.mod.evaluate, bad, expected_hit_substr="scratch-sha-not-on-origin"
                )
        self.assertFalse(result["ok"])

    def test_proven_red_archive_under_workspace(self) -> None:
        """L74: TMPDIR=/workspace style archive must FAIL."""
        ws = Path("/workspace")
        if not ws.is_dir():
            self.skipTest("no /workspace on this host")
        with tempfile.TemporaryDirectory(prefix="ce-review-", dir=str(ws)) as tmp:
            bad = {
                "kind": "archive",
                "pushed": True,
                "path": tmp,
                "repo": str(ROOT),
                "sha": "0" * 40,
            }
            with mock.patch.object(self.mod, "sha_on_origin", return_value=True):
                result = self.proven.prove_red(
                    self.mod.evaluate,
                    bad,
                    expected_hit_substr="workspace",
                )
        self.assertFalse(result["ok"])

    def test_archive_under_tmp_passes(self) -> None:
        with tempfile.TemporaryDirectory(prefix="ce-review-ok-") as tmp:
            obs = {
                "kind": "archive",
                "pushed": True,
                "path": tmp,
                "repo": str(ROOT),
                "sha": "8786fbe2785505c9ea30c09a85604da2107d03a1",
            }
            with mock.patch.object(self.mod, "sha_on_origin", return_value=True):
                result = self.mod.evaluate(obs)
            self.assertTrue(result["ok"], result)

    def test_create_uses_mkdtemp_not_hardcoded_tmp(self) -> None:
        """Codacy B108: create_archive_scratch must not pass dir='/tmp'."""
        import inspect
        src = inspect.getsource(self.mod.create_archive_scratch)
        self.assertNotIn('dir="/tmp"', src)
        self.assertNotIn("dir='/tmp'", src)
        self.assertIn("mkdtemp", src)


class L79CmsBlockIdentityTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_cms_block_identity", CE / "rules/cms_block_identity.py")
        self.proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")

    def test_proven_red_mismatch_on_paste(self) -> None:
        bad = {
            "block_id": "home-faq",
            "current_block_text": "FAQ body here",
            "expected_pre_text": "CTA band text",
            "paste": True,
            "readback_before_paste": True,
        }
        result = self.proven.prove_red(
            self.mod.evaluate, bad, expected_hit_substr="block-identity-mismatch"
        )
        self.assertIn("paste-or-save-with-wrong-block", result["hits"])

    def test_proven_red_missing_readback_before_save(self) -> None:
        bad = {
            "block_id": "home-faq",
            "current_block_text": "same",
            "expected_pre_text": "same",
            "save": True,
            "readback_before_save": False,
        }
        result = self.proven.prove_red(
            self.mod.evaluate, bad, expected_hit_substr="readback-before-save"
        )
        self.assertFalse(result["ok"])

    def test_matching_identity_passes(self) -> None:
        obs = {
            "block_id": "home-faq",
            "current_block_text": "  FAQ  body ",
            "expected_pre_text": "FAQ body",
            "paste": True,
            "readback_before_paste": True,
        }
        self.assertTrue(self.mod.evaluate(obs)["ok"])


class L80TestRunBudgetTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_task_ledger", CE / "task_ledger.py")
        self.proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")

    def test_kind_test_run_is_90_with_proof(self) -> None:
        self.assertEqual(90.0, self.mod.kind_budget_minutes("test-run"))
        self.assertEqual(
            90.0,
            self.mod.resolve_budget_minutes(
                kind="test-run",
                minutes=35.0,
                explicit=False,
                task_cmd="mvn test -Dtest=Foo",
            ),
        )

    def test_test_run_without_proof_stays_35(self) -> None:
        self.assertEqual(
            35.0,
            self.mod.resolve_budget_minutes(
                kind="test-run", minutes=35.0, explicit=False
            ),
        )

    def test_explicit_minutes_win(self) -> None:
        self.assertEqual(
            40.0,
            self.mod.resolve_budget_minutes(
                kind="test-run", minutes=40.0, explicit=True
            ),
        )

    def test_default_stays_35(self) -> None:
        self.assertEqual(
            35.0,
            self.mod.resolve_budget_minutes(kind=None, minutes=35.0, explicit=False),
        )

    def test_proven_red_test_run_kind_abuse(self) -> None:
        """L74: kind=test-run with task_class=cleanup|pr|design must FAIL."""
        bad = {
            "kind": "test-run",
            "task_class": "cleanup",
            "task_cmd": "mvn test",
        }
        result = self.proven.prove_red(
            self.mod.evaluate_test_run_kind,
            bad,
            expected_hit_substr="test-run-kind-abuse",
        )
        self.assertFalse(result["ok"])
        self.assertEqual(
            35.0,
            self.mod.resolve_budget_minutes(
                kind="test-run",
                task_class="pr",
                task_cmd="pytest",
                explicit=False,
            ),
        )

    def test_proven_red_test_run_missing_proof(self) -> None:
        bad = {"kind": "test-run", "task_class": "qa"}
        result = self.proven.prove_red(
            self.mod.evaluate_test_run_kind,
            bad,
            expected_hit_substr="test-run-missing-proof",
        )
        self.assertFalse(result["ok"])


class L81NoSelfClockTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load(
            "ce_motion_graphics_gate",
            CE / "addons/design-skills/scripts/motion_graphics_gate.py",
        )
        self.proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")

    def _write_asr(self, tmp: Path, words: list[dict]) -> tuple[str, str]:
        path = tmp / "asr.json"
        path.write_text(json.dumps({"words": words}), encoding="utf-8")
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        return str(path), digest

    def _base_obs(self, asr_path: str, digest: str, captions: list[dict]) -> dict:
        return {
            "transcript_manually_verified": True,
            "reviewer_brief_lists_mg_checks": True,
            "kind": "kinetic",
            "static_text_blocks": [],
            "ocr_frames": [{"text": "ok", "expected": "ok"}],
            "diagrams": [{"animation": "draw-on"}],
            "asr_path": asr_path,
            "asr_sha256": digest,
            "asr_tool": "whisper",
            "asr_source": "audio",
            "captions": captions,
        }

    def test_proven_red_mid_speech_voiced_self_clock(self) -> None:
        bad = {
            "onset_rows": [
                {
                    "cue": 8,
                    "cue_s": 15.70,
                    "phrase_onset": 15.70,
                    "onset_method": "mid_speech_voiced",
                }
            ]
        }
        result = self.proven.prove_red(
            lambda obs: {
                "ok": not self.mod.self_clock_hits(obs),
                "hits": self.mod.self_clock_hits(obs),
            },
            bad,
            expected_hit_substr="self-clock",
        )
        self.assertTrue(any("mid_speech_voiced" in h for h in result["hits"]))

    def test_proven_red_energy_peak_onset_eq_cue(self) -> None:
        """L74: renamed/benign method with onset==cue_s must FAIL (value-based)."""
        bad = {
            "onset_rows": [
                {
                    "cue_s": 15.70,
                    "phrase_onset": 15.70,
                    "onset_method": "energy_peak",
                }
            ]
        }
        result = self.proven.prove_red(
            lambda obs: {
                "ok": not self.mod.self_clock_hits(obs),
                "hits": self.mod.self_clock_hits(obs),
            },
            bad,
            expected_hit_substr="self-clock",
        )
        self.assertTrue(any("energy_peak" in h for h in result["hits"]))

    def test_proven_red_empty_method_onset_eq_cue(self) -> None:
        bad = {
            "onset_rows": [
                {"cue_s": 10.0, "phrase_onset": 10.0, "onset_method": ""}
            ]
        }
        result = self.proven.prove_red(
            lambda obs: {
                "ok": not self.mod.self_clock_hits(obs),
                "hits": self.mod.self_clock_hits(obs),
            },
            bad,
            expected_hit_substr="self-clock",
        )
        self.assertFalse(result["ok"])

    def test_mutant_mid_speech_cue_plus_500ms_fails(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            # Audio words at true speech times
            asr_path, digest = self._write_asr(
                tmp_path,
                [
                    {"word": "retrieve", "start": 15.34},
                    {"word": "web", "start": 26.44},
                    {"word": "ok", "start": 1.0},
                ],
            )
            # Cue 0 is mid-speech but shifted +500ms late vs ASR
            captions = [
                {"start": 15.84, "lines": ["retrieve"]},  # +500ms vs 15.34
                {"start": 26.44, "lines": ["web"]},
            ]
            obs = self._base_obs(asr_path, digest, captions)
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"], result)
            self.assertTrue(
                any("onset>" in h for h in result["hits"]),
                result["hits"],
            )

    def test_no_onset_fails_not_zero(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            asr_path, digest = self._write_asr(
                tmp_path, [{"word": "hello", "start": 1.0}]
            )
            captions = [{"start": 2.0, "lines": ["missingword"]}]
            obs = self._base_obs(asr_path, digest, captions)
            result = self.mod.evaluate(obs)
            self.assertFalse(result["ok"])
            self.assertTrue(
                any("unmatched-asr-word" in h for h in result["hits"]),
                result["hits"],
            )


class InstallPerfFastPathTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_worktree_overlay", CE / "worktree_overlay.py")

    def test_slow_mark_is_30(self) -> None:
        self.assertEqual(30.0, self.mod.SLOW_INSTALL_MARK_S)

    def test_healthy_first_doctor_skips_second_pass(self) -> None:
        """L74/PERF: when first doctor is healthy, mode=module-fast and one pass."""

        class FakeModule:
            def doctor_with_dependencies(self, session, verify_clients=False):
                return {"status": "healthy", "components": []}

        with mock.patch.object(self.mod, "_load_install_module", return_value=FakeModule()):
            with mock.patch.object(self.mod, "_unhealthy_with_hosts", return_value=[]):
                with mock.patch.object(self.mod, "_blocking_doctor_failures", return_value=[]):
                    with mock.patch.object(self.mod, "_run_repairs") as repairs:
                        with tempfile.TemporaryDirectory() as _td:
                            _p = Path(_td)
                            result = self.mod._post_upgrade_repair_and_verify(_p, _p)
        self.assertTrue(result["doctor_ok"])
        self.assertEqual("module-fast", result["mode"])
        self.assertEqual(1, result["doctor_passes"])
        repairs.assert_not_called()


class TraineeCountFactTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_public_facts", CE / "rules/public_facts.py")
        self.proven = _load("ce_gate_proven_red", CE / "rules/gate_proven_red.py")

    def test_proven_red_484(self) -> None:
        bad = {"trainee_count": "484", "assert_public_trainee_figure": True}
        result = self.proven.prove_red(
            self.mod.evaluate, bad, expected_hit_substr="484"
        )
        self.assertFalse(result["ok"])

    def test_600_plus_passes(self) -> None:
        obs = {
            "trainee_count": "600+ trainees",
            "assert_public_trainee_figure": True,
            "text": "We trained 600+ trainees.",
        }
        self.assertTrue(self.mod.evaluate(obs)["ok"], self.mod.evaluate(obs))

    def test_arabic_form_passes(self) -> None:
        obs = {
            "trainee_count": "أكتر من 600 متدرب",
            "assert_public_trainee_figure": True,
        }
        self.assertTrue(self.mod.evaluate(obs)["ok"])

    def test_constants(self) -> None:
        self.assertEqual("600+ trainees", self.mod.TRAINEE_COUNT_PUBLIC)
        self.assertIn("484", self.mod.SUPERSEDED_TRAINEE_COUNTS)


class SpecAndMapPresentTest(TestCase):
    def test_spec_files_exist(self) -> None:
        self.assertTrue((CE / "guides/ce-rules-2026-10-31-8-SPEC.md").is_file())
        self.assertTrue((CE / "references/ce-rules-2026-10-31-8-SPEC.md").is_file())
        for name in (
            "review-scratch-cleanup.md",
            "cms-block-identity.md",
            "test-run-budget.md",
            "no-self-clock.md",
            "install-perf-30s.md",
            "trainee-count-600.md",
            "memory-dump-own-shard.md",
            "install-download-retry.md",
        ):
            self.assertTrue(
                (CE / "references/details/ce-rules-2026-10-31-8" / name).is_file(),
                name,
            )


class MemoryDumpOwnShardTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_memory_full_dump_318", CE / "rules/memory_full_dump.py")
        self.proven = _load("ce_gate_proven_red_318b", CE / "rules/gate_proven_red.py")

    def test_proven_red_host_pool_as_expected_with_bot(self) -> None:
        """L74: Carl 82/209 — host_total alone must NOT count_ok when bot is set."""
        batches = [self.mod.QueryBatch("q", [f"m{i}" for i in range(82)], top_n=50)]
        structured = [{"text": f"m{i}", "bot": "carl"} for i in range(82)]
        _, report = self.mod.merge_batches(
            batches, bot="carl", host_total=209, structured_hits=structured
        )
        self.assertFalse(report.count_ok)
        self.assertTrue(
            any("shard_total" in n or "own shard" in n for n in report.notes)
        )

    def test_own_shard_total_passes(self) -> None:
        structured = [
            {"text": "a", "bot": "carl", "date": "2026-10-01", "type": "fact", "id": "id-a"},
            {"text": "b", "bot": "carl", "date": "2026-10-02", "type": "fact", "id": "id-b"},
            {"text": "shared", "bot": "", "date": "2026-10-01", "type": "fact", "id": "id-s"},
            {"text": "x", "bot": "cleo", "date": "2026-10-01", "type": "fact", "id": "id-x"},
        ]
        texts, report = self.mod.merge_batches(
            [],
            bot="carl",
            shard_total=2,
            structured_hits=structured,
            known_shard_ids=["id-a", "id-b"],
        )
        self.assertEqual({"a", "b"}, set(texts))
        self.assertTrue(report.count_ok)

    def test_dedupe_by_text_date_type(self) -> None:
        structured = [
            {"text": "same", "bot": "carl", "date": "d1", "type": "t1", "id": "i1"},
            {"text": "same", "bot": "carl", "date": "d1", "type": "t1", "id": "i1"},
            {"text": "same", "bot": "carl", "date": "d1", "type": "t2", "id": "i2"},
        ]
        texts, report = self.mod.merge_batches(
            [],
            bot="carl",
            shard_total=2,
            structured_hits=structured,
            known_shard_ids=["i1", "i2"],
        )
        self.assertEqual(2, report.exported_unique)
        self.assertTrue(report.count_ok)

    def test_proven_red_forged_expected_vs_known_ids(self) -> None:
        """L74: forge --expected/shard_total to short export while known ids are larger."""
        structured = [
            {"text": f"m{i}", "bot": "carl", "id": f"id-{i}"} for i in range(82)
        ]
        known = [f"id-{i}" for i in range(209)]

        def gate(obs):
            _, report = self.mod.merge_batches(
                [],
                bot=obs["bot"],
                shard_total=obs["shard_total"],
                structured_hits=obs["structured"],
                known_shard_ids=obs["known"],
                transcript_lookup=lambda _f: None,
            )
            hits = []
            if not report.count_ok:
                hits.append("forged-expected-or-incomplete")
            if any("mismatches host-derived" in n for n in report.notes):
                hits.append("claimed-mismatch-host-derived")
            return {"ok": report.count_ok, "hits": hits, "notes": report.notes}

        bad = {
            "bot": "carl",
            "shard_total": 82,
            "structured": structured,
            "known": known,
        }
        result = self.proven.prove_red(
            gate, bad, expected_hit_substr="claimed-mismatch-host-derived"
        )
        self.assertFalse(result["ok"])

    def test_proven_red_truncated_without_transcript(self) -> None:
        """L74: truncated fact with no ReadTranscript recovery → uncaptured + fail."""
        partial = "Long fact that got cut off mid sentence by the host…"
        _, report = self.mod.merge_batches(
            [],
            bot="carl",
            shard_total=1,
            structured_hits=[
                {"text": partial, "bot": "carl", "truncated": True, "id": "fact-9"}
            ],
            transcript_lookup=lambda _f: None,
        )
        self.assertFalse(report.count_ok)
        self.assertIn("fact-9", report.uncaptured_ids)
        self.assertTrue(report.transcript_fallback_required)

    def test_truncated_recovered_via_transcript(self) -> None:
        partial = "Long fact that got cut off mid sentence by the host…"
        full = (
            "Long fact that got cut off mid sentence by the host UI "
            "but the transcript has the rest."
        )

        def lookup(fact):
            if fact.fact_id == "fact-9" or fact.text.startswith("Long fact"):
                return full
            return None

        texts, report = self.mod.merge_batches(
            [],
            bot="carl",
            shard_total=1,
            structured_hits=[
                {"text": partial, "bot": "carl", "truncated": True, "id": "fact-9"}
            ],
            transcript_lookup=lookup,
            known_shard_ids=["fact-9"],
        )
        self.assertTrue(report.count_ok)
        self.assertEqual(1, report.truncated_recovered)
        self.assertEqual([full], texts)

    def test_cli_merges_batches_jsonl(self) -> None:
        from pathlib import Path as P

        with tempfile.TemporaryDirectory() as tmp:
            root = P(tmp)
            batches = root / "batches.jsonl"
            line = json.dumps(
                {
                    "query": "q1",
                    "hits": [
                        {"text": "one", "bot": "carl", "date": "d", "type": "t", "id": "o1"},
                        {"text": "two", "bot": "carl", "date": "d", "type": "t", "id": "o2"},
                    ],
                }
            )
            batches.write_text(line + chr(10), encoding="utf-8")
            known = root / "known.txt"
            known.write_text("o1\no2\n", encoding="utf-8")
            out = root / "dump.md"
            receipt = root / "r.json"
            rc = self.mod.run_cli(
                [
                    "--batches",
                    str(batches),
                    "--expected",
                    "2",
                    "--out",
                    str(out),
                    "--receipt",
                    str(receipt),
                    "--bot",
                    "carl",
                    "--known-ids",
                    str(known),
                ]
            )
            self.assertEqual(0, rc)
            data = json.loads(receipt.read_text(encoding="utf-8"))
            self.assertTrue(data["count_ok"])
            self.assertIn("one", out.read_text(encoding="utf-8"))


class InstallDownloadRetryTest(TestCase):
    def setUp(self) -> None:
        self.mod = _load("ce_bootstrap_318", CE / "bootstrap.py")

    def test_proven_red_ssl_eof_used_to_skip_retry(self) -> None:
        """L74: ssl.SSLEOFError must get a backoff (old path returned None)."""
        import ssl

        err = ssl.SSLEOFError(8, "[SSL: UNEXPECTED_EOF_WHILE_READING] EOF")
        self.assertTrue(self.mod.is_transient_network_error(err))
        delay = self.mod.retry_delay(err, 0)
        self.assertIsNotNone(delay)
        self.assertEqual(self.mod.RETRY_BASE_SECONDS, delay)
        self.assertEqual(
            self.mod.RETRY_BASE_SECONDS * 2, self.mod.retry_delay(err, 1)
        )

    def test_read_response_retries_ssl_eof(self) -> None:
        import ssl
        
        good = mock.MagicMock()
        good.read.side_effect = [b"ok-body", b""]
        good.__enter__.return_value = good
        good.__exit__.return_value = None
        calls = {"n": 0}

        def opener(_req, timeout=30):
            calls["n"] += 1
            if calls["n"] == 1:
                raise ssl.SSLEOFError(8, "[SSL: UNEXPECTED_EOF_WHILE_READING]")
            return good

        sleeper = mock.Mock()
        value = self.mod.read_response(
            opener, "https://raw.githubusercontent.com/x/y/z", sleeper=sleeper
        )
        self.assertEqual(b"ok-body", value)
        self.assertEqual(2, calls["n"])
        sleeper.assert_called()

    def test_local_clone_fallback_after_network_failure(self) -> None:
        import tempfile
        from pathlib import Path as P
        
        with tempfile.TemporaryDirectory() as tmp:
            dest = P(tmp) / "dest"
            dest.mkdir()
            # Point env at this worktree as the local clone.
            with mock.patch.dict(
                self.mod.os.environ,
                {self.mod.LOCAL_CLONE_ENV: str(ROOT)},
            ):
                with mock.patch.object(
                    self.mod,
                    "download_source_remote",
                    side_effect=RuntimeError(
                        "unable to resolve latest ChaosEngine from the configured upstream"
                    ),
                ):
                    source = self.mod.download_source(
                        "owner/repo",
                        "0" * 40,
                        dest,
                    )
            self.assertTrue((source / "VERSION").is_file())
            self.assertTrue((source / "skills/chaos-engine/SKILL.md").is_file())

    def test_proven_red_same_size_corrupt_body_fails_hash(self) -> None:
        """L74: size-matched corrupt body must FAIL git blob sha check."""
        proven = _load("ce_gate_proven_red_inst", CE / "rules/gate_proven_red.py")
        good = b"hello-chaos-engine-source"
        bad = b"HELLO-CHAOS-ENGINE-SOURCE"  # same length
        self.assertEqual(len(good), len(bad))
        oid = self.mod.git_blob_sha1(good)
        self.assertNotEqual(oid, self.mod.git_blob_sha1(bad))
        tree = [
            {
                "path": "chaos-engine/VERSION",
                "type": "blob",
                "mode": "100644",
                "size": len(good),
                "sha": oid,
            },
            {
                "path": "chaos-engine/skills/chaos-engine/SKILL.md",
                "type": "blob",
                "mode": "100644",
                "size": len(good),
                "sha": oid,
            },
        ]
        selected, _total, _exe = self.mod._select_source_blobs(tree)
        self.assertEqual(2, len(selected))
        hits = []
        for _path, expected_size, expected_sha in selected:
            content = bad
            if len(content) != expected_size:
                hits.append("size-mismatch")
            elif self.mod.git_blob_sha1(content).casefold() != expected_sha.casefold():
                hits.append("hash-mismatch")
        self.assertIn("hash-mismatch", hits)
        result = proven.prove_red(
            lambda obs: {
                "ok": self.mod.git_blob_sha1(obs["body"]) == obs["sha"],
                "hits": (
                    ["hash-mismatch"]
                    if self.mod.git_blob_sha1(obs["body"]) != obs["sha"]
                    else []
                ),
            },
            {"body": bad, "sha": oid},
            expected_hit_substr="hash-mismatch",
        )
        self.assertFalse(result["ok"])

    def test_maintain_fetch_retries_gnutls(self) -> None:
        """L74: GnuTLS fetch failure is retried (Cortex field report)."""
        tool = _load("ce_tool_318", CE / "tool.py")
        from pathlib import Path as P
        import subprocess

        calls = {"fetch": 0}

        def runner(cmd, **kwargs):
            # _maintain_git argv: [git, subcommand, ...]
            sub = cmd[1] if len(cmd) > 1 else ""
            if sub == "fetch":
                calls["fetch"] += 1
                if calls["fetch"] < 3:
                    return subprocess.CompletedProcess(
                        cmd,
                        1,
                        stdout="",
                        stderr="GnuTLS, handshake failed: The TLS connection was non-properly terminated.",
                    )
                return subprocess.CompletedProcess(cmd, 0, stdout="", stderr="")
            if sub == "rev-parse":
                return subprocess.CompletedProcess(cmd, 0, stdout="main\n", stderr="")
            if sub == "rev-list":
                return subprocess.CompletedProcess(cmd, 0, stdout="0\n", stderr="")
            if sub == "status":
                return subprocess.CompletedProcess(cmd, 0, stdout="", stderr="")
            return subprocess.CompletedProcess(cmd, 0, stdout="", stderr="")

        with mock.patch.object(tool, "default_branch_name", return_value="main"):
            with mock.patch.object(tool, "time") as time_mod:
                time_mod.sleep = mock.Mock()
                with tempfile.TemporaryDirectory() as _td:
                    ok, msg = tool.maintain_sync(P(_td), runner=runner)
        self.assertTrue(ok)
        self.assertGreaterEqual(calls["fetch"], 3)


if __name__ == "__main__":
    main()
