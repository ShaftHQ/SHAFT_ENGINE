"""CE 2026.10.31.6 consolidated retro gates (H-* + L60–L65 + H-IDENTITY/H-SCOPE/H-PR-SCOPE)."""
from __future__ import annotations

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DESIGN_QC = ROOT / "chaos-engine/addons/design-skills/scripts/design_qc.py"
CRAFT_QC = ROOT / "chaos-engine/addons/design-skills/scripts/craft_qc.py"


def run_design(*argv: str) -> tuple[int, dict]:
    proc = subprocess.run(  # nosec B603 — fixed argv: local design_qc + test paths
        [sys.executable, str(DESIGN_QC), *argv],
        check=False,
        capture_output=True,
        text=True,
        cwd=str(ROOT),
    )
    payload = {}
    for line in (proc.stdout or "").splitlines():
        line = line.strip()
        if line.startswith("{") and line.endswith("}"):
            try:
                payload = json.loads(line)
            except json.JSONDecodeError:
                # Non-JSON stdout lines from CLI helpers are ignored on purpose.
                continue
    return proc.returncode, payload


class VersionBumpGateTest(unittest.TestCase):
    def test_requires_version_when_ce_changes(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.overlay_pre_push import ce_version_bump_failures

        hits = ce_version_bump_failures(
            ROOT, ["chaos-engine/references/bot-entry.md"],
        )
        # VERSION is bumped in this PR tree, but the helper looks at *changed*
        # paths only — without VERSION in the list it must fail.
        self.assertTrue(hits)
        self.assertIn("ce-version-bump", hits[0])

    def test_pass_when_version_in_paths(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.overlay_pre_push import ce_version_bump_failures

        hits = ce_version_bump_failures(
            ROOT,
            [
                "chaos-engine/references/bot-entry.md",
                "chaos-engine/VERSION",
            ],
        )
        self.assertEqual([], hits)


class RadioSilenceTest(unittest.TestCase):
    def test_env_blocks(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import radio_silence

        self.assertTrue(
            radio_silence.user_send_blocked(environ={"CHAOS_RADIO_SILENCE": "1"})
        )
        self.assertFalse(radio_silence.user_send_blocked(environ={}))
        self.assertEqual(
            radio_silence.status_channel(blocked=True), "orchestrator-only"
        )


class ProgressLogTest(unittest.TestCase):
    def test_append_grows(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import progress_log

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            path = progress_log.append_step(
                "demo", "step-a", next_action="step-b", checkpoints_dir=root,
            )
            first = path.read_text(encoding="utf-8")
            progress_log.append_step("demo", "step-b", checkpoints_dir=root)
            second = path.read_text(encoding="utf-8")
            self.assertIn("step-a", first)
            self.assertGreater(len(second), len(first))
            self.assertIn("step-b", second)


class MovingChannelPinTest(unittest.TestCase):
    def test_min_and_exact(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import moving_channel_pin as pin

        self.assertTrue(pin.channel_pin_allows("37.2.13", "37.2.12", mode="min"))
        self.assertFalse(pin.channel_pin_allows("36.9.0", "37.2.12", mode="min"))
        self.assertFalse(pin.channel_pin_allows("37.2.13", "37.2.12", mode="exact"))
        self.assertTrue(pin.channel_pin_allows("37.2.12", "37.2.12", mode="exact"))


class FailClosedTemplateTest(unittest.TestCase):
    def test_missing_file_and_empty_obs(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import gate_fail_closed as gfc

        self.assertEqual(["missing-input"], gfc.require_file(None))
        with tempfile.TemporaryDirectory() as temporary:
            empty = Path(temporary) / "empty.json"
            empty.write_text("{}", encoding="utf-8")
            raw, hits = gfc.require_observations(empty)
            self.assertIsNone(raw)
            self.assertTrue(hits)


class WorkerOwnershipTest(unittest.TestCase):
    def test_record_and_allows(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci import worker_ownership as wo

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            created = root / "wt-demo"
            created.mkdir()
            manifest = wo.record(
                "demo", created, kind="worktree", checkpoints_dir=root,
            )
            self.assertTrue(wo.allows_cleanup(manifest, created))
            other = root / "foreign"
            other.mkdir()
            self.assertFalse(wo.allows_cleanup(manifest, other))


class SemanticBaselineTest(unittest.TestCase):
    def test_refresh_updates_hash_keeps_reason(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.skillspector.semantic_baseline import refresh_entry, semantic_id

        baseline = {"version": 2, "fingerprints": []}
        first = refresh_entry(
            baseline,
            rule_id="LP4",
            file="SKILL.md",
            new_hash="sha256:aaa",
            reason="keep-me",
        )
        self.assertEqual("keep-me", first["reason"])
        sid = semantic_id("LP4", "SKILL.md")
        self.assertEqual(sid, first["semantic_id"])
        second = refresh_entry(
            baseline,
            rule_id="LP4",
            file="SKILL.md",
            new_hash="sha256:bbb",
        )
        self.assertEqual("sha256:bbb", second["hash"])
        self.assertEqual("keep-me", second["reason"])
        self.assertEqual(1, len(baseline["fingerprints"]))


class InfraPreflightTest(unittest.TestCase):
    def test_swap_missing(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci import box_disk_guard as bdg

        reasons = bdg.blocked_infra_reasons(
            ROOT, free=100 * 1024**3, used_percent=10.0, swap_kib=0,
        )
        self.assertIn("swap-missing", reasons)
        reasons_ok = bdg.blocked_infra_reasons(
            ROOT, free=100 * 1024**3, used_percent=10.0, swap_kib=1024,
            apt_mirror_ok=False,
        )
        self.assertIn("apt-mirror-unreachable", reasons_ok)
        self.assertNotIn("swap-missing", reasons_ok)


class LikenessProvenanceTest(unittest.TestCase):
    def _mp4(self, root: Path) -> Path:
        mp4 = root / "shot.mp4"
        mp4.write_bytes(b"\x00\x00\x00\x18ftypmp42")
        return mp4

    def test_ai_ref_provenance_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            mp4 = self._mp4(root)
            obs = root / "obs.json"
            obs.write_text(
                json.dumps(
                    {
                        "independent_reviewer": True,
                        "blind": True,
                        "likeness": {
                            "verdict": "pass",
                            "side_by_side": True,
                            "refs": [
                                {
                                    "kind": "ai_still",
                                    "sha256": "x",
                                    "source_path": "ai.jpg",
                                }
                            ],
                        },
                        "voice": {
                            "verdict": "pass",
                            "side_by_side": True,
                            "refs": [
                                {
                                    "kind": "real_audio",
                                    "sha256": "y",
                                    "source_path": "real.wav",
                                }
                            ],
                        },
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_design(
                "likenessvoice", str(mp4), "--observations", str(obs),
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(
                any("banned-kind" in h for h in payload.get("hits", [])), payload
            )

    def test_real_provenance_pass(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            mp4 = self._mp4(root)
            obs = root / "obs.json"
            obs.write_text(
                json.dumps(
                    {
                        "independent_reviewer": True,
                        "blind": True,
                        "builder_id": "a",
                        "reviewer_id": "b",
                        "likeness": {
                            "verdict": "pass",
                            "side_by_side": True,
                            "refs": [
                                {
                                    "kind": "real_photo",
                                    "sha256": "x",
                                    "source_url": "https://example.invalid/p",
                                }
                            ],
                        },
                        "voice": {
                            "verdict": "pass",
                            "side_by_side": True,
                            "refs": [
                                {
                                    "kind": "real_audio",
                                    "sha256": "y",
                                    "source_path": "/refs/voice.wav",
                                }
                            ],
                        },
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_design(
                "likenessvoice", str(mp4), "--observations", str(obs),
            )
            self.assertEqual(0, code, payload)


class ScheduleDupTest(unittest.TestCase):
    def test_missing_inventory_fails(self) -> None:
        code, payload = run_design(
            "scheduledup",
            "--inventory", "/no/such/inventory.json",
            "--title", "Hello",
        )
        self.assertEqual(1, code, payload)
        self.assertIn("missing-inventory", payload.get("hits", []))

    def test_dup_and_clean(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            inv = root / "inv.json"
            inv.write_text(
                json.dumps(
                    {
                        "units": [
                            {
                                "id": "U1",
                                "title": "Hello World",
                                "start": "1.0",
                                "end": "2.0",
                            }
                        ]
                    }
                ),
                encoding="utf-8",
            )
            code, payload = run_design(
                "scheduledup",
                "--inventory", str(inv),
                "--title", "hello world",
                "--start", "1.0",
                "--end", "2.0",
            )
            self.assertEqual(1, code, payload)
            inv2 = root / "inv2.json"
            inv2.write_text(json.dumps({"units": [{"id": "U2", "title": "Other"}]}), encoding="utf-8")
            code, payload = run_design(
                "scheduledup", "--inventory", str(inv2), "--title", "hello world",
            )
            self.assertEqual(0, code, payload)


class SignedRendersNeverTouchTest(unittest.TestCase):
    def test_never_touch(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import delete_guard

        hit = delete_guard.never_touch(Path("/workspace/signed-renders/UNIT/a.mp4"))
        self.assertIn("signed-renders", hit)


class KaraokeHumanFragTest(unittest.TestCase):
    def test_short_frag_fails_and_clean_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            bad = root / "bad.ass"
            bad.write_text(
                "Dialogue: 0,0:00:01.00,0:00:02.00,Human,,0,0,0,,{" + chr(92) + "k40}hi there\n",
                encoding="utf-8",
            )
            code, payload = run_design("karaokehumanfrag", str(bad))
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)
            good = root / "good.ass"
            good.write_text(
                "Dialogue: 0,0:00:01.00,0:00:03.00,Human,,0,0,0,,phrase plate only\n",
                encoding="utf-8",
            )
            code, payload = run_design("karaokehumanfrag", str(good))
            self.assertEqual(0, code, payload)

    def test_missing_input_fails(self) -> None:
        code, payload = run_design("karaokehumanfrag", "/no/such.ass")
        self.assertEqual(1, code, payload)
        self.assertIn("missing-input", payload.get("hits", []))


class DocAndCardPresenceTest(unittest.TestCase):
    def test_cards_and_docs(self) -> None:
        version = (ROOT / "chaos-engine/VERSION").read_text(encoding="utf-8").strip()
        self.assertEqual("2026.10.31.6", version)
        for rel in (
            "chaos-engine/references/radio-silence.md",
            "chaos-engine/references/gate-fail-closed.md",
            "chaos-engine/references/doc-claim-review.md",
            "chaos-engine/references/progress-log.md",
            "chaos-engine/references/moving-channel-pin.md",
            "chaos-engine/addons/design-skills/references/lessons/schedule-dup-before-rebuild.md",
            "chaos-engine/addons/design-skills/references/lessons/signed-renders-store.md",
            "chaos-engine/addons/design-skills/references/lessons/karaoke-human-frag.md",
            "chaos-engine/addons/design-skills/references/lessons/uncaptioned-speech.md",
            "chaos-engine/references/pr-scope-check.md",
            "chaos-engine/references/h-scope.md",
            "chaos-engine/references/ce-retro-2026-10-31-6-SPEC.md",
            "chaos-engine/guides/ce-retro-2026-10-31-6-SPEC.md",
            "chaos-engine/addons/design-skills/scripts/uncaptioned_speech.py",
            "chaos-engine/addons/design-skills/scripts/human_cue.py",
            "chaos-engine/addons/design-skills/scripts/motion_graphics_gate.py",
            "chaos-engine/addons/design-skills/references/lessons/motion-graphics-gate.md",
            "scripts/ci/pr_scope_check.py",
        ):
            self.assertTrue((ROOT / rel).is_file(), rel)
        bot = (ROOT / "chaos-engine/references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("radio-silence", bot)
        checklist = (
            ROOT / "chaos-engine/references/design-review-video-checklist.md"
        ).read_text(encoding="utf-8")
        self.assertIn("doc-claim", checklist.casefold())



class UncaptionedSpeechL64Test(unittest.TestCase):
    def test_uncovered_fails_covered_passes_missing_fails(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            cues = root / "cues.json"
            asr = root / "asr.json"
            # Carl shape: ASR words in seconds {s,e,t}; human cues {s,e,t,human}.
            asr.write_text(
                json.dumps([
                    {"s": 0.0, "e": 0.20, "t": "hello"},
                    {"s": 0.22, "e": 0.55, "t": "world"},
                ]),
                encoding="utf-8",
            )
            cues.write_text(json.dumps([]), encoding="utf-8")
            code, payload = run_design(
                "uncaptionedspeech", "--cues", str(cues), "--asr", str(asr),
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)
            cues.write_text(
                json.dumps([
                    {"s": 0.0, "e": 0.50, "t": "hello there", "human": True},
                ]),
                encoding="utf-8",
            )
            code, payload = run_design(
                "uncaptionedspeech", "--cues", str(cues), "--asr", str(asr),
            )
            self.assertEqual(0, code, payload)
            code, payload = run_design(
                "uncaptionedspeech",
                "--cues", str(root / "missing.json"),
                "--asr", str(asr),
            )
            self.assertEqual(1, code, payload)
            self.assertIn("missing-input", payload.get("hits", []))
            # Non-hall cue without human:true must NOT cover (H-DOC-CLAIM / Carl fix 4).
            cues.write_text(
                json.dumps([
                    {"s": 0.0, "e": 0.50, "t": "hello world"},
                ]),
                encoding="utf-8",
            )
            code, payload = run_design(
                "uncaptionedspeech", "--cues", str(cues), "--asr", str(asr),
            )
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("hits"), payload)


class PrScopeCheckTest(unittest.TestCase):
    def test_out_of_scope_and_missing(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.pr_scope_check import evaluate

        body = "<!-- chaos-engine:scope chaos-engine/,tests/scripts/ -->\n"
        result = evaluate(body, ["chaos-engine/VERSION", "src/main/java/X.java"])
        self.assertFalse(result["ok"])
        self.assertTrue(any("out-of-scope" in h for h in result["hits"]))
        result_ok = evaluate(body, ["chaos-engine/VERSION", "tests/scripts/t.py"])
        self.assertTrue(result_ok["ok"], result_ok)
        missing = evaluate("no scope here", ["chaos-engine/VERSION"])
        self.assertFalse(missing["ok"])
        self.assertIn("missing-scope", missing["hits"])

    def test_scope_section_bullets(self) -> None:
        sys.path.insert(0, str(ROOT))
        from scripts.ci.pr_scope_check import parse_scope_prefixes

        body = "## Scope\n- chaos-engine/\n- scripts/ci/\n\n## Plan\n- do stuff\n"
        prefixes = parse_scope_prefixes(body)
        self.assertEqual(["chaos-engine/", "scripts/ci/"], prefixes)


class IdentityPreCommitCliTest(unittest.TestCase):
    def test_cli_pre_commit_blocks_foreign(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import commit_identity as ci

        hits = ci.pre_commit_identity_failures(
            name="ChaosEngine",
            email="chaosengine@mail.grokbot.com",
            remote_url="git@github.com:OtherOrg/designer-workspace.git",
        )
        self.assertTrue(hits)
        self.assertIn("designer-workspace", hits[0])
        # Engine remote with ChaosEngine identity must pass.
        ok = ci.pre_commit_identity_failures(
            name="ChaosEngine",
            email="chaosengine@mail.grokbot.com",
            remote_url="https://github.com/ShaftHQ/SHAFT_ENGINE.git",
        )
        self.assertEqual([], ok)




class HumanCueSharedHelperTest(unittest.TestCase):
    def test_json_vs_ass_predicates(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine/addons/design-skills/scripts"))
        import human_cue as hc

        self.assertTrue(hc.is_human_json_cue({"human": True, "t": "hi"}))
        self.assertFalse(hc.is_human_json_cue({"human": False, "t": "hi"}))
        self.assertFalse(hc.is_human_json_cue({"t": "hi"}))
        self.assertTrue(hc.is_human_ass_dialogue(style="Human", name="", text="x"))
        self.assertTrue(hc.is_human_ass_dialogue(style="Default", name="Human", text="x"))
        self.assertTrue(hc.is_human_ass_dialogue(style="Default", name="", text="{\\an8}human=true hi"))
        self.assertFalse(hc.is_human_ass_dialogue(style="Default", name="", text="hello"))
        note = hc.human_predicate_note()
        self.assertIn("L63", note)
        self.assertIn("L64", note)
        # Lesson cards document the split.
        l64 = (ROOT / "chaos-engine/addons/design-skills/references/lessons/uncaptioned-speech.md").read_text(encoding="utf-8")
        self.assertIn("human: true", l64)
        self.assertIn("is_human_json_cue", l64)
        l63 = (ROOT / "chaos-engine/addons/design-skills/references/lessons/karaoke-human-frag.md").read_text(encoding="utf-8")
        self.assertIn("is_human_ass_dialogue", l63)


class HScopeCardTest(unittest.TestCase):
    def test_h_scope_links_carl_surfaces_not_pr_diff_only(self) -> None:
        card = (ROOT / "chaos-engine/references/h-scope.md").read_text(encoding="utf-8")
        for needle in (
            "L41", "title_scope_gate", "sequential-wip", "parallel_stream_conflict",
            "H-PR-SCOPE", "spoken-lock",
        ):
            self.assertIn(needle, card)
        pr = (ROOT / "chaos-engine/references/pr-scope-check.md").read_text(encoding="utf-8")
        self.assertIn("H-PR-SCOPE", pr)
        self.assertNotIn("# PR scope check (H-SCOPE)", pr)
        checklist = (ROOT / "chaos-engine/references/design-review-video-checklist.md").read_text(encoding="utf-8")
        self.assertIn("H-PR-SCOPE", checklist)
        self.assertIn("Share only approved deliverables", checklist)
        self.assertNotIn("Share samples immediately", checklist)
        # L41 remains under spoken-lock wording
        self.assertIn("spoken-lock keeps **scope words** (design L41)", checklist)


class PrScopeWiredTest(unittest.TestCase):
    def test_overlay_and_harness_call_pr_scope(self) -> None:
        opp = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        self.assertIn("def pr_scope_failures", opp)
        self.assertIn("pr_scope_failures(root, changed)", opp)
        harness = (ROOT / "scripts/ci/harness_pr_gate.py").read_text(encoding="utf-8")
        self.assertIn("def pr_scope_gate_failures", harness)
        self.assertIn("pr_scope_gate_failures(", harness)
        sys.path.insert(0, str(ROOT))
        from scripts.ci.pr_scope_check import pr_scope_failures, is_agentic_pr

        body = "<!-- chaos-engine:agentic -->\n<!-- chaos-engine:scope chaos-engine/ -->\n"
        self.assertTrue(is_agentic_pr({"body": body, "labels": []}))
        hits = pr_scope_failures(body, ["src/X.java"], require_agentic=True, pr={"body": body})
        self.assertTrue(hits)
        ok = pr_scope_failures(body, ["chaos-engine/VERSION"], require_agentic=True, pr={"body": body})
        self.assertEqual([], ok)


class MotionGraphicsL65Test(unittest.TestCase):
    def _good(self) -> dict:
        return {
            "kind": "kinetic",
            "transcript_manually_verified": True,
            "reviewer_brief_lists_mg_checks": True,
            "shared_with_owner": False,
            "wip_sample_shared": False,
            "ocr_frames": [
                {"text": f"مرحبا {chr(0x2066)}SHAFT{chr(0x2069)}", "expected": f"مرحبا {chr(0x2066)}SHAFT{chr(0x2069)}"},
            ],
            "static_text_blocks": [{"text": "ok", "duration_s": 2.0, "words": 1}],
            # Word-level ASR timings (seconds); cue start aligned within 100ms.
            "asr_words": [
                {"word": "Hello", "start": 1.00, "end": 1.20},
                {"word": "there", "start": 1.25, "end": 1.40},
                {"word": "friends", "start": 1.45, "end": 1.70},
            ],
            "captions": [
                {
                    "start": 1.10,
                    "lines": ["Hello there friends"],
                    "phrase_split": True,
                    # Builder self-attest ignored for pass/fail.
                    "onset_ms_vs_speech": 50,
                }
            ],
            "diagrams": [{"animation": "draw-on"}],
        }

    def test_pass_and_key_failures(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine/addons/design-skills/scripts"))
        import motion_graphics_gate as mg

        good = self._good()
        self.assertTrue(mg.evaluate(good)["ok"], mg.evaluate(good))
        # bidi unisolated
        bad = dict(good)
        bad["ocr_frames"] = [{"text": "مرحبا SHAFT", "expected": "مرحبا SHAFT"}]
        self.assertFalse(mg.evaluate(bad)["ok"])
        self.assertTrue(any("bidi" in h for h in mg.evaluate(bad)["hits"]))
        # static too long
        bad = dict(good)
        bad["static_text_blocks"] = [{"text": "one two three four five six seven", "duration_s": 4.0}]
        hits = mg.evaluate(bad)["hits"]
        self.assertTrue(any("duration" in h or "words" in h for h in hits), hits)
        # measured onset >150ms (cue early vs ASR)
        bad = dict(good)
        bad["captions"] = [{
            "start": 0.50,
            "onset_ms_vs_speech": 50,
            "lines": ["Hello there friends"],
            "phrase_split": True,
        }]
        hits = mg.evaluate(bad)["hits"]
        self.assertTrue(any("onset>" in h for h in hits), hits)
        # caption line >32
        bad = dict(good)
        bad["captions"] = [{
            "start": 1.10,
            "lines": ["x" * 33],
            "phrase_split": True,
        }]
        self.assertTrue(any("32chars" in h or ">32" in h for h in mg.evaluate(bad)["hits"]))
        # phrase split false
        bad = dict(good)
        bad["captions"] = [{
            "start": 1.10,
            "lines": ["Hello-", "world"],
            "phrase_split": False,
        }]
        self.assertTrue(any("mid-phrase" in h for h in mg.evaluate(bad)["hits"]))
        # diagram pop
        bad = dict(good)
        bad["diagrams"] = [{"animation": "pop"}]
        self.assertTrue(any("pop" in h for h in mg.evaluate(bad)["hits"]))
        # WIP sample shared → FAIL
        bad = dict(good)
        bad["wip_sample_shared"] = True
        self.assertTrue(any("wip-sample-shared" in h for h in mg.evaluate(bad)["hits"]))
        # shared without review/approval → FAIL
        bad = dict(good)
        bad["shared_with_owner"] = True
        hits = mg.evaluate(bad)["hits"]
        self.assertTrue(any("shared-without-review" in h for h in hits), hits)
        self.assertTrue(any("shared-without-owner-approval" in h for h in hits), hits)
        # approved share path
        bad = dict(good)
        bad.update({
            "shared_with_owner": True,
            "reviewed": True,
            "owner_approved": True,
            "coach_acceptance_applicable": True,
            "coach_accepted": True,
        })
        self.assertTrue(mg.evaluate(bad)["ok"], mg.evaluate(bad))
        bad["coach_accepted"] = False
        self.assertTrue(any("shared-without-coach" in h for h in mg.evaluate(bad)["hits"]))
        # missing input
        self.assertIn("missing-input", mg.evaluate(None)["hits"])
        # CLI
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            obs = root / "obs.json"
            obs.write_text(json.dumps(good), encoding="utf-8")
            code, payload = run_design("mggate", str(obs))
            self.assertEqual(0, code, payload)
            obs.write_text(json.dumps({"kind": "kinetic"}), encoding="utf-8")
            code, payload = run_design("mggate", str(obs))
            self.assertEqual(1, code, payload)

    def test_builder_onset_lie_fails_against_asr(self) -> None:
        """Carl r2: obs claims ≤150ms but ASR delta >150ms → FAIL; missing ASR → FAIL."""
        sys.path.insert(0, str(ROOT / "chaos-engine/addons/design-skills/scripts"))
        import motion_graphics_gate as mg

        # Builder swears 50ms; cue at 1.0s vs ASR word at 2.6s → 1600ms measured.
        lie = {
            "kind": "kinetic",
            "transcript_manually_verified": True,
            "reviewer_brief_lists_mg_checks": True,
            "shared_with_owner": False,
            "wip_sample_shared": False,
            "ocr_frames": [{"text": "ok", "expected": "ok"}],
            "static_text_blocks": [{"text": "ok", "duration_s": 1.0, "words": 1}],
            "asr_words": [{"word": "Hello", "start": 2.60, "end": 2.80}],
            "captions": [{
                "start": 1.00,
                "onset_ms_vs_speech": 50,
                "onset_ms": 50,
                "lines": ["Hello"],
                "phrase_split": True,
            }],
            "diagrams": [{"animation": "draw-on"}],
        }
        hits = mg.evaluate(lie)["hits"]
        self.assertFalse(mg.evaluate(lie)["ok"], hits)
        self.assertTrue(any("onset>" in h for h in hits), hits)
        # Lone builder onset field, no ASR → FAIL missing-asr-words
        no_asr = dict(lie)
        del no_asr["asr_words"]
        hits = mg.evaluate(no_asr)["hits"]
        self.assertIn("missing-asr-words", hits)
        self.assertFalse(mg.evaluate(no_asr)["ok"])
        # Empty ASR list → FAIL
        empty = dict(lie)
        empty["asr_words"] = []
        self.assertIn("empty-asr-words", mg.evaluate(empty)["hits"])
        # Lesson documents word-level ASR measurement
        lesson = (
            ROOT / "chaos-engine/addons/design-skills/references/lessons/motion-graphics-gate.md"
        ).read_text(encoding="utf-8")
        self.assertIn("word-level", lesson.lower())
        self.assertIn("asr_words", lesson)
        self.assertIn("insufficient", lesson.lower())


class IdentityOwnerBindingTest(unittest.TestCase):
    def test_ce_owner_identity_binding_and_card_names_remotes(self) -> None:
        sys.path.insert(0, str(ROOT / "chaos-engine"))
        import commit_identity as ci

        card = (ROOT / "chaos-engine/references/commit-identity.md").read_text(encoding="utf-8")
        self.assertIn("allowed_remotes", card)
        self.assertIn("user-guide", card)
        self.assertIn("CE_OWNER_IDENTITY", card)
        self.assertIn("owner-identity", card)
        # Portable card must not embed org remote tokens; origin-only SPEC names them.
        self.assertNotIn("ShaftHQ", card)
        remotes = ci.allowed_remotes()
        self.assertEqual(2, len(remotes))
        self.assertIn("ShaftHQ/SHAFT_ENGINE", remotes)
        self.assertIn("ShaftHQ/shafthq.github.io", remotes)
        guides = (ROOT / "chaos-engine/guides/ce-retro-2026-10-31-6-SPEC.md").read_text(encoding="utf-8")
        self.assertIn("ShaftHQ/SHAFT_ENGINE", guides)
        self.assertIn("ShaftHQ/shafthq.github.io", guides)
        # Portable: owner binding is env/file only — no personal mailbox domains in card
        self.assertNotIn("outlook.com", card.casefold())
        self.assertNotIn("gmail.com", card.casefold())
        self.assertNotIn("yahoo.com", card.casefold())
        # Unset → owner@example.com still ok
        hits = ci.pre_commit_identity_failures(
            name="Owner",
            email="owner@example.com",
            remote_url="https://github.com/example/designer-workspace.git",
            environ={},
        )
        self.assertEqual([], hits)
        # Set → mismatch fails
        hits = ci.pre_commit_identity_failures(
            name="Other",
            email="other@example.com",
            remote_url="https://github.com/example/designer-workspace.git",
            environ={"CE_OWNER_IDENTITY": "Owner <owner@example.com>"},
        )
        self.assertTrue(hits)
        hits = ci.pre_commit_identity_failures(
            name="Owner",
            email="owner@example.com",
            remote_url="https://github.com/example/designer-workspace.git",
            environ={"CE_OWNER_IDENTITY": "Owner <owner@example.com>"},
        )
        self.assertEqual([], hits)



if __name__ == "__main__":
    unittest.main()
