"""Optional ChaosEngine add-ons: discovery, explicit selection, payload, wrappers, design QC."""

from __future__ import annotations

import importlib.util
import json
import os
import re
import shutil
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
DESIGN = CE / "addons/design-skills"
QC = DESIGN / "scripts/design_qc.py"
NAMES = {"design-skills", "shaft-engine-users", "shaft-core-developers"}
CARDS = (
    "design-brief-storyboard", "brand-system", "web-visual-direction", "web-interface-audit",
    "typography-layout", "color-contrast", "image-graphics", "motion-principles",
    "html-motion-graphics", "technical-animation", "screen-capture", "edit-assembly",
    "cutting-pacing", "color-grading", "audio-mix-loudness", "noise-removal", "voice-over-tts",
    "captions-subtitles", "video-restoration-upscaling", "delivery-qc", "explainer-arc",
)


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


catalog = _load("ce_addon_catalog_test", CE / "addon_catalog.py")
install = _load("ce_install_addons_test", CE / "install.py")


class CatalogTests(unittest.TestCase):
    def setUp(self):
        self.found = catalog.discover(CE)

    def test_the_three_addons_are_discovered(self):
        self.assertEqual(NAMES, set(self.found))
        self.assertEqual(["shaft-engine-users"], self.found["shaft-core-developers"]["manifest"]["requires"])
        self.assertEqual("repository", self.found["shaft-core-developers"]["manifest"]["distribution"])

    def test_nothing_is_selected_by_default(self):
        self.assertEqual([], catalog.resolve(self.found, set(), set(), set()))

    def test_requires_are_added_and_dependents_block_removal(self):
        self.assertEqual(
            ["shaft-core-developers", "shaft-engine-users"],
            catalog.resolve(self.found, {"shaft-core-developers"}, set(), set()),
        )
        with self.assertRaisesRegex(ValueError, "required by shaft-core-developers"):
            catalog.resolve(self.found, {"shaft-core-developers"}, {"shaft-engine-users"}, set())

    def test_persisted_choice_survives_and_explicit_removal_wins(self):
        self.assertEqual(["design-skills"], catalog.resolve(self.found, set(), set(), {"design-skills"}))
        self.assertEqual([], catalog.resolve(self.found, set(), {"design-skills"}, {"design-skills"}))

    def test_unknown_names_fail_closed_and_powershell_spelling_is_accepted(self):
        with self.assertRaisesRegex(ValueError, "unknown ChaosEngine add-on: nope"):
            catalog.resolve(self.found, {"nope"}, set(), set())
        self.assertEqual(["design-skills"], catalog.resolve(self.found, {"designskills"}, set(), set()))

    def test_flags_and_environment_are_split(self):
        requested, removed, rest = catalog.split_flags(
            ["--with-design-skills", "--without-shaft-engine-users", "--without-memory", "--x"]
        )
        self.assertEqual({"design-skills"}, requested)
        self.assertEqual({"shaft-engine-users"}, removed)
        self.assertEqual(["--without-memory", "--x"], rest)
        self.assertEqual({"a-b", "c-d"}, catalog.environment_names({"CHAOS_ENGINE_ADDONS": "a-b, c-d,"}))

    def test_invalid_manifests_and_cycles_fail_closed(self):
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "chaos-engine"
            for name, requires in (("one-a", ["two-b"]), ("two-b", ["one-a"])):
                folder = source / "addons" / name
                folder.mkdir(parents=True)
                (folder / "addon.json").write_text(json.dumps({
                    "schemaVersion": 1, "kind": "addon", "name": name, "requires": requires,
                    "description": "d", "useWhen": "u",
                }), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "cycle"):
                catalog.discover(source)
            (source / "addons/two-b/addon.json").write_text('{"kind": "addon"}', encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "invalid"):
                catalog.discover(source)


class PayloadTests(unittest.TestCase):
    def relatives(self, distribution: str, addons=()) -> set[str]:
        return {
            install.payload_relative(CE, path).as_posix()
            for path in install.source_files(CE, distribution, addons)
        }

    def test_default_payload_has_no_addon_files(self):
        relatives = self.relatives("portable")
        self.assertFalse(any(item.startswith("addons/") for item in relatives))
        self.assertIn("references/addons.md", relatives)

    def test_selected_addons_land_under_addons(self):
        relatives = self.relatives("portable", ("design-skills", "shaft-engine-users"))
        self.assertIn("addons/design-skills/SKILL.md", relatives)
        self.assertIn("addons/design-skills/scripts/design_qc.py", relatives)
        self.assertIn("addons/shaft-engine-users/addon.json", relatives)
        self.assertIn("addons/shaft-engine-users/shaft-developer/SKILL.md", relatives)
        self.assertFalse(any(item.startswith("addons/shaft-core-developers") for item in relatives))

    def test_plan_ignores_project_files_and_honours_the_environment(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "pom.xml").write_text(
                "<project><dependencies><dependency><artifactId>shaft-engine</artifactId>"
                "</dependency></dependencies></project>",
                encoding="utf-8",
            )
            self.assertEqual(("portable", ()), install.plan_install(project, CE, environ={}))
            self.assertEqual(
                ("portable", ("design-skills",)),
                install.plan_install(project, CE, environ={"CHAOS_ENGINE_ADDONS": "design-skills"}),
            )


class AddOnOnlyReinstallTests(unittest.TestCase):
    """#6494: adding an add-on re-provisioned every account tool (~3 min vs 1:13)."""

    COMMIT = "1" * 40
    TOOLS = ("uv", "python", "node", "java", "mempalace", "graphify", "memory", "context7")

    def test_adding_an_addon_reuses_account_tools_and_a_plain_rerun_still_heals(self):
        provisioned = []
        load_controller = install.load_dependency_controller

        def receipt_writer(project, _specification, **_kwargs):
            provisioned.append(project)
            receipt = {
                "schemaVersion": 2,
                "scope": "user",
                "components": {name: {"status": "healthy", "action": "reused"} for name in self.TOOLS},
                "commands": {
                    name: str(Path(sys.executable).resolve())
                    for name in ("python3", "node", "memory-mcp", "mempalace-mcp")
                },
            }
            project.joinpath(".chaos-engine-dependencies.json").write_text(json.dumps(receipt), encoding="utf-8")
            return receipt

        def controller(root):
            real = vars(load_controller(root))
            return SimpleNamespace(**{**real, "install_account_dependencies": receipt_writer})

        with tempfile.TemporaryDirectory() as temporary, \
                mock.patch.object(install, "load_dependency_controller", side_effect=controller), \
                mock.patch.object(install, "initialize_account_project_palace"):
            project = Path(temporary) / "consumer"
            project.mkdir()
            install.install_with_dependencies(project, CE, self.COMMIT)
            self.assertEqual(1, len(provisioned))

            install.install_with_dependencies(project, CE, self.COMMIT, addons=("design-skills",))
            self.assertEqual(1, len(provisioned), "an add-on change must not re-provision account tools")
            self.assertTrue((project / ".chaos-engine/addons/design-skills/SKILL.md").is_file())

            install.install_with_dependencies(project, CE, self.COMMIT, addons=("design-skills",))
            self.assertEqual(2, len(provisioned), "a plain rerun keeps healing account tools")


class WrapperContractTests(unittest.TestCase):
    def test_wrappers_forward_generic_flags_without_naming_a_product(self):
        shell = (CE / "install.sh").read_text(encoding="utf-8")
        powershell = (CE / "install.ps1").read_text(encoding="utf-8")
        self.assertIn("--with-?*|--without-?*)", shell)
        self.assertIn("ValueFromRemainingArguments", powershell)
        self.assertIn("ConvertTo-ChaosEngineAddOnFlags", powershell)
        for text in (shell, powershell):
            self.assertNotIn("shaft", text.casefold())

    def test_install_md_documents_every_addon_flag(self):
        text = (CE / "INSTALL.md").read_text(encoding="utf-8")
        for name in NAMES:
            pascal = "".join(part.capitalize() for part in name.split("-"))
            self.assertIn(f"--with-{name}", text)
            self.assertIn(f"-With{pascal}", text)
            self.assertEqual(name.replace("-", ""), pascal.lower())

    @unittest.skipUnless(shutil.which("pwsh"), "pwsh not installed")
    def test_powershell_switches_become_addon_flags(self):
        script = (
            "$t=$null;$e=$null;$a=[System.Management.Automation.Language.Parser]::ParseFile('"
            + str(CE / "install.ps1")
            + "',[ref]$t,[ref]$e);$f=$a.FindAll({$args[0] -is [System.Management.Automation.Language."
            "FunctionDefinitionAst] -and $args[0].Name -eq 'ConvertTo-ChaosEngineAddOnFlags'},$true)[0];"
            "Invoke-Expression $f.Extent.Text;ConvertTo-ChaosEngineAddOnFlags @('-WithDesignSkills','-WithoutX1')"
        )
        pwsh = shutil.which("pwsh")
        result = subprocess.run(  # nosec B603 - resolved pwsh path, fixed argv.
            [pwsh, "-NoLogo", "-NoProfile", "-Command", script], capture_output=True, text=True, check=True
        )
        self.assertEqual(["--with-designskills", "--without-x1"], result.stdout.split())


class DesignContentTests(unittest.TestCase):
    def test_router_links_every_card_within_budget(self):
        router = (DESIGN / "SKILL.md").read_text(encoding="utf-8")
        self.assertLessEqual(len(router.encode("utf-8")), 4096)
        self.assertEqual(21, len(CARDS))
        for name in CARDS:
            card = DESIGN / "references" / f"{name}.md"
            self.assertIn(f"references/{name}.md", router)
            text = card.read_text(encoding="utf-8")
            self.assertLessEqual(len(text.encode("utf-8")), 6144, name)
            self.assertTrue(text.startswith(f"---\nname: {name}\ndescription: Use when"), name)
            for section in ("## Use when", "## Verify", "## Sources"):
                self.assertIn(section, text, name)

    def test_relative_links_resolve(self):
        for path in DESIGN.rglob("*.md"):
            for target in re.findall(r"\]\(([^)#]+)(?:#[^)]*)?\)", path.read_text(encoding="utf-8")):
                if "://" in target:
                    continue
                self.assertTrue((path.parent / target).exists(), f"{path}: {target}")

    def test_design_content_is_project_neutral(self):
        for path in DESIGN.rglob("*"):
            if path.is_file() and "__pycache__" not in path.parts:
                self.assertIsNone(re.search(r"shaft|mohab", path.read_text(encoding="utf-8"), re.I), path)


class DesignQcTests(unittest.TestCase):
    qc = _load("ce_design_qc_test", QC)

    def run_qc(self, *args: str) -> tuple[int, dict]:
        result = subprocess.run(  # nosec B603 - current interpreter, fixed script.
            [sys.executable, str(QC), *args], capture_output=True, text=True, check=False
        )
        return result.returncode, json.loads(result.stdout.strip().splitlines()[-1])

    def test_contrast_ratio_matches_wcag_examples(self):
        self.assertAlmostEqual(21.0, self.qc.contrast_ratio("#000", "#fff"), places=2)
        code, payload = self.run_qc("contrast", "--pair", "#ffffff:#767676")
        self.assertEqual((0, "pass"), (code, payload["status"]))
        code, payload = self.run_qc("contrast", "--pair", "#999999:#ffffff")
        self.assertEqual((1, "fail"), (code, payload["status"]))

    def test_captions_rules(self):
        with tempfile.TemporaryDirectory() as temporary:
            good = Path(temporary) / "good.srt"
            good.write_text("1\n00:00:01,000 --> 00:00:03,000\nOne line installs it.\n\n"
                            "2\n00:00:03,500 --> 00:00:05,000\nThen run doctor.\n", encoding="utf-8")
            self.assertEqual(0, self.run_qc("captions", str(good))[0])
            bad = Path(temporary) / "bad.vtt"
            bad.write_text("WEBVTT\n\n00:00:01.000 --> 00:00:01.400\n" + "x" * 50 + "\n", encoding="utf-8")
            code, payload = self.run_qc("captions", str(bad))
            self.assertEqual(1, code)
            self.assertTrue(any("chars" in problem for problem in payload["problems"]))

    def test_word_error_rate_and_brief(self):
        self.assertEqual(0.0, self.qc.word_error_rate(["a", "b"], ["a", "b"]))
        self.assertAlmostEqual(0.5, self.qc.word_error_rate(["a", "b"], ["a", "c"]))
        with tempfile.TemporaryDirectory() as temporary:
            board = Path(temporary) / "storyboard.json"
            board.write_text(json.dumps({"targetDuration": 10, "scenes": [
                {"id": "s1", "duration": 10, "visual": "v", "vo": "x", "asset": "a", "source": "s",
                 "claims": [{"text": "fast"}]}]}), encoding="utf-8")
            code, payload = self.run_qc("brief", str(board))
            self.assertEqual(1, code)
            self.assertIn("claim without evidence", payload["problems"][0])

    def test_unslop_flags_banned_patterns_unless_allowed(self):
        with tempfile.TemporaryDirectory() as temporary:
            css = Path(temporary) / "a.css"
            css.write_text(".t{background-clip:text}\n.g{backdrop-filter: blur(8px)} /* design-qc: allow */\n",
                           encoding="utf-8")
            code, payload = self.run_qc("unslop", temporary)
            self.assertEqual(1, code)
            self.assertEqual(1, len(payload["hits"]))

    def test_missing_tools_report_skipped_never_pass(self):
        original = self.qc.shutil.which
        try:
            self.qc.shutil.which = lambda _name: None
            self.assertEqual(4, self.qc.main(["loudness", "missing.wav"]))
            self.assertEqual(4, self.qc.main(["vmaf", "a.mp4", "b.mp4"]))
        finally:
            self.qc.shutil.which = original

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_delivery_and_levels_on_a_generated_clip(self):
        with tempfile.TemporaryDirectory() as temporary:
            clip = Path(temporary) / "clip.mp4"
            subprocess.run([  # nosec B603 - resolved ffmpeg path, fixed argv.
                shutil.which("ffmpeg"), "-hide_banner", "-loglevel", "error", "-f", "lavfi", "-i",
                "color=c=gray:s=1920x1080:r=30:d=1", "-f", "lavfi", "-i", "sine=f=440:d=1:sample_rate=48000",
                "-vf", "format=yuv420p", "-c:v", "libx264", "-profile:v", "high",
                "-x264-params", "colorprim=bt709:transfer=bt709:colormatrix=bt709", "-color_range", "tv",
                "-c:a", "aac", "-shortest", str(clip),
            ], check=True)
            self.assertEqual(0, self.run_qc("delivery", str(clip), "--preset", "1080p", "--fps", "30")[0])
            self.assertEqual(0, self.run_qc("levels", str(clip))[0])
            self.assertEqual(0, self.run_qc("colortags", str(clip))[0])
            self.assertEqual(0, self.run_qc("flash", str(clip))[0])


class DesignLessonTests(unittest.TestCase):
    """Install-video lessons (#6502): new gates and rules."""

    qc = DesignQcTests.qc

    def run_qc(self, *args: str) -> tuple[int, dict]:
        return DesignQcTests.run_qc(self, *args)

    def write(self, folder: str, name: str, payload: object) -> str:
        path = Path(folder) / name
        path.write_text(payload if isinstance(payload, str) else json.dumps(payload), encoding="utf-8")
        return str(path)

    def test_vooverlap_masks_and_edit_list_gap(self):
        frames = self.qc.mask_overlaps([(0, [True] * 10), (8, [True] * 5)], 0.02)
        self.assertEqual([{"start": 0.16, "end": 0.2}], frames)
        self.assertEqual([], self.qc.mask_overlaps([(0, [True] * 5 + [False] * 5), (5, [True] * 5)], 0.02))
        with tempfile.TemporaryDirectory() as temporary:
            def edl(second_at: float) -> str:
                return self.write(temporary, "edl.json", {"audio": [
                    {"src": "a.wav", "at": 0, "dur": 2.0, "vo": True},
                    {"src": "b.wav", "at": second_at, "dur": 1.0, "vo": True},
                    {"src": "bed.wav", "at": 0, "dur": 9.0, "duck": True}]})
            code, payload = self.run_qc("vooverlap", edl(2.1), "--edl-only")
            self.assertEqual((1, "fail"), (code, payload["status"]))
            self.assertEqual("a.wav", payload["edl_overlaps"][0]["a"])
            self.assertEqual(0, self.run_qc("vooverlap", edl(2.3), "--edl-only")[0])

    def test_vowords_finds_dropped_and_repeated_words(self):
        with tempfile.TemporaryDirectory() as temporary:
            script = self.write(temporary, "script.json", {"lines": [
                {"id": "e1", "text": "Cards for web, image, motion and video work."},
                {"id": "e2", "text": "Open the project folder."}]})
            heard = self.write(temporary, "heard.json", {
                "e1": "cards for web image and video work", "e2": {"transcript": "open the project folder folder"}})
            code, payload = self.run_qc("vowords", script, heard)
            self.assertEqual(1, code)
            lines = {line["id"]: line for line in payload["lines"]}
            self.assertEqual(["motion"], lines["e1"]["dropped"])
            self.assertEqual(["folder"], lines["e2"]["repeated"])
            clean = self.write(temporary, "clean.json", {
                "e1": "cards for web image motion and video work", "e2": "open the project folder"})
            self.assertEqual(0, self.run_qc("vowords", script, clean)[0])

    def test_ttslint_flags_cli_syntax_and_repeated_phonemes(self):
        with tempfile.TemporaryDirectory() as temporary:
            cli = self.write(temporary, "cli.txt", "Add --with-design-skills to the command.\n")
            code, payload = self.run_qc("ttslint", cli)
            self.assertEqual(1, code)
            self.assertTrue(any("cli" in hit for hit in payload["hits"]))
            script = self.write(temporary, "script.json", {"lines": [{"id": "h2", "text": "A brand new user."}]})
            phonemes = self.write(temporary, "ipa.json", {"h2": "ɐ bɹˈænd nuː jˈuːzɚ"})
            code, payload = self.run_qc("ttslint", script, "--phonemes", phonemes)
            self.assertEqual(1, code)
            self.assertTrue(any("new user" in hit for hit in payload["hits"]))
            self.assertEqual(0, self.run_qc("ttslint", script, "--phonemes", phonemes, "--allow", "new user")[0])
            spoken = self.write(temporary, "spoken.json", {"lines": [
                {"id": "l2", "text": "Files live under .tool/addons.", "say": "Files live under dot tool, add-ons."}]})
            self.assertEqual(0, self.run_qc("ttslint", spoken)[0])

    def test_tts_gates_on_folded_wer_and_reports_raw(self):
        with tempfile.TemporaryDirectory() as temporary:
            script = self.write(temporary, "s.txt", "Codex installs the core")
            heard = self.write(temporary, "h.txt", "codecs installs the core")
            self.assertEqual(1, self.run_qc("tts", script, heard)[0])
            code, payload = self.run_qc("tts", script, heard, "--fold", "codecs=codex")
            self.assertEqual((0, 0.0, 0.25), (code, payload["wer"], payload["raw_wer"]))

    def test_static_spans_ignore_change_and_honour_allow(self):
        still = [bytes(576)] * 50
        spans, longest = self.qc.static_spans(still, fps=10, thresh=3, max_s=3, allow=[])
        self.assertEqual(1, len(spans))
        self.assertGreater(longest, 3)
        moving = [bytes([(i // 10) * 20]) * 576 for i in range(50)]
        self.assertEqual([], self.qc.static_spans(moving, fps=10, thresh=3, max_s=3, allow=[])[0])
        self.assertEqual([], self.qc.static_spans(still, fps=10, thresh=3, max_s=3, allow=[(0, 5)])[0])

    def test_static_spans_measure_drift_from_the_window_start(self):  # #6724
        drift = [bytes([i // 4]) * 576 for i in range(50)]  # 0.25 level per frame, 1.25 over any 0.5 s lag
        self.assertEqual([], self.qc.static_spans(drift, fps=10, thresh=3, max_s=3, allow=[])[0])
        flat = [*drift[:12], *[drift[12]] * 38]
        spans, _ = self.qc.static_spans(flat, fps=10, thresh=3, max_s=3, allow=[])
        self.assertEqual([{"start": 1.2, "end": 5.0, "dur": 3.8}], spans)

    def test_levels_names_offending_frames(self):
        log = ("frame:0 pts:0 pts_time:0\nlavfi.signalstats.YMIN=20\nlavfi.signalstats.YMAX=200\n"
               "frame:1 pts:1 pts_time:268.97\nlavfi.signalstats.YMIN=12\nlavfi.signalstats.YMAX=226\n")
        self.assertEqual([{"t": 268.97, "ymin": 12.0, "ymax": 226.0}], self.qc.level_violations(log))

    def test_idle_gate_uses_load_per_cpu(self):
        with mock.patch.object(self.qc.os, "getloadavg", return_value=(7.5, 0, 0)), \
                mock.patch.object(self.qc.os, "cpu_count", return_value=8):
            self.assertEqual(1, self.qc.main(["idle"]))
        with mock.patch.object(self.qc.os, "getloadavg", return_value=(1.0, 0, 0)), \
                mock.patch.object(self.qc.os, "cpu_count", return_value=8):
            self.assertEqual(0, self.qc.main(["idle"]))

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_static_gate_on_a_generated_still(self):
        with tempfile.TemporaryDirectory() as temporary:
            clip = Path(temporary) / "still.mp4"
            subprocess.run([  # nosec B603 - resolved ffmpeg path, fixed argv.
                shutil.which("ffmpeg"), "-hide_banner", "-loglevel", "error", "-f", "lavfi", "-i",
                "color=c=gray:s=640x360:r=10:d=5", "-pix_fmt", "yuv420p", str(clip),
            ], check=True)
            self.assertEqual(1, self.run_qc("static", str(clip))[0])
            self.assertEqual(0, self.run_qc("static", str(clip), "--allow", "0-6")[0])

    def test_cards_carry_the_session_rules(self):
        def card(name: str) -> str:
            return (DESIGN / "references" / f"{name}.md").read_text(encoding="utf-8")
        router = (DESIGN / "SKILL.md").read_text(encoding="utf-8")
        self.assertIn("references/pipelines/runbook.md", router)
        runbook = (DESIGN / "references/pipelines/runbook.md").read_text(encoding="utf-8")
        for stage in ("Detached build", "Fast gates", "Idle full QC", "Review", "Deliver", "--only", "STATUS.md"):
            self.assertIn(stage, runbook)
        self.assertIn("vooverlap", card("edit-assembly"))
        self.assertIn("static", card("cutting-pacing"))
        for rule in ("ttslint", "vowords", "0.85", "--fold"):
            self.assertIn(rule, card("voice-over-tts"))
        self.assertIn("40 px", card("typography-layout"))
        self.assertIn("CRF", card("color-grading"))
        self.assertIn("review", card("delivery-qc").lower())


class DesignRound3Tests(unittest.TestCase):
    """Install-video round-3 lessons and explainer research (#6648)."""

    qc = DesignQcTests.qc

    def run_qc(self, *args: str) -> tuple[int, dict]:
        return DesignQcTests.run_qc(self, *args)

    def write(self, folder: str, name: str, payload: object) -> str:
        return DesignLessonTests.write(self, folder, name, payload)

    @staticmethod
    def ffmpeg(*args: str) -> None:
        subprocess.run([shutil.which("ffmpeg"), "-hide_banner", "-loglevel", "error", "-y", *args],  # nosec B603
                       check=True)

    def test_flat_runs_skip_head_and_tail(self):  # #6649
        stds = [0.0] * 30 + [20.0] * 30 + [0.2] * 3 + [20.0] * 30 + [0.0] * 30
        self.assertEqual([{"start": 2.0, "frames": 3}], self.qc.flat_runs(stds, fps=30, edge=1.0, limit=1.0))
        self.assertEqual([], self.qc.flat_runs([0.0] * 30 + [20.0] * 30 + [0.0] * 30, fps=30, edge=1.0, limit=1.0))

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_flatframes_gate_on_a_black_dip(self):  # #6649
        with tempfile.TemporaryDirectory() as temporary:
            clean, dipped = Path(temporary) / "clean.mp4", Path(temporary) / "dip.mp4"
            self.ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=4", "-pix_fmt", "yuv420p", str(clean))
            self.ffmpeg("-f", "lavfi", "-i", "testsrc2=s=320x180:r=30:d=4", "-vf",
                        "drawbox=c=black:t=fill:enable='between(n,60,62)'", "-pix_fmt", "yuv420p", str(dipped))
            self.assertEqual(0, self.run_qc("flatframes", str(clean))[0])
            code, payload = self.run_qc("flatframes", str(dipped))
            self.assertEqual((1, 2.0), (code, payload["runs"][0]["start"]))

    def test_contenthold_limits_plain_and_stepped_holds(self):  # #6650
        with tempfile.TemporaryDirectory() as temporary:
            def edl(*holds: dict) -> str:
                segments = [{"src": [0, 2], "speed": 1.0}, *holds]
                return self.write(temporary, "edl.json", {"clips": [{"id": "k1", "segments": segments}]})
            code, payload = self.run_qc("contenthold", edl({"hold": 4.0, "at": 2}))
            self.assertEqual((1, "k1", 2.0), (code, payload["over_limit"][0]["clip"], payload["over_limit"][0]["at"]))
            self.assertEqual(0, self.run_qc("contenthold", edl({"hold": 4.0, "at": 2, "marks": [1, 2]}))[0])
            self.assertEqual(0, self.run_qc("contenthold", edl({"hold": 5.0, "at": 2, "scroll": "Host"}))[0])
            self.assertEqual(1, self.run_qc("contenthold", edl({"hold": 6.0, "at": 2, "marks": [1, 2]}))[0])

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_static_crop_ignores_moving_overlays(self):  # #6650
        with tempfile.TemporaryDirectory() as temporary:
            clip = Path(temporary) / "overlay.mp4"
            self.ffmpeg("-f", "lavfi", "-i", "color=c=gray:s=640x360:r=10:d=5", "-f", "lavfi", "-i",
                        "color=c=white:s=40x40:r=10:d=5", "-filter_complex", "[0][1]overlay=x='mod(t*80,600)':y=300",
                        "-pix_fmt", "yuv420p", str(clip))
            self.assertEqual(0, self.run_qc("static", str(clip))[0])
            self.assertEqual(1, self.run_qc("static", str(clip), "--crop", "640:280:0:0")[0])

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_static_compares_against_the_window_start(self):  # #6724
        with tempfile.TemporaryDirectory() as temporary:
            frame = Path(temporary) / "frame.png"
            self.ffmpeg("-f", "lavfi", "-i", "color=c=0x202830:s=1280x720:r=25:d=1,"
                        "drawbox=x=380:y=300:w=520:h=40:color=0x607080:t=fill,"
                        "drawbox=x=420:y=370:w=440:h=40:color=0x607080:t=fill", "-frames:v", "1", str(frame))
            push, still = Path(temporary) / "push.mp4", Path(temporary) / "still.mp4"
            self.ffmpeg("-i", str(frame), "-vf", "scale=5120:2880,zoompan=z='1+0.0045*on/25':"
                        "x='iw/2-(iw/zoom/2)':y='ih/2-(ih/zoom/2)':d=100:s=1280x720:fps=25",
                        "-t", "4", "-pix_fmt", "yuv420p", str(push))
            self.ffmpeg("-loop", "1", "-framerate", "25", "-i", str(frame), "-t", "4", "-pix_fmt", "yuv420p", str(still))
            self.assertEqual(0, self.run_qc("static", str(push), "--max", "3")[0])
            self.assertEqual(1, self.run_qc("static", str(still), "--max", "3")[0])

    def test_claims_need_must_and_reject_must_not(self):  # #6651
        with tempfile.TemporaryDirectory() as temporary:
            listing = "design-skills  installed\nshaft-engine-users  installed"
            claims = self.write(temporary, "claims.json", {"claims": [
                {"id": "l1", "t": 3.2, "must": ["^design-skills\\s+installed"], "screen": listing},
                {"id": "r2", "t": 9.1, "must": ["^design-skills"], "mustNot": ["^shaft-engine-users\\s+installed"],
                 "screen": listing},
                {"id": "c5", "t": 12.0, "must": ["healthy\\s+15/15"], "screen": "healthy 14/15"}]})
            code, payload = self.run_qc("claims", claims)
            self.assertEqual(1, code)
            results = {row["id"]: row for row in payload["claims"]}
            self.assertTrue(results["l1"]["ok"])
            self.assertEqual(["^shaft-engine-users\\s+installed"], results["r2"]["unexpected"])
            self.assertEqual(["healthy\\s+15/15"], results["c5"]["missing"])

    def test_edge_hits_find_text_in_the_outer_strip(self):  # #6652
        row = [20] * 100
        right = row[:96] + [230, 230, 230, 230]
        self.assertEqual(["right"], self.qc.edge_hits([right] * 20, strip=4, delta=70, min_pixels=12))
        self.assertEqual([], self.qc.edge_hits([row[:40] + [230] * 20 + row[60:]] * 20, strip=4, delta=70,
                                               min_pixels=12))

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_edgeclip_gate_on_a_vertical_clip(self):  # #6652
        with tempfile.TemporaryDirectory() as temporary:
            clipped, centered = Path(temporary) / "clipped.mp4", Path(temporary) / "centered.mp4"
            for path, x in ((clipped, 1060), (centered, 500)):
                self.ffmpeg("-f", "lavfi", "-i", "color=c=0x101418:s=1080x1920:r=10:d=1", "-vf",
                            f"drawbox=x={x}:y=800:w=40:h=200:c=white:t=fill", "-pix_fmt", "yuv420p", str(path))
            code, payload = self.run_qc("edgeclip", str(clipped))
            self.assertEqual((1, "right"), (code, payload["clipped"][0]["side"]))
            self.assertEqual(0, self.run_qc("edgeclip", str(centered))[0])

    def test_staletext_finds_shorter_rewrites_without_erase(self):  # #6653
        hits = self.qc.stale_rewrites([(0.5, "Installing tools\rDone\n")], 80)
        self.assertEqual("alling tools", hits[0]["stale_tail"])
        self.assertEqual([], self.qc.stale_rewrites([(0.5, "Installing tools\r\x1b[KDone\n")], 80))
        self.assertEqual([], self.qc.stale_rewrites([(0.1, "50%"), (0.2, "\r100%\n")], 80))
        with tempfile.TemporaryDirectory() as temporary:
            cast = Path(temporary) / "take.cast"
            cast.write_text(json.dumps({"version": 2, "width": 80, "height": 24}) + "\n"
                            + json.dumps([0.5, "o", "Installing tools\r\x1b[1BDone"]) + "\n"
                            + json.dumps([0.9, "o", "\x1b[1A\rOK\n"]) + "\n", encoding="utf-8")
            code, payload = self.run_qc("staletext", str(cast))
            self.assertEqual((1, "stalling tools"), (code, payload["hits"][0]["stale_tail"]))

    def test_psparse_static_flags_backslash_continuations(self):  # #6654
        with tempfile.TemporaryDirectory() as temporary:
            bad = self.write(temporary, "bad.ps1", "PS> irm https://example.test/install.ps1 | iex \\\n  -Verbose\n")
            good = self.write(temporary, "good.ps1", "PS> $s = irm https://example.test/install.ps1\n\nPS> iex $s\n")
            code, payload = self.run_qc("psparse", bad, "--static")
            self.assertEqual((1, True), (code, payload["commands"][0]["backslash_continuation"]))
            self.assertEqual(0, self.run_qc("psparse", good, "--static")[0])

    @unittest.skipUnless(shutil.which("pwsh"), "pwsh not installed")
    def test_psparse_parses_with_pwsh(self):  # #6654
        with tempfile.TemporaryDirectory() as temporary:
            valid = self.write(temporary, "ok.ps1", "PS> Get-ChildItem | Select -First 1\n")
            broken = self.write(temporary, "no.ps1", "PS> Get-ChildItem | | Select\n")
            self.assertEqual(0, self.run_qc("psparse", valid)[0])
            self.assertEqual(1, self.run_qc("psparse", broken)[0])

    def board(self, folder: str, **change: object) -> str:
        scenes = [
            {"id": "s1", "beat": "hook", "start": 0, "duration": 5, "vo": "Your build is red.",
             "onScreenText": "Build red"},
            {"id": "s2", "beat": "problem", "duration": 20, "vo": "Flaky tests hide real bugs."},
            {"id": "s3", "beat": "solution", "duration": 40, "vo": "One engine with built-in waits."},
            {"id": "s4", "beat": "proof", "duration": 25, "vo": "Open source since 2018.",
             "claims": [{"text": "since 2018", "evidence": "repo created_at"}]},
            {"id": "s5", "beat": "cta", "duration": 15, "vo": "Generate a project today.",
             "onScreenText": "Generate a project"}]
        data = {"audience": "exec", "scenes": scenes}
        for key, value in change.items():
            scene_id, field = key.split("__")
            next(s for s in scenes if s["id"] == scene_id)[field] = value
        return self.write(folder, "board.json", data)

    def test_arc_checks_beats_hook_cta_proof_and_band(self):  # #6655
        with tempfile.TemporaryDirectory() as temporary:
            self.assertEqual(0, self.run_qc("arc", self.board(temporary))[0])
            for change, needle in (({"s1__duration": 8}, "hook"), ({"s5__beat": "solution"}, "cta"),
                                   ({"s4__claims": [{"text": "x"}]}, "evidence"),
                                   ({"s3__duration": 140}, "duration"), ({"s2__beat": "proof"}, "order")):
                code, payload = self.run_qc("arc", self.board(temporary, **change))
                self.assertEqual(1, code, change)
                self.assertTrue(any(needle in problem for problem in payload["problems"]), (change, payload))
            self.assertEqual(0, self.run_qc("arc", self.board(temporary, s3__duration=130), "--audience",
                                            "technical", "--min", "100")[0])

    def test_describe_requires_on_screen_text_spoken_or_described(self):  # #6656
        with tempfile.TemporaryDirectory() as temporary:
            self.assertEqual(0, self.run_qc("describe", self.board(temporary))[0])
            unspoken = self.board(temporary, s3__onScreenText="Allure evidence included")
            code, payload = self.run_qc("describe", unspoken)
            self.assertEqual((1, "s3"), (code, payload["scenes"][0]["id"]))
            described = self.board(temporary, s3__onScreenText="Allure evidence included",
                                   s3__description="A report panel shows Allure evidence.")
            self.assertEqual(0, self.run_qc("describe", described)[0])

    def test_cards_carry_the_round3_and_explainer_rules(self):  # #6648
        def card(name: str) -> str:
            return (DESIGN / "references" / f"{name}.md").read_text(encoding="utf-8")
        self.assertIn("flatframes", card("edit-assembly"))
        self.assertIn("frame 0", card("edit-assembly"))
        self.assertIn("contenthold", card("cutting-pacing"))
        self.assertIn("claims", card("design-brief-storyboard"))
        self.assertIn("reflow", card("screen-capture"))
        self.assertIn("staletext", card("screen-capture"))
        self.assertIn("psparse", card("typography-layout"))
        self.assertIn("edgeclip", card("delivery-qc"))
        self.assertIn("muted", card("delivery-qc"))
        self.assertIn("describe", card("captions-subtitles"))
        self.assertIn("1.2.5", card("captions-subtitles"))
        self.assertIn("3 frames", card("html-motion-graphics"))
        self.assertIn("zero", card("technical-animation"))
        arc = card("explainer-arc")
        for rule in ("hook", "proof", "cta", "design_qc.py arc", "engagement"):
            self.assertIn(rule, arc)

    def test_voice_card_separates_synthesis_and_asr(self):  # #6663
        voice = (DESIGN / "references/voice-over-tts.md").read_text(encoding="utf-8")
        for rule in ("separate processes", "temperature 0", "harness fault", "G2P"):
            self.assertIn(rule, voice)
        runbook = (DESIGN / "references/pipelines/runbook.md").read_text(encoding="utf-8")
        self.assertIn("two processes", runbook)


class DesignRound4Tests(unittest.TestCase):
    """Feature-video retrospective lessons (#6684)."""

    qc = DesignQcTests.qc

    def run_qc(self, *args: str) -> tuple[int, dict]:
        return DesignQcTests.run_qc(self, *args)

    def write(self, folder: str, name: str, payload: object) -> str:
        return DesignLessonTests.write(self, folder, name, payload)

    @staticmethod
    def ledger(status: str = "open", rounds: int = 1, evidence: str = "frames r1.png: overlap at y 1290") -> dict:
        finding = {"id": "f1", "t": "0:15", "severity": "moderate", "claim": "caption covers chips",
                   "verdict": "true", "evidence": evidence, "status": status}
        if status == "fixed":
            finding["fix_evidence"] = "frames r2.png: 140 px gap"
        others = [{"round": n, "output": "a", "findings": []} for n in range(2, rounds + 1)]
        return {"rounds": [{"round": 1, "output": "a", "findings": [finding]}, *others]}

    def test_findings_gate_on_open_verified_findings(self):  # #6685
        with tempfile.TemporaryDirectory() as folder:
            code, out = self.run_qc("findings", self.write(folder, "l.json", self.ledger()))
            self.assertEqual((1, "fix"), (code, out["next"]))
            code, out = self.run_qc("findings", self.write(folder, "l.json", self.ledger("fixed")))
            self.assertEqual((0, "deliver"), (code, out["next"]))
            code, out = self.run_qc("findings", self.write(folder, "l.json", self.ledger(evidence="")))
            self.assertEqual((1, "verify"), (code, out["next"]))
            code, out = self.run_qc("findings", self.write(folder, "l.json", self.ledger(rounds=3)), "--cap", "3")
            self.assertEqual((1, "owner"), (code, out["next"]))
            code, out = self.run_qc("findings", self.write(folder, "l.json", self.ledger("fixed", rounds=4)))
            self.assertEqual((1, "owner"), (code, out["next"]))
            self.assertTrue(any("cap" in problem for problem in out["problems"]))
            log = Path(folder) / "log.md"
            ledger = self.ledger("fixed")
            ledger["rounds"][0]["findings"].append({"id": "f2", "t": "2:18", "severity": "major", "claim": "typo",
                                                    "verdict": "false", "evidence": "frame 138 reads Backend"})
            code, out = self.run_qc("findings", self.write(folder, "l.json", ledger), "--markdown", str(log))
            self.assertEqual((0, 0.5), (code, out["false_rate"]))
            self.assertIn("frame 138 reads Backend", log.read_text(encoding="utf-8"))

    @unittest.skipUnless(shutil.which("ffmpeg"), "ffmpeg not installed")
    def test_frames_sheet_and_spectrum(self):  # #6686
        self.assertEqual(96.0, self.qc.parse_time("1:36"))
        self.assertEqual(96.5, self.qc.parse_time("96.5"))
        with tempfile.TemporaryDirectory() as folder:
            clip, sheet, spectrum = (str(Path(folder) / n) for n in ("c.mp4", "s.png", "a.png"))
            DesignRound3Tests.ffmpeg("-f", "lavfi", "-i", "testsrc=s=320x240:d=4:r=10", "-f", "lavfi",
                                     "-i", "sine=f=440:d=4", "-shortest", "-pix_fmt", "yuv420p", clip)
            code, out = self.run_qc("frames", clip, "--at", "1", "--at", "0:03", "--width", "160",
                                    "--out", sheet, "--spectrum", spectrum)
            self.assertEqual(0, code, out)
            self.assertEqual([[0.0, 1.0, 2.0], [2.0, 3.0, 3.95]], [row["times"] for row in out["frames"]])
            probe = self.qc.video_stream(self.qc.ffprobe(sheet))
            self.assertEqual((480, 240), (probe["width"], probe["height"]))
            self.assertGreater(Path(spectrum).stat().st_size, 0)

    def test_fresh_flags_targets_older_than_sources(self):  # #6687
        with tempfile.TemporaryDirectory() as folder:
            source, target = Path(folder) / "make.py", Path(folder) / "out" / "scene.html"
            target.parent.mkdir()
            source.write_text("x", encoding="utf-8")
            target.write_text("y", encoding="utf-8")
            os.utime(source, (1000, 1000))
            os.utime(target, (2000, 2000))
            args = ("fresh", "--source", str(source), "--target", str(Path(folder) / "out" / "*.html"))
            self.assertEqual(0, self.run_qc(*args)[0])
            os.utime(source, (3000, 3000))
            code, out = self.run_qc(*args)
            self.assertEqual(1, code)
            self.assertEqual([str(target)], out["stale"])
            code, _ = self.run_qc("fresh", "--source", str(source), "--target", str(Path(folder) / "none*.html"))
            self.assertEqual(1, code)

    def test_all_caches_passing_steps_by_input_hash(self):  # #6688
        with tempfile.TemporaryDirectory() as folder:
            board = self.write(folder, "b.json", {"scenes": [{"id": "s1", "onScreenText": "one two", "duration": 4}]})
            bad = self.write(folder, "bad.json", {"scenes": [{"id": "s2", "onScreenText": "a b c d e f", "duration": 1}]})
            plan = self.write(folder, "p.json", {"checks": [{"check": "holds", "args": [board]},
                                                           {"check": "holds", "args": [bad]}]})
            cache = str(Path(folder) / "cache.json")
            first, second = (self.run_qc("all", plan, "--cache", cache) for _ in range(2))
            self.assertEqual((1, 0), (first[0], first[1]["cached"]))
            self.assertEqual((1, 1), (second[0], second[1]["cached"]), "only the passing step is cached")
            Path(board).write_text(json.dumps({"scenes": [{"id": "s1", "onScreenText": "one", "duration": 4}]}),
                                   encoding="utf-8")
            self.assertEqual(0, self.run_qc("all", plan, "--cache", cache)[1]["cached"])

    def test_ttslint_flags_spaced_letters(self):  # #6689
        with tempfile.TemporaryDirectory() as folder:
            spaced = self.write(folder, "s.json", {"lines": [{"id": "k1", "text": "x", "say": "the C L I cover it"}]})
            hyphen = self.write(folder, "h.json", {"lines": [{"id": "k1", "text": "x", "say": "the C-L-I covers it"}]})
            code, out = self.run_qc("ttslint", spaced)
            self.assertEqual(1, code)
            self.assertIn("spaced letters 'C L I'", out["hits"][0])
            self.assertEqual(0, self.run_qc("ttslint", hyphen)[0])

    def test_vopauses_flags_long_gaps_inside_a_clause(self):  # #6689
        def word(text, start, end):
            return {"word": text, "start": start, "end": end}
        with tempfile.TemporaryDirectory() as folder:
            bad = self.write(folder, "b.json", {"k5": [word("cl,", 0.0, 0.5), word("I", 1.8, 1.9), word("cover", 2.0, 2.4)]})
            ok = self.write(folder, "o.json", {"k6": {"words": [word("done.", 0.0, 0.5), word("Next", 1.7, 2.0)]}})
            code, out = self.run_qc("vopauses", bad)
            self.assertEqual(1, code)
            self.assertEqual([{"id": "k5", "after": "cl,", "before": "I", "at": 0.5, "gap": 1.3}], out["pauses"])
            self.assertEqual(0, self.run_qc("vopauses", ok)[0])

    def test_revealhold_resolves_beats_and_flags_late_reveals(self):  # #6690
        self.assertAlmostEqual(3.95, self.qc.resolve_at("e1+3.1+0.4", {"e1": 0.45}))
        self.assertAlmostEqual(1.8, self.qc.resolve_at("2-0.2", {}))
        with tempfile.TemporaryDirectory() as folder:
            late = self.write(folder, "t.json", {"scenes": [{"id": "s1", "start": 0, "duration": 10, "beats": {},
                                                             "reveals": [2, 9.2]}]})
            code, out = self.run_qc("revealhold", late)
            self.assertEqual(1, code)
            self.assertEqual("s1", out["problems"][0]["id"])
            html = Path(folder) / "html"
            html.mkdir()
            (html / "s1.html").write_text('<div data-at="e1+3.1+0.4">a</div><b data-at="0">t</b>', encoding="utf-8")
            timeline = self.write(folder, "h.json", {"scenes": [{"id": "s1", "start": 0, "duration": 6,
                                                                 "beats": {"e1": 0.45}}]})
            code, out = self.run_qc("revealhold", timeline, "--html", str(html))
            self.assertEqual(0, code, out)
            (html / "s1.html").write_text('<div data-at="e9+1">a</div>', encoding="utf-8")
            self.assertEqual(1, self.run_qc("revealhold", timeline, "--html", str(html))[0])

    @unittest.skipUnless(shutil.which("ffmpeg") and shutil.which("ffprobe"), "ffmpeg not installed")
    def test_gapfloor_flags_a_bed_between_lines_and_allows_deliberate_sound(self):  # #6706
        with tempfile.TemporaryDirectory() as folder:
            timeline = self.write(folder, "edl.json", {"lines": [{"start": 1.0, "end": 2.0}, {"start": 4.0, "end": 5.0}]})

            def mix(name: str, source: str) -> str:
                path = str(Path(folder) / name)
                subprocess.run([shutil.which("ffmpeg"), "-v", "error", "-f", "lavfi", "-i", source,  # nosec B603
                                "-ac", "1", "-c:a", "pcm_f32le", path], check=True)
                return path
            bed = mix("bed.wav", "aevalsrc=0.0112*sin(2*PI*110*t):s=48000:d=6")      # about -42 dBFS RMS
            quiet = mix("quiet.wav", "aevalsrc=0.0002*sin(2*PI*110*t):s=48000:d=6")  # about -77 dBFS RMS
            code, out = self.run_qc("gapfloor", bed, "--timeline", timeline, "--fade", "0")
            self.assertEqual((1, "fail"), (code, out["status"]))
            self.assertAlmostEqual(-42.0, out["floor_dbfs"], delta=1.0)
            self.assertEqual(0, self.run_qc("gapfloor", quiet, "--timeline", timeline, "--fade", "0")[0])
            allowed = self.run_qc("gapfloor", bed, "--timeline", timeline, "--fade", "0",
                                  "--allow", "0-1", "--allow", "2-4", "--allow", "5-6")[1]
            self.assertEqual(("pass", 0.0), (allowed["status"], allowed["gap_seconds"]))

    def test_delivery_knows_the_vertical_2160_preset(self):  # #6706
        self.assertEqual((2160, 3840), self.qc.PRESETS["vertical-2160"])

    def test_cards_carry_the_2160p_and_clean_audio_rules(self):  # #6706
        def card(name: str) -> str:
            return (DESIGN / "references" / f"{name}.md").read_text(encoding="utf-8")
        for rule in ("gapfloor", "drone", "-14 LUFS", "limiter"):
            self.assertIn(rule, card("audio-mix-loudness"))
        self.assertIn("Locate the noise before removing it", card("noise-removal"))
        self.assertIn("device scale\n  factor 2", card("html-motion-graphics"))
        for rule in ("vertical-2160", "gapfloor", "lanczos"):
            self.assertIn(rule, card("delivery-qc"))
        self.assertIn("re-shot, not upscaled", card("screen-capture"))

    def test_cards_carry_the_retrospective_rules(self):  # #6684
        def card(name: str) -> str:
            return (DESIGN / "references" / f"{name}.md").read_text(encoding="utf-8")
        runbook = (DESIGN / "references/pipelines/runbook.md").read_text(encoding="utf-8")
        for rule in ("design_qc.py findings", "design_qc.py frames", "design_qc.py fresh", "--cache",
                     "one reviewer per output", "measured facts", "3 rounds", "must_not", "tool.py job wait"):
            self.assertIn(rule, runbook)
        self.assertIn("findings", card("delivery-qc"))
        voice = card("voice-over-tts")
        for rule in ("vopauses", "C-L-I"):
            self.assertIn(rule, voice)
        motion = card("html-motion-graphics")
        for rule in ("revealhold", "1.5 s", "orphan"):
            self.assertIn(rule, motion)

    def test_cards_carry_the_v2_video_lessons(self):  # #6739
        def card(name: str) -> str:
            return " ".join((DESIGN / "references" / f"{name}.md").read_text(encoding="utf-8").split())
        self.assertIn("0.9 x the D09 device scale factor", card("color-grading"))
        self.assertIn("own soft knee", card("color-grading"))
        self.assertIn("one browser at a time", card("html-motion-graphics"))
        self.assertIn("misses sub-pixel motion", card("cutting-pacing"))
        self.assertIn("re-measure them after every re-encode", card("cutting-pacing"))
        delivery = card("delivery-qc")
        for rule in ("100 MB", "2 GB", "previous embed pull request", "(`-v2`)", "staged hashes",
                     "--latest=false", "pages/builds/latest"):
            with self.subTest(rule=rule):
                self.assertIn(rule, delivery)
        runbook = " ".join((DESIGN / "references/pipelines/runbook.md").read_text(encoding="utf-8").split())
        for rule in ("per-call tool limit", "new file name", "## 6. Learning Session",
                     "Learning Session after every final delivery", "ONE PR", "skip-release-notes"):
            with self.subTest(rule=rule):
                self.assertIn(rule, runbook)


if __name__ == "__main__":
    unittest.main()
