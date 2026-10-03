"""Optional ChaosEngine add-ons: discovery, explicit selection, payload, wrappers, design QC."""

from __future__ import annotations

import importlib.util
import json
import re
import shutil
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

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
    "captions-subtitles", "video-restoration-upscaling", "delivery-qc",
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
    def test_router_links_twenty_cards_within_budget(self):
        router = (DESIGN / "SKILL.md").read_text(encoding="utf-8")
        self.assertLessEqual(len(router.encode("utf-8")), 4096)
        self.assertEqual(20, len(CARDS))
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


if __name__ == "__main__":
    unittest.main()
