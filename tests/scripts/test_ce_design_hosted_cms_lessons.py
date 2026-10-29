"""Hosted-CMS embed lessons L23–L31 + gates (Carl retro 2026-10-29)."""

from __future__ import annotations

import json
import subprocess  # nosec B404 - fixed list-form argv in tests, never a shell.
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))  # noqa: E402
from tests.scripts.ce_neutral_terms import private_terms

ROOT = Path(__file__).resolve().parents[2]
DESIGN = ROOT / "chaos-engine/addons/design-skills"
LESSONS = DESIGN / "references/lessons"
SITE = DESIGN / "references/site-builders"
QC = DESIGN / "scripts/design_qc.py"
GATES = DESIGN / "scripts/hosted_cms_gates.py"

CARDS = {
    "hosted-cms-no-dup-chrome.md": (
        "Body only",
        "Sticky site header offset",
        "--header-height",
        "scroll-margin-top",
        "cmssticky",
    ),
    "hosted-cms-assets.md": (
        "No relative local assets",
        "Probe CSP first",
        "cmsassets",
        "naturalWidth",
    ),
    "hosted-cms-owns-meta.md": (
        "No paste social meta",
        "cmsmeta",
        "UNFIXABLE",
    ),
    "hostile-cms-resets.md": (
        "Hostile injection before SIGN",
        "Near-white headings on dark",
        "Identity squircle",
        "hostilecms",
        "0.85",
    ),
    "full-width-embed.md": (
        "Span the content grid",
        "0.92",
        "cmswidth",
        "L23",
    ),
    "live-verify-cms-embed.md": (
        "Public URL, not editor",
        "390 and 1440",
        "livecms",
        "Fail closed",
    ),
    "plan-first-multi-axis.md": (
        "Plan before build",
        "Acceptance first",
        "design-loop",
    ),
    "explainer-embed-length.md": (
        "60–100 s",
        "120 s",
        "cmsduration",
        "30 s",
    ),
    "paste-then-reverify.md": (
        "Build artifact only",
        "Re-verify immediately",
        "livecms",
        "Nested",
    ),
}


def run_qc(*args: str) -> tuple[int, dict]:
    result = subprocess.run(  # nosec B603
        [sys.executable, str(QC), *args],
        capture_output=True, text=True, check=False,
    )
    lines = [ln for ln in result.stdout.strip().splitlines() if ln.strip()]
    payload = json.loads(lines[-1]) if lines else {
        "status": "error", "stdout": result.stdout, "stderr": result.stderr,
    }
    return result.returncode, payload


class HostedCmsLessonCardsTest(unittest.TestCase):
    def test_cards_needles_budget_index(self) -> None:
        index = (LESSONS / "index.md").read_text(encoding="utf-8")
        for name, needles in CARDS.items():
            raw = (LESSONS / name).read_text(encoding="utf-8")
            with self.subTest(card=name):
                self.assertLessEqual(len(raw.encode("utf-8")), 6144)
                self.assertIn("## Why", raw)
                self.assertIn("## Verify", raw)
                text = " ".join(raw.split())
                for needle in needles:
                    self.assertIn(needle, text)
                self.assertIn(f"]({name})", index)
        playbook = (SITE / "hosted-cms-embed.md").read_text(encoding="utf-8")
        self.assertIn("Sticky site header offset", playbook)
        self.assertIn("L23", playbook)
        sq = (SITE / "squarespace-7-1.md").read_text(encoding="utf-8")
        self.assertIn("Probe the live origin", sq)
        self.assertIn("hosted-cms-embed", sq)
        self.assertIn("livecms", sq)
        inv = " ".join((DESIGN / "INVENTORY.md").read_text(encoding="utf-8").split())
        self.assertIn("31 lesson", inv)
        self.assertIn("L01-L31", inv)
        self.assertIn("L32 reserved", inv)
        craft = (DESIGN / "references/craft/index.md").read_text(encoding="utf-8")
        self.assertIn("hosted-CMS no-dup chrome", craft)
        self.assertIn("Hosted CMS embed", craft)
        # L32 must not exist yet (Arabic pending)
        self.assertFalse(any(LESSONS.glob("*arabic*tech*")))
        self.assertNotIn("L32 ", index)

    def test_cards_are_project_neutral(self) -> None:
        banned = (
            "automatest", "mohab", "hrsd", "elmenus", "stc", "price",
            "carl", "coach", *private_terms(),
        )
        names = list(CARDS) + ["hosted-cms-embed.md", "squarespace-7-1.md"]
        for name in names:
            path = LESSONS / name if name in CARDS else SITE / name
            lowered = path.read_text(encoding="utf-8").lower()
            for word in banned:
                self.assertNotIn(word, lowered, f"{name}: {word}")


class CmsStickyHeaderOffsetTest(unittest.TestCase):
    """Riskiest assumption: cmssticky fails when sticky offset tokens absent."""

    def test_cms_sticky_requires_header_offset(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.css"
            good = Path(folder) / "good.css"
            bad.write_text(".embed { padding-top: 96px; }", encoding="utf-8")
            good.write_text(
                ".embed{--header-height:var(--header-height,5.75rem);"
                "scroll-margin-top:calc(var(--header-height) + .5rem)}",
                encoding="utf-8",
            )
            code, payload = run_qc("cmssticky", str(bad))
            self.assertEqual(1, code, payload)
            self.assertEqual("fail", payload.get("status"), payload)
            self.assertIn("--header-height", payload.get("missing", []))
            code, payload = run_qc("cmssticky", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class CmsMetaAssetsChromeTest(unittest.TestCase):
    def test_cmsmeta_rejects_og(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.html"
            good = Path(folder) / "good.html"
            bad.write_text(
                '<html><head><meta property="og:title" content="x"></head></html>',
                encoding="utf-8",
            )
            good.write_text("<html><head><title>x</title></head><body></body></html>", encoding="utf-8")
            code, payload = run_qc("cmsmeta", str(bad))
            self.assertEqual(1, code, payload)
            code, payload = run_qc("cmsmeta", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_cmsassets_rejects_relative(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.html"
            good = Path(folder) / "good.html"
            bad.write_text('<img src="../assets/mark.svg">', encoding="utf-8")
            good.write_text('<img src="https://cdn.example.com/mark.svg">', encoding="utf-8")
            code, payload = run_qc("cmsassets", str(bad))
            self.assertEqual(1, code, payload)
            self.assertGreater(payload.get("count", 0), 0)
            code, payload = run_qc("cmsassets", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_cmschrome_rejects_topbar(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            bad = Path(folder) / "bad.html"
            good = Path(folder) / "good.html"
            bad.write_text('<header class="topbar"><nav></nav></header><main></main>', encoding="utf-8")
            good.write_text('<div class="product-embed"><section class="hero"></section></div>', encoding="utf-8")
            code, payload = run_qc("cmschrome", str(bad))
            self.assertEqual(1, code, payload)
            code, payload = run_qc("cmschrome", str(good))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class HostileCmsAndWidthDurationTest(unittest.TestCase):
    def test_hostile_heading_and_tile(self) -> None:
        code, payload = run_qc("hostilecms", "--heading-color", "rgb(26,111,192)")
        self.assertEqual(1, code, payload)
        code, payload = run_qc("hostilecms", "--heading-color", "rgb(255,255,255)")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)
        code, payload = run_qc(
            "hostilecms",
            "--tile-w", "88", "--tile-h", "88",
            "--img-x", "10", "--img-y", "10", "--img-w", "60", "--img-h", "60",
            "--tile-x", "0", "--tile-y", "0",
        )
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)
        code, payload = run_qc(
            "hostilecms",
            "--tile-w", "88", "--tile-h", "40",
            "--img-x", "0", "--img-y", "0", "--img-w", "20", "--img-h", "20",
        )
        self.assertEqual(1, code, payload)

    def test_cmswidth_ratio(self) -> None:
        code, payload = run_qc("cmswidth", "--code", "800", "--column", "1000")
        self.assertEqual(1, code, payload)
        code, payload = run_qc("cmswidth", "--code", "950", "--column", "1000")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)

    def test_cmsduration_band_and_peer(self) -> None:
        code, payload = run_qc("cmsduration", "--dur", "225")
        self.assertEqual(1, code, payload)
        code, payload = run_qc("cmsduration", "--dur", "86", "--peer", "100")
        self.assertEqual((0, "pass"), (code, payload["status"]), payload)
        code, payload = run_qc("cmsduration", "--dur", "86", "--peer", "140")
        self.assertEqual(1, code, payload)


class LivecmsMatrixTest(unittest.TestCase):
    def test_livecms_fail_closed_on_partial(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            partial = Path(folder) / "partial.json"
            full = Path(folder) / "full.json"
            partial.write_text(json.dumps({"http_200": True, "viewport_390": True}), encoding="utf-8")
            full.write_text(json.dumps({
                "http_200": True,
                "no_dup_chrome": True,
                "assets_loaded": True,
                "heading_colour": "pass",
                "identity_tile": "pass",
                "og_title_once": True,
                "viewport_390": True,
                "viewport_1440": True,
            }), encoding="utf-8")
            code, payload = run_qc("livecms", str(partial))
            self.assertEqual(1, code, payload)
            self.assertTrue(payload.get("missing"), payload)
            code, payload = run_qc("livecms", str(full))
            self.assertEqual((0, "pass"), (code, payload["status"]), payload)


class GatesModuleImportTest(unittest.TestCase):
    def test_module_importable(self) -> None:
        self.assertTrue(GATES.is_file())
        import importlib.util
        spec = importlib.util.spec_from_file_location("hosted_cms_gates", GATES)
        mod = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(mod)
        self.assertEqual("fail", mod.check_cms_sticky(".x{padding:1px}").get("status"))


if __name__ == "__main__":
    unittest.main()
