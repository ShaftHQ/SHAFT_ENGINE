from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]


def contrast(first: str, second: str) -> float:
    def luminance(color: str) -> float:
        channels = [int(color[index:index + 2], 16) / 255 for index in (1, 3, 5)]
        channels = [value / 12.92 if value <= 0.04045 else ((value + 0.055) / 1.055) ** 2.4
                    for value in channels]
        return 0.2126 * channels[0] + 0.7152 * channels[1] + 0.0722 * channels[2]

    lighter, darker = sorted((luminance(first), luminance(second)), reverse=True)
    return (lighter + 0.05) / (darker + 0.05)


def tokens() -> dict[str, str]:
    values = {}
    for line in (ROOT / "design/shaft-design-tokens.properties").read_text(encoding="utf-8").splitlines():
        if line and not line.startswith("#"):
            key, value = line.split("=", 1)
            values[key] = value.lower()
    return values


class ShaftDesignTokensTest(unittest.TestCase):
    def test_shared_surfaces_keep_exact_shaft_identity_tokens(self):
        expected = tokens()
        report = (ROOT / "shaft-engine/src/main/java/com/shaft/tools/internal/support/ReportHtmlTheme.java").read_text(
            encoding="utf-8").lower()
        intellij = (ROOT / "shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftDesignTokens.java").read_text(
            encoding="utf-8").lower().replace("0x", "#")

        for key in ("light.primary", "light.muted", "light.pass", "light.warning", "light.fail",
                    "dark.primary", "dark.muted", "dark.pass", "dark.warning", "dark.fail"):
            self.assertIn(expected[key], report, f"report palette drifted at {key}")
            self.assertIn(expected[key], intellij, f"IntelliJ palette drifted at {key}")

    def test_progress_and_success_semantics_do_not_share_colors(self):
        expected = tokens()
        self.assertNotEqual(expected["light.primary"], expected["light.pass"])
        self.assertNotEqual(expected["dark.primary"], expected["dark.pass"])
        self.assertNotIn(expected["light.fail"], {expected["light.primary"], expected["light.pass"]})
        self.assertNotIn(expected["dark.fail"], {expected["dark.primary"], expected["dark.pass"]})

    def test_runtime_log_palettes_use_exact_semantic_tokens(self):
        expected = tokens()
        pattern = ("FATAL=fg_{fail} blink, ERROR=fg_{fail} bold, WARN=fg_{warning} bold, "
                   "INFO=fg_{primary} bold, DEBUG=fg_{muted}, TRACE=bright_black").format(
                       fail=expected["light.fail"], warning=expected["light.warning"],
                       primary=expected["light.primary"], muted=expected["light.muted"]).lower()
        for resource in ROOT.glob("shaft-*/src/main/resources/properties/default/log4j2.properties"):
            self.assertIn(pattern, resource.read_text(encoding="utf-8").lower(), str(resource))

    def test_assertion_overlay_text_meets_normal_text_contrast(self):
        expected = tokens()
        for background in (expected["light.pass"], expected["light.fail"]):
            self.assertGreaterEqual(contrast(expected["light.onDark"], background), 4.5)
        manager = (ROOT / "shaft-engine/src/main/java/com/shaft/gui/internal/image/ScreenshotManager.java").read_text(
            encoding="utf-8").lower()
        self.assertIn("color:#ffffff", manager)


if __name__ == "__main__":
    unittest.main()
