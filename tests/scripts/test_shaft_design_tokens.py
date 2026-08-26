from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]


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


if __name__ == "__main__":
    unittest.main()
