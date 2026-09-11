"""Javadoc @param arity checker for interaction overloads (#5748)."""

from __future__ import annotations

import importlib.util
import io
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "scripts/ci/check_javadoc_param_arity.py"
FRAMEWORK_SOURCE = (
    ROOT
    / "chaos-engine/profiles/shaft/references/playbooks/framework-source.md"
)
ZERO_LLM = ROOT / "chaos-engine/references/zero-llm-catalog.md"
HARNESS_PR_GATE = ROOT / "scripts/ci/harness_pr_gate.py"

BOGUS_THREE_ARG = '''
package demo;

public final class TypeStrategies {
    /**
     * Mobile native text entry.
     *
     * @param replaceAllowed when false, skip replaceElementValue
     */
    public static void typeMobileText(WebDriver driver, WebElement element, CharSequence[] text) {
        typeMobileText(driver, element, text, true);
    }

    /**
     * @param replaceAllowed when false, skip replaceElementValue
     */
    public static void typeMobileText(WebDriver driver, WebElement element, CharSequence[] text,
                                      boolean replaceAllowed) {
    }
}
'''

CLEAN_OVERLOADS = '''
package demo;

public final class ClickStrategies {
    /**
     * @param mobileNativeTouchFallback when true, retry with touch
     */
    public static void click(WebDriver driver, WebElement element, ElementKind kind,
                             boolean javascriptFallbackEnabled, boolean mobileNativeTouchFallback) {
    }

    /**
     * @param mobileNativeTouchFallback when true, retry with touch
     * @param windowsDesktop            when true, retry with windows: click
     */
    public static void click(WebDriver driver, WebElement element, ElementKind kind,
                             boolean javascriptFallbackEnabled, boolean mobileNativeTouchFallback,
                             boolean windowsDesktop) {
    }
}
'''

PROSE_MENTION = '''
package demo;

public final class Docs {
    /**
     * Do not copy {@code @param replaceAllowed} onto the 3-arg overload.
     * Example prose: avoid leftover @param replaceAllowed tags.
     */
    public static void typeMobileText(WebDriver driver, WebElement element, CharSequence[] text) {
    }
}
'''


def load_module():
    spec = importlib.util.spec_from_file_location("ce_check_javadoc_param_arity", MODULE_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError("could not load check_javadoc_param_arity.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CheckJavadocParamArityTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = load_module()

    def test_detects_bogus_param_on_shorter_overload(self):
        findings = self.mod.check_source(Path("TypeStrategies.java"), BOGUS_THREE_ARG)
        self.assertEqual(1, len(findings))
        self.assertEqual("replaceAllowed", findings[0].param)
        self.assertEqual("typeMobileText", findings[0].method)
        self.assertEqual(("driver", "element", "text"), findings[0].signature_params)

    def test_accepts_matching_overload_params(self):
        findings = self.mod.check_source(Path("ClickStrategies.java"), CLEAN_OVERLOADS)
        self.assertEqual([], findings)

    def test_ignores_prose_mentions_of_param_inside_javadoc(self):
        findings = self.mod.check_source(Path("Docs.java"), PROSE_MENTION)
        self.assertEqual([], findings)

    def test_cli_fails_on_fixture_and_passes_on_clean(self):
        with tempfile.TemporaryDirectory() as tmp:
            bad = Path(tmp) / "Bad.java"
            good = Path(tmp) / "Good.java"
            bad.write_text(BOGUS_THREE_ARG, encoding="utf-8")
            good.write_text(CLEAN_OVERLOADS, encoding="utf-8")
            stderr = io.StringIO()
            old = sys.stderr
            sys.stderr = stderr
            try:
                self.assertEqual(1, self.mod.main(["--paths", str(bad)]))
                self.assertIn("replaceAllowed", stderr.getvalue())
                self.assertEqual(0, self.mod.main(["--paths", str(good)]))
            finally:
                sys.stderr = old

    def test_live_interaction_package_is_clean(self):
        sources = self.mod.discover_sources(ROOT)
        self.assertTrue(
            sources,
            "expected */gui/element/internal/interaction sources in this checkout",
        )
        findings = self.mod.check_paths(sources)
        self.assertEqual(
            [],
            [f.format() for f in findings],
            "live interaction @param tags must match method arity",
        )

    def test_portable_payload_avoids_forbidden_product_tokens(self):
        text = MODULE_PATH.read_text(encoding="utf-8").casefold()
        for token in ("shaft", "mohab", "act-as-mohab", "act as mohab"):
            self.assertNotIn(token, text)

    def test_implementer_checklist_and_zero_llm_catalog_name_the_check(self):
        framework = FRAMEWORK_SOURCE.read_text(encoding="utf-8")
        catalog = ZERO_LLM.read_text(encoding="utf-8")
        gate = HARNESS_PR_GATE.read_text(encoding="utf-8")
        self.assertIn("Overload `@param` arity", framework)
        self.assertIn("../../../../../scripts/ci/check_javadoc_param_arity.py", framework)
        self.assertTrue(
            (FRAMEWORK_SOURCE.parent / "../../../../../scripts/ci/check_javadoc_param_arity.py")
            .resolve()
            .is_file(),
            "framework-source relative link to checker must resolve",
        )
        self.assertIn("[`check_javadoc_param_arity.py`](../../scripts/ci/check_javadoc_param_arity.py)", catalog)
        self.assertIn("[`check_javadoc_param_arity.py`](../../scripts/ci/check_javadoc_param_arity.py)", catalog)
        self.assertIn("javadoc-param-arity-contract", gate)
        self.assertIn("check_javadoc_param_arity.py", gate)


if __name__ == "__main__":
    unittest.main()
