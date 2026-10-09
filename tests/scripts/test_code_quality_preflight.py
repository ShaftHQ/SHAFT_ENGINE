"""Code-quality preflight: Codacy-class findings caught before push."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("code_quality_preflight", ROOT / "scripts/ci/code_quality_preflight.py")
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


def findings(source: str, lines: set[int] | None = None) -> list[str]:
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "sample.py"
        path.write_text(textwrap.dedent(source), encoding="utf-8")
        return MODULE.file_findings(path, lines or {0}, "sample.py")


class CodeQualityPreflightTest(unittest.TestCase):
    def test_complexity_counts_decisions_like_codacy(self):
        branches = "\n".join(f"    if x == {index}:\n        return {index}" for index in range(15))
        source = f"def f(x):\n{branches}\n    return -1\n"
        import ast
        self.assertEqual(MODULE.complexity(ast.parse(source).body[0]), 16)
        self.assertIn("complexity: `f` is 16", findings(source)[0])
        trimmed = source.replace("    if x == 14:\n        return 14\n", "")
        self.assertEqual(findings(trimmed), [])

    def test_boolean_operators_and_comprehensions_do_not_count(self):
        import ast
        source = "def f(a, b):\n    return [x for x in a if x] if a and b or a else None\n"
        self.assertEqual(MODULE.complexity(ast.parse(source).body[0]), 1)

    def test_loop_else_and_nested_functions(self):
        import ast
        source = "def f(a):\n    for x in a:\n        pass\n    else:\n        pass\n    def g():\n        if a:\n            pass\n"
        self.assertEqual(MODULE.complexity(ast.parse(source).body[0]), 3)

    def test_empty_except_needs_a_comment(self):
        bare = "try:\n    x = 1\nexcept OSError:\n    pass\n"
        self.assertIn("empty-except", findings(bare)[0])
        explained = "try:\n    x = 1\nexcept OSError:\n    pass  # best effort: cleanup may race\n"
        self.assertEqual(findings(explained), [])

    def test_unnecessary_lambda(self):
        self.assertIn("unnecessary-lambda", findings("key = lambda item: str(item)\n")[0])
        self.assertEqual(findings("key = lambda item: item.name(item)\n"), [])
        self.assertEqual(findings("key = lambda item: str(item, 'x')\n"), [])

    def test_f_string_without_placeholders_ignores_format_specs(self):
        self.assertIn("f-string-without-placeholders", findings("x = f'plain'\n")[0])
        self.assertEqual(findings("w = 3\nx = f'{w:>10}'\n"), [])


    def test_weak_assert_true_false_wrapping_compare(self):
        self.assertIn("assertGreater", findings("self.assertTrue(a > b)\n")[0])
        self.assertIn("assertIn", findings("self.assertTrue(x in items)\n")[0])
        self.assertIn("assertNotIn", findings("self.assertFalse(x in items)\n")[0])
        self.assertIn("assertEqual", findings("assertTrue(a == b)\n")[0])
        self.assertEqual(findings("self.assertTrue(ready)\n"), [])
        self.assertEqual(findings("self.assertGreater(a, b)\n"), [])

    def test_only_changed_lines_are_checked(self):
        source = "try:\n    x = 1\nexcept OSError:\n    pass\ny = f'plain'\n"
        self.assertEqual(len(findings(source, {5})), 1)
        self.assertEqual(findings(source, {1}), [])

    def test_overlay_pre_push_runs_the_preflight(self):
        text = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        self.assertIn("code_quality_failures(root)", text)


# PMD 7.7.0 NPathComplexity reports for this sample (reportLevel 1): the estimator must match.
JAVA_SAMPLE = """\
class Sample {
    int sequential(int a) {
        if (a > 1) { a++; }
        if (a > 2) { a++; }
        if (a > 3 && a < 9) { a++; } else { a--; }
        return a;
    }
    int loops(java.util.List<Integer> xs) {
        int total = 0;
        for (int x : xs) { if (x > 0) { total += x; } }
        for (int i = 0; i < 3 || total > 9; i++) { total--; }
        while (total > 100) { total /= 2; }
        return total > 0 ? total : -total;
    }
    String tryAndTernary(String s) {
        try (java.io.StringReader r = new java.io.StringReader(s)) {
            s = s.isEmpty() ? "x" : s.isBlank() ? "y" : s;
        } catch (java.io.UncheckedIOException e) {
            if (e.getMessage() == null) { return null; }
        } finally {
            s = s.trim();
        }
        return s;
    }
    int switched(String kind) {
        return switch (kind) {
            case "a", "b" -> 1;
            case "c" -> kind.isEmpty() ? 2 : 3;
            default -> 4;
        };
    }
    int switchStatement(int k) {
        switch (k) {
            case 1:
            case 2:
                k++;
                break;
            case 3:
                k--;
                break;
            default:
                k = 0;
        }
        return k;
    }
    Runnable lambdas(boolean flag) {
        Runnable r = () -> {
            if (flag || !flag) { System.out.println(); }
            if (flag) { System.out.println(); }
        };
        return r;
    }
}
"""
PMD_NPATH = {"sequential": 12, "loops": 54, "tryAndTernary": 7, "switched": 4, "switchStatement": 4, "lambdas": 6}


def java_findings(source: str, lines: set[int] | None = None, baseline: str | None = None) -> list[str]:
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "Sample.java"
        path.write_text(source, encoding="utf-8")
        return MODULE.java_findings(path, lines or {0}, "Sample.java", baseline)


def wide_method(branches: int) -> str:
    body = "\n".join(f"        if (a == {index}) {{ a++; }}" for index in range(branches))
    return f"class Wide {{\n    int wide(int a) {{\n{body}\n        return a;\n    }}\n}}\n"


class JavaNpathPreflightTest(unittest.TestCase):
    def test_estimator_matches_pmd(self):
        npath = {name: value for name, _first, _last, value in MODULE._java_npath().methods(JAVA_SAMPLE)}
        self.assertEqual(npath, PMD_NPATH)

    def test_method_over_the_gate_is_flagged(self):
        self.assertEqual(java_findings(wide_method(7)), [])  # 2^7 = 128
        found = java_findings(wide_method(8))  # 2^8 = 256
        self.assertEqual(len(found), 1)
        self.assertIn("Sample.java:2 npath: `wide` is 256 (gate 200)", found[0])

    def test_only_changed_methods_count(self):
        source = wide_method(8)
        self.assertEqual(java_findings(source, {1}), [])
        self.assertEqual(len(java_findings(source, {4})), 1)

    def test_legacy_method_flagged_only_when_it_grows(self):
        self.assertEqual(java_findings(wide_method(8), {4}, baseline=wide_method(8)), [])
        self.assertEqual(len(java_findings(wide_method(9), {4}, baseline=wide_method(8))), 1)
        self.assertEqual(len(java_findings(wide_method(8), {4}, baseline=wide_method(7))), 1)

    def test_unparseable_java_does_not_block(self):
        self.assertEqual(java_findings("class Broken { void f() { if ( }"), [])


if __name__ == "__main__":
    unittest.main()
