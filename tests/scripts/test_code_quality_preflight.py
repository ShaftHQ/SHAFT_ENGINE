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

    def test_only_changed_lines_are_checked(self):
        source = "try:\n    x = 1\nexcept OSError:\n    pass\ny = f'plain'\n"
        self.assertEqual(len(findings(source, {5})), 1)
        self.assertEqual(findings(source, {1}), [])

    def test_overlay_pre_push_runs_the_preflight(self):
        text = (ROOT / "scripts/ci/overlay_pre_push.py").read_text(encoding="utf-8")
        self.assertIn("code_quality_failures(root)", text)


if __name__ == "__main__":
    unittest.main()
