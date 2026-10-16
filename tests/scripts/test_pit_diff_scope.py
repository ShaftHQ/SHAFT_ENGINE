"""Tests for scripts/ci/pit_diff_scope.py (#7004)."""
from __future__ import annotations

import importlib.util
import io
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
import unittest.mock

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("pit_diff_scope", ROOT / "scripts/ci/pit_diff_scope.py")
pit = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(pit)

REPORT = """<mutations>
<mutation detected="true" status="KILLED"><mutatedClass>com.shaft.A</mutatedClass><lineNumber>3</lineNumber><description>negated</description></mutation>
<mutation detected="false" status="SURVIVED"><mutatedClass>com.shaft.A</mutatedClass><lineNumber>7</lineNumber><description>removed call</description></mutation>
<mutation detected="true" status="TIMED_OUT"><mutatedClass>com.shaft.B</mutatedClass><lineNumber>1</lineNumber><description>x</description></mutation>
<mutation detected="false" status="NO_COVERAGE"><mutatedClass>com.shaft.B</mutatedClass><lineNumber>2</lineNumber><description>y</description></mutation>
</mutations>"""


class ScopeTest(unittest.TestCase):
    def test_maps_main_classes_and_skips_tests_and_infos(self) -> None:
        got = pit.scope([
            "shaft-cli/src/main/java/com/shaft/cli/Main.java",
            "shaft-cli/src/test/java/com/shaft/cli/MainTest.java",
            "shaft-cli/src/main/java/com/shaft/cli/package-info.java",
            "pom.xml",
            "shaft-cli/src/main/resources/x.java",
        ])
        self.assertEqual({"shaft-cli": ["com.shaft.cli.Main"]}, got)

    def test_caps_modules_and_classes(self) -> None:
        paths = [f"m{i}/src/main/java/a/C{j}.java" for i in range(3) for j in range(4)]
        got = pit.scope(paths, max_modules=2, max_classes=3)
        self.assertEqual(["m0", "m1"], list(got))
        self.assertEqual(["a.C0", "a.C1", "a.C2"], got["m0"])

    def test_cli_scope_prints_tab_separated(self) -> None:
        out = io.StringIO()
        with unittest.mock.patch("sys.stdin", io.StringIO("m/src/main/java/a/B.java\nm/src/main/java/a/C.java\n")), redirect_stdout(out):
            self.assertEqual(0, pit.main(["scope"]))
        self.assertEqual("m\ta.B,a.C\n", out.getvalue())


class SummaryTest(unittest.TestCase):
    def test_score_counts_and_survivors(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            report = Path(tmp, "m", "target", "pit-reports")
            report.mkdir(parents=True)
            (report / "mutations.xml").write_text(REPORT, encoding="utf-8")
            out = io.StringIO()
            with redirect_stdout(out):
                pit.main(["summary", tmp])
        text = out.getvalue()
        self.assertIn("Mutation score: **50%** (2/4 detected)", text)
        self.assertIn("| SURVIVED | 1 |", text)
        self.assertIn("`com.shaft.A:7` removed call", text)

    def test_no_reports_is_explicit(self) -> None:
        self.assertIn("No mutations generated", pit.summarize([]))


if __name__ == "__main__":
    unittest.main()
