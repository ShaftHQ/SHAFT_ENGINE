import tempfile
import unittest
from pathlib import Path

from scripts.ci.jacoco_coverage_gate import load_metrics, to_markdown


class JacocoCoverageGateTests(unittest.TestCase):
    def test_loads_aggregate_metrics_from_jacoco_csv(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            report = Path(temp_dir) / "jacoco.csv"
            report.write_text(
                "GROUP,PACKAGE,CLASS,INSTRUCTION_MISSED,INSTRUCTION_COVERED,BRANCH_MISSED,BRANCH_COVERED,LINE_MISSED,LINE_COVERED,COMPLEXITY_MISSED,COMPLEXITY_COVERED,METHOD_MISSED,METHOD_COVERED\n"
                "bundle,pkg,A,1,9,2,8,3,7,0,0,4,6\n"
                "bundle,pkg,B,0,10,1,9,2,8,0,0,3,7\n",
                encoding="utf-8",
            )

            metrics = {metric.name: metric for metric in load_metrics(report)}

        self.assertEqual(metrics["instruction"].covered, 19)
        self.assertEqual(metrics["instruction"].total, 20)
        self.assertEqual(metrics["line"].missed, 5)
        self.assertAlmostEqual(metrics["line"].percent, 75.0)

    def test_renders_markdown_with_gate_marker(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            report = Path(temp_dir) / "jacoco.csv"
            report.write_text(
                "GROUP,PACKAGE,CLASS,INSTRUCTION_MISSED,INSTRUCTION_COVERED,BRANCH_MISSED,BRANCH_COVERED,LINE_MISSED,LINE_COVERED,COMPLEXITY_MISSED,COMPLEXITY_COVERED,METHOD_MISSED,METHOD_COVERED\n"
                "bundle,pkg,A,0,10,0,10,0,10,0,0,0,10\n",
                encoding="utf-8",
            )

            markdown = to_markdown(load_metrics(report), "line", 80.0)

        self.assertIn("| line | 10 | 10 | 100.00% | minimum 80.00% |", markdown)


if __name__ == "__main__":
    unittest.main()
