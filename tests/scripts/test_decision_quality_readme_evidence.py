"""README evidence block must be generated from the committed #5522 aggregate."""

from __future__ import annotations

import json
import shutil
import tempfile
import unittest
from pathlib import Path

from scripts.ci import validate_chaos_engine_readme as readme_owner


ROOT = Path(__file__).resolve().parents[2]
AGGREGATE = ROOT / "chaos-engine/decision-quality-calibration.aggregate.json"


class DecisionQualityReadmeEvidenceTest(unittest.TestCase):
    def test_renderer_uses_aggregate_values_without_zero_filling_unavailable(self):
        evidence = json.loads(AGGREGATE.read_text(encoding="utf-8"))
        render = getattr(readme_owner, "render_omniroute_evidence", None)
        self.assertTrue(callable(render))

        block = render(ROOT)
        metrics = evidence["metrics"]
        gate = evidence["comparison"]["gateVerdict"]

        self.assertIn("directional walking skeleton", block.casefold())
        self.assertIn(str(evidence["trialAccounting"]["observed"]), block)
        self.assertIn(str(gate["verdict"]), block)
        self.assertIn(str(metrics["control"]["tokens"]), block)
        self.assertIn(str(metrics["chaos-engine"]["tokens"]), block)
        self.assertIn(str(metrics["control"]["latency_seconds"]), block)
        self.assertIn(str(metrics["chaos-engine"]["latency_seconds"]), block)
        self.assertIn("UNAVAILABLE", block)
        self.assertIn("decision-quality-calibration.aggregate.json", block)
        self.assertIn("decision-quality-report.md", block)
        # Missing cost must stay literal UNAVAILABLE, never coerced to 0 in the table.
        self.assertRegex(block, r"\|\s*`cost_usd`\s*\|\s*UNAVAILABLE\s*\|\s*UNAVAILABLE\s*\|")

    def test_repository_readme_evidence_matches_renderer_and_write_is_idempotent(self):
        write_generated = getattr(readme_owner, "write_generated", None)
        self.assertTrue(callable(write_generated))
        self.assertEqual([], readme_owner.validate(ROOT))

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            shutil.copytree(ROOT / "chaos-engine", root / "chaos-engine")
            readme = root / "chaos-engine/README.md"
            original = readme.read_text(encoding="utf-8")
            start = "<!-- evidence:omniroute-calibration:start -->"
            end = "<!-- evidence:omniroute-calibration:end -->"
            self.assertEqual(1, original.count(start))
            self.assertEqual(1, original.count(end))

            before, rest = original.split(start, 1)
            _, after = rest.split(end, 1)
            readme.write_text(
                f"{before}{start}\n| Metric | control | chaos-engine |\n| --- | --- | --- |\n| `tokens` | 1 | 2 |\n{end}{after}",
                encoding="utf-8",
            )
            errors = readme_owner.validate(root)
            self.assertTrue(
                any("omniroute-calibration" in error for error in errors),
                errors,
            )

            write_generated(root)
            self.assertEqual([], readme_owner.validate(root))
            after_first = readme.read_text(encoding="utf-8")
            write_generated(root)
            self.assertEqual(after_first, readme.read_text(encoding="utf-8"))
            expected = readme_owner.render_omniroute_evidence(root)
            actual = after_first.split(start, 1)[1].split(end, 1)[0].strip()
            self.assertEqual(expected, actual)


if __name__ == "__main__":
    unittest.main()
