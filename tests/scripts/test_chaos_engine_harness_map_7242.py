"""#7242 chunk 3: the lazy harness map is complete, fresh, and costs one entry line."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import re
import subprocess  # nosec B404 - runs the repository's own tool.py
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load(path: Path, name: str):
    specification = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


class HarnessMapTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.map = load(CE / "harness_map.py", "ce_harness_map_7242")
        cls.text = (CE / cls.map.MAP).read_text(encoding="utf-8")

    def test_map_is_fresh(self):
        self.assertEqual(self.map.render(CE), self.text, "run python3 chaos-engine/harness_map.py --write")

    def test_every_harness_file_is_listed(self):
        expected = set()
        for pattern in ("identity.md", "companions/*.md", "skills/*/SKILL.md",
                        "references/*.md", "hooks/*.py", "*.py"):
            expected |= {p.relative_to(CE).as_posix() for p in CE.glob(pattern) if p.is_file()}
        expected.discard(self.map.MAP)
        missing = sorted(path for path in expected if f"`{path}`:" not in self.text)
        self.assertEqual([], missing)

    def test_every_index_skill_inside_the_overlay_is_listed(self):
        import json
        index = json.loads((CE / "harness-index.json").read_text(encoding="utf-8"))
        for entry in index["entries"]:
            path = entry["path"]
            if path.startswith("skills/") and path.count("/") == 2 and (CE / path).is_file():
                self.assertIn(f"`{path}`:", self.text)

    def test_entry_prints_one_pointer_line_not_the_map(self):
        out = subprocess.run([sys.executable, str(CE / "tool.py"), "entry", "--full"],  # nosec B603
                             cwd=ROOT, capture_output=True, text=True, timeout=60, check=True).stdout
        lines = [line for line in out.splitlines() if line.startswith("entry: ")]
        self.assertEqual(1, len(lines))
        self.assertIn(f"index={len(self.map.mapped_files(CE))}", lines[0])
        self.assertIn("references/harness-map.md", out)
        self.assertNotIn("# Harness map", out)

    def test_real_rendered_map_fits_the_reference_cap(self):
        rendered = self.map.render(CE)
        budget_path = ROOT / "scripts/ci/agent_guidance_budget.json"
        budgets = json.loads(budget_path.read_text(encoding="utf-8"))["file_budgets"]
        ceiling = budgets["chaos-engine/references/*.md"]["max_bytes"]
        self.assertIsNone(self.map.overflow_finding(ROOT, rendered))
        self.assertLessEqual(len(rendered.encode("utf-8")), ceiling)
        self.assertEqual(0, self.map.main(["--check"]))

    def test_check_passes_a_fixture_cap_above_the_rendered_map(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            _write_map_budget(repo, 10**9)
            self.assertEqual(0, self.map.main(["--check"], repo=repo))

    def test_check_fails_when_rendered_map_exceeds_the_reference_cap(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            budget_path = _write_map_budget(repo, 10)
            rendered = self.map.render(CE)
            buffer = io.StringIO()
            with contextlib.redirect_stderr(buffer):
                code = self.map.main(["--check"], repo=repo)
            self.assertEqual(1, code)
            match = re.fullmatch(
                r"harness-map overflow: host=harness-map count=(\d+) ceiling=(\d+)",
                buffer.getvalue().strip(),
            )
            self.assertIsNotNone(match)
            budgets = json.loads(budget_path.read_text(encoding="utf-8"))["file_budgets"]
            ceiling = budgets["chaos-engine/references/*.md"]["max_bytes"]
            self.assertEqual(len(rendered.encode("utf-8")), int(match.group(1)))
            self.assertEqual(ceiling, int(match.group(2)))
            self.assertGreater(int(match.group(1)), ceiling)


def _write_map_budget(repo: Path, ceiling: int) -> Path:
    path = repo / "scripts/ci/agent_guidance_budget.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"file_budgets": {"chaos-engine/references/*.md": {"max_bytes": ceiling}}}),
        encoding="utf-8",
    )
    return path


if __name__ == "__main__":
    unittest.main()
