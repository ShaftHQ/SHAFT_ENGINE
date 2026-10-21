"""#7242 chunk 3: the lazy harness map is complete, fresh, and costs one entry line."""

from __future__ import annotations

import importlib.util
import subprocess  # nosec B404 - runs the repository's own tool.py
import sys
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


if __name__ == "__main__":
    unittest.main()
