"""#5774: durable native Memory is tracked; Graphify/MemPalace retrieve docs stay active."""

from __future__ import annotations

import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GITIGNORE = ROOT / ".gitignore"
RETRIEVE = ROOT / "chaos-engine/references/retrieve-first.md"
GRAPHIFY = ROOT / "chaos-engine/references/graphify.md"
SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
GOTCHA = (
    ROOT
    / ".memory/memory/gotchas/durable-native-memory-is-tracked-not-gitignored.md"
)


class MemoryTracked5774Test(unittest.TestCase):
    def test_gitignore_does_not_ignore_durable_memory_trees(self) -> None:
        text = GITIGNORE.read_text(encoding="utf-8")
        self.assertIn("Durable native Memory is tracked", text)
        self.assertNotIn(".memory/memory/*", text)
        self.assertNotIn(".memory/relations/*", text)
        self.assertIn(".memory/private/", text)
        self.assertIn("graphify-out/", text)
        # durable gotcha path must not be ignored
        sample = (
            ".memory/memory/gotchas/"
            "durable-native-memory-is-tracked-not-gitignored.md"
        )
        result = subprocess.run(
            ["git", "-C", str(ROOT), "check-ignore", "-v", sample],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertNotEqual(0, result.returncode, result.stdout)

    def test_mempalace_yaml_is_not_ignored(self) -> None:
        result = subprocess.run(
            ["git", "-C", str(ROOT), "check-ignore", "-v", "mempalace.yaml"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertNotEqual(0, result.returncode, result.stdout)

    def test_retrieve_first_and_graphify_instructions_are_active(self) -> None:
        skill = SKILL.read_text(encoding="utf-8")
        self.assertIn("retrieve-first.md", skill)
        self.assertIn("MemPalace", skill)
        self.assertIn("Graphify", skill)
        retrieve = RETRIEVE.read_text(encoding="utf-8")
        self.assertIn("MemPalace", retrieve)
        self.assertIn("Graphify", retrieve)
        self.assertIn("memory search", retrieve)
        graphify = GRAPHIFY.read_text(encoding="utf-8")
        self.assertIn("graphify query", graphify)
        self.assertIn("Retrieve first", graphify)
        self.assertTrue(GOTCHA.is_file())
        self.assertIn("#5774", GOTCHA.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
