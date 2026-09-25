"""The five local runtime bodies live under local-runtimes/references (#6200)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SKILLS = ROOT / "chaos-engine/skills"
FOLDED = ("omniroute", "freetoken", "colibri", "local-openai-compat", "local-coding-delegate")
POINTER_BYTES = 700


class LocalRuntimesFoldTest(unittest.TestCase):
    def test_runtime_skills_are_thin_compatibility_pointers(self):
        for name in FOLDED:
            with self.subTest(name=name):
                pointer = (SKILLS / name / "SKILL.md").read_text(encoding="utf-8")
                self.assertLessEqual(len(pointer.encode("utf-8")), POINTER_BYTES)
                self.assertIn("Compatibility pointer", pointer)
                self.assertIn(f"(../local-runtimes/references/{name}.md)", pointer)

    def test_local_runtimes_table_opens_the_folded_bodies(self):
        table = (SKILLS / "local-runtimes/SKILL.md").read_text(encoding="utf-8")
        for name in FOLDED:
            with self.subTest(name=name):
                self.assertIn(f"(references/{name}.md)", table)
                self.assertNotIn(f"(../{name}/SKILL.md)", table)

    def test_folded_bodies_resolve_every_relative_link(self):
        link = re.compile(r"(?<!!)\[[^]]*\]\(([^)\s]+)\)")
        for name in FOLDED:
            body = SKILLS / "local-runtimes/references" / f"{name}.md"
            for target in link.findall(body.read_text(encoding="utf-8")):
                path = target.split("#", 1)[0]
                if not path or re.match(r"^[a-z][a-z0-9+.-]*:", path, re.I):
                    continue
                with self.subTest(name=name, target=target):
                    self.assertTrue((body.parent / path).exists(), target)


if __name__ == "__main__":
    unittest.main()
