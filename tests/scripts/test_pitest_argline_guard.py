"""Weekly PIT must not inherit unresolved Surefire/JaCoCo @{argLine}."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def _pitest_profile_block(pom_text: str) -> str:
    """Return the <profile>…</profile> that declares <id>pitest</id>."""
    match = re.search(
        r"<profile>\s*<id>pitest</id>.*?</profile>",
        pom_text,
        flags=re.DOTALL,
    )
    if match is None:
        raise AssertionError("pom.xml missing <profile><id>pitest</id>")
    return match.group(0)


class PitestArgLineGuardTest(unittest.TestCase):
    def test_pitest_profile_sets_explicit_argline(self) -> None:
        pom = (ROOT / "pom.xml").read_text(encoding="utf-8")
        block = _pitest_profile_block(pom)
        self.assertIn("<artifactId>pitest-maven</artifactId>", block)
        arg_match = re.search(r"<argLine>(.*?)</argLine>", block, flags=re.DOTALL)
        self.assertIsNotNone(arg_match, "pitest-maven must set argLine explicitly")
        text = (arg_match.group(1) or "").strip()
        self.assertTrue(text, "pitest argLine must be non-empty")
        self.assertNotIn("@{argLine}", text)
        self.assertNotIn("{argLine}", text)


if __name__ == "__main__":
    unittest.main()
