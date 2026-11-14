"""Implementer opens named sites before surveying the tree (#8036)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ROLES = ROOT / "chaos-engine/references/roles.md"


def implementer_section(text: str) -> str:
    match = re.search(
        r"(?m)^## Implementer\s*$\n(.*?)(?=^##\s|\Z)",
        text,
        re.DOTALL,
    )
    if match is None:
        raise AssertionError("missing ## Implementer section")
    return " ".join(match.group(1).split())


class ImplementerNamedSitesTest(unittest.TestCase):
    def setUp(self) -> None:
        self.section = implementer_section(ROLES.read_text(encoding="utf-8"))

    def test_exact_edit_still_applies_before_any_other_read(self) -> None:
        self.assertIn(
            "When the brief already contains the exact edit, apply that edit before any other read.",
            self.section,
        )

    def test_named_sites_are_edited_before_a_broader_search(self) -> None:
        named = "names the files, the functions, and the assertion or failure text"
        edit = "open those sites and edit them before a broader search"
        self.assertIn(named, self.section)
        self.assertIn(edit, self.section)
        self.assertLess(self.section.index(named), self.section.index(edit))

    def test_further_read_only_for_missing_symbol_or_disagreement(self) -> None:
        self.assertIn(
            "Read another file only when a named symbol is missing, or when "
            "the named file disagrees with the brief.",
            self.section,
        )

    def test_brief_that_names_no_files_may_still_discover(self) -> None:
        discovery = "discovery stays allowed"
        unnamed = "names no files"
        self.assertIn(discovery, self.section)
        self.assertIn(unnamed, self.section)
        self.assertLess(self.section.index(discovery), self.section.index(unnamed))

    def test_stop_when_the_prompt_and_the_code_disagree(self) -> None:
        self.assertIn(
            "If several minutes pass with no diff, stop and report that instead of another survey.",
            self.section,
        )
        self.assertIn(
            "If the prompt and the code disagree, or the brief is ambiguous, "
            "stop and return the ambiguity.",
            self.section,
        )
        self.assertIn("Do not guess.", self.section)

    def test_shape_without_named_sites_keeps_tdd_and_ponytail(self) -> None:
        self.assertIn("using TDD and Ponytail", self.section)


if __name__ == "__main__":
    unittest.main()
