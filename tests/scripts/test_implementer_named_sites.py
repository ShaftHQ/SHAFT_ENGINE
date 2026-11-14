"""Implementer opens named sites before surveying the tree (#8036)."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ROLES = ROOT / "chaos-engine/references/roles.md"

SENTENCES = (
    "When the brief already contains the exact edit, apply that edit before any other read.",
    "When the brief names the files, the functions, and the assertion or failure text, but not the exact patch, open those sites and edit them before a broader search instead of surveying the rest of the tree first.",
    "Read another file only when a named symbol is missing.",
    "When the brief has neither an exact edit nor those named sites, discovery stays allowed, including a brief that names no files.",
    "If several minutes pass with no diff, stop and report that instead of another survey.",
    "If the prompt and the code disagree, or the brief is ambiguous, stop and return the ambiguity.",
    "Do not guess.",
)

INTRO = (
    "Implements one bounded spec using TDD and Ponytail. "
    "Runs at the default capability level unless the assignment states otherwise. "
    "Returns architectural ambiguity undecided. "
    "May assign only mechanical, spec-exact, or bulk work downward, then verifies its output."
)

FORBIDDEN = (
    "do not open those sites and edit them before a broader search",
    "survey the rest of the tree first. Then open those sites and edit them before a broader search",
    "Read another file only when a named symbol is missing, or when the named file disagrees with the brief.",
)


def implementer_section(text: str) -> str:
    match = re.search(
        r"(?m)^## Implementer\s*$\n(.*?)(?=^##\s|\Z)",
        text,
        re.DOTALL,
    )
    if match is None:
        raise AssertionError("missing ## Implementer section")
    return " ".join(match.group(1).split())


def policy_sentences(section: str) -> list[str]:
    start = section.index(SENTENCES[0])
    policy = section[start:]
    return [part.strip() + "." for part in policy.split(".") if part.strip()]


class ImplementerNamedSitesTest(unittest.TestCase):
    def setUp(self) -> None:
        self.section = implementer_section(ROLES.read_text(encoding="utf-8"))

    def test_text_before_the_first_policy_sentence_is_only_the_intro(self) -> None:
        self.assertEqual(INTRO, self.section[: self.section.index(SENTENCES[0])].strip())

    def test_policy_sentences_are_exact_and_in_order(self) -> None:
        found = policy_sentences(self.section)
        self.assertEqual(list(SENTENCES), found)
        for got, expected in zip(found, SENTENCES):
            self.assertEqual(expected, got)

    def test_negated_and_reordered_failures_are_absent(self) -> None:
        for bad in FORBIDDEN:
            self.assertNotIn(bad, self.section)
        self.assertIn("instead of surveying the rest of the tree first.", self.section)
        self.assertIn(
            "When the brief has neither an exact edit nor those named sites,",
            self.section,
        )

    def test_further_read_is_only_a_missing_symbol(self) -> None:
        self.assertEqual(
            "Read another file only when a named symbol is missing.",
            policy_sentences(self.section)[2],
        )

    def test_tdd_and_ponytail_stay_before_the_brief_shapes(self) -> None:
        self.assertIn("using TDD and Ponytail", self.section)
        self.assertLess(
            self.section.index("using TDD and Ponytail"),
            self.section.index(SENTENCES[0]),
        )


if __name__ == "__main__":
    unittest.main()
