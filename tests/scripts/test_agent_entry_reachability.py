"""Both real entries (CLI pointers + SessionStart cards, bot `tool.py entry`) reach every harness file."""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts" / "ci"))

import harness_reachability as reach  # noqa: E402

CARDS = ("identity.md", "skills/chaos-engine/SKILL.md")


def fixture(root: Path, agents_links: str = "", bot_links: str = "") -> None:
    ce = root / "chaos-engine"
    (ce / "skills/chaos-engine").mkdir(parents=True)
    (ce / "references").mkdir()
    (ce / "tool.py").write_text(f"ENTRY_FILES = {CARDS!r}\n", encoding="utf-8")
    (ce / "identity.md").write_text("# id\n", encoding="utf-8")
    (ce / "skills/chaos-engine/SKILL.md").write_text("[a](../../references/a.md)\n", encoding="utf-8")
    (ce / "references/a.md").write_text("# a\n", encoding="utf-8")
    (ce / "references/only.md").write_text("# only\n", encoding="utf-8")
    (ce / "references/bot-entry.md").write_text(bot_links + "\n", encoding="utf-8")
    (root / "AGENTS.md").write_text("[core](.chaos-engine/skills/chaos-engine/SKILL.md)\n" + agents_links, encoding="utf-8")
    (root / "CLAUDE.md").write_text("@AGENTS.md\n", encoding="utf-8")


class EntryReachTest(unittest.TestCase):
    def reach(self, **links: str) -> dict[str, set[str]]:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp).resolve()
            fixture(root, **links)
            return reach.entry_reach(root, "chaos-engine/")

    def test_snapshot_link_is_walked_through_its_source_twin(self) -> None:
        self.assertIn("chaos-engine/references/a.md", self.reach()["cli"])

    def test_cli_only_link_is_one_sided(self) -> None:
        got = self.reach(agents_links="[o](chaos-engine/references/only.md)\n")
        self.assertIn("chaos-engine/references/only.md", got["cli"])
        self.assertNotIn("chaos-engine/references/only.md", got["bot"])

    def test_bot_only_link_is_one_sided(self) -> None:
        got = self.reach(bot_links="[o](only.md)")
        self.assertIn("chaos-engine/references/only.md", got["bot"])
        self.assertNotIn("chaos-engine/references/only.md", got["cli"])


class LiveTreeTest(unittest.TestCase):
    def test_every_harness_file_is_reached_from_both_entries(self) -> None:
        report = reach.harness_report(ROOT)
        self.assertEqual([], [o for o in report["orphans"] if "entry)" in o])
        cli, bot = (set(report["entry_reach"][name]) for name in ("cli", "bot"))
        deployable = {e for e in report["elements"] if e.startswith("chaos-engine/")}
        self.assertEqual(set(), deployable - cli)
        self.assertEqual(set(), deployable - bot)

    def test_bot_and_cli_deliver_the_same_cards_from_one_source(self) -> None:
        roots = reach.entry_roots(ROOT, "chaos-engine/")
        self.assertEqual(roots["cli"][len(reach.CLI_POINTERS):], roots["bot"][1:])
        sys.path.insert(0, str(ROOT / "chaos-engine" / "hooks"))
        import lifecycle

        cards = {f"chaos-engine/{card}" for card in lifecycle.COMPANION_CARDS.values()}
        self.assertTrue(cards <= set(roots["bot"]))


if __name__ == "__main__":
    unittest.main()
