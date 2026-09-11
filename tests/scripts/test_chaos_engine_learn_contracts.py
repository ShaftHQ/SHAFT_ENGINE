"""#5723: work-item A1, PR-merger A2, portable learn/design/research contracts."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
LEVEL1 = ROOT / "chaos-engine/references/level-1-catalog.md"
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
WORK_ITEM = ROOT / "chaos-engine/skills/work-item/SKILL.md"
CONTRACTS = (
    ROOT / "chaos-engine/references/harness-learn.md",
    ROOT / "chaos-engine/references/learn-traces.md",
    ROOT / "chaos-engine/references/design-loop.md",
    ROOT / "chaos-engine/references/deep-research.md",
)


def catalog_rows(skill_text: str) -> dict[str, str]:
    section = re.search(r"(?ms)^## Catalog\n(.*?)(?=^## |\Z)", skill_text)
    if section is None:
        raise AssertionError("router missing ## Catalog")
    rows: dict[str, str] = {}
    for line in section.group(1).splitlines():
        if not line.startswith("|") or re.match(r"^\|\s*name\s*\|", line, re.I):
            continue
        if line.startswith("| ---"):
            continue
        parts = [part.strip() for part in line.strip().strip("|").split("|")]
        if len(parts) >= 3 and parts[0] not in {"name", ""}:
            rows[parts[0]] = parts[2].strip("`")
    return rows


class LearnContractTests(unittest.TestCase):
    def test_a1_work_item_one_issue_named_pr(self):
        text = WORK_ITEM.read_text(encoding="utf-8")
        self.assertIn("One GitHub issue tracks the full plan", text)
        self.assertIn("currently active PR", text)

    def test_a2_pr_merger_does_not_import_pr_babysit_never_merge(self):
        section = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn("When the owner says babysit and merge it when green", section)
        self.assertIn("Bundled `pr-babysit` forbids merge", section)
        self.assertIn("do not copy that rule into ChaosEngine", section)
        self.assertIn("do not edit the bundled skill in place", section)

    def test_a2_pr_merger_requires_epic_scope_before_auto_merge(self):
        playbook = PLAYBOOK.read_text(encoding="utf-8")
        planning = (ROOT / "chaos-engine/references/work-github-planning.md").read_text(
            encoding="utf-8"
        )
        skill = SKILL.read_text(encoding="utf-8")
        compact = re.sub(r"\s+", " ", playbook).casefold()
        self.assertIn("do not arm auto-merge while any in-scope sub-issue is open", compact)
        self.assertIn("initial scope", compact)
        self.assertIn("dropped scope", compact)
        self.assertIn("related sub-issue is merged", planning.casefold())
        self.assertIn("initial scope is complete", skill.casefold())

    def test_router_and_level1_discover_portable_contracts(self):
        rows = catalog_rows(SKILL.read_text(encoding="utf-8"))
        skill = SKILL.read_text(encoding="utf-8")
        level1 = LEVEL1.read_text(encoding="utf-8")
        for name, rel in (
            ("harness-learn", "references/harness-learn.md"),
            ("design-loop", "references/design-loop.md"),
            ("deep-research", "references/deep-research.md"),
            ("learn-traces", "references/learn-traces.md"),
        ):
            with self.subTest(name=name):
                self.assertIn(name, rows)
                self.assertEqual(rows[name], rel)
                self.assertTrue((ROOT / "chaos-engine" / rel).is_file())
                self.assertIn(rel.split("/")[-1], skill)
                self.assertIn(rel.split("/")[-1], level1)

    def test_contracts_forbid_home_skill_copies_and_host_tui_vendor(self):
        learn = CONTRACTS[0].read_text(encoding="utf-8")
        traces = CONTRACTS[1].read_text(encoding="utf-8")
        design = CONTRACTS[2].read_text(encoding="utf-8")
        research = CONTRACTS[3].read_text(encoding="utf-8")
        self.assertIn("`~/.grok/skills`", learn)
        self.assertIn("never recreate", learn.casefold())
        self.assertIn("Map", traces)
        self.assertIn("Reduce", traces)
        self.assertIn("Verify", traces)
        self.assertNotIn("learn-traces.rhai", traces)
        self.assertIn("0 open issues", design)
        self.assertIn("PR Plan", design)
        self.assertIn("Verify", research)
        self.assertIn("cited", research.casefold())
        self.assertIn("Do not install duplicates under `~/.grok/skills`", learn)


if __name__ == "__main__":
    unittest.main()
