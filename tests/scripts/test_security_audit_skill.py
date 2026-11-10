"""Own-repo security audit skill stays off the always-loaded path."""
from __future__ import annotations

import importlib.util
import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SKILL = ROOT / "chaos-engine/skills/security-audit/SKILL.md"
VERIFY = ROOT / "chaos-engine/skills/security-audit/verify.py"
BUDGET = ROOT / "scripts/ci/agent_guidance_budget.json"
ROUTER = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"


def load_verify():
    spec = importlib.util.spec_from_file_location("security_audit_verify", VERIFY)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def folded_description(text: str) -> str:
    match = re.search(r"^description:\s*>-\n((?:[ \t]+.+\n)+)", text, re.M)
    if match is None:
        raise AssertionError("frontmatter description is missing")
    return " ".join(line.strip() for line in match.group(1).splitlines())


def listed_paths(node) -> list[str]:
    found: list[str] = []
    if isinstance(node, str):
        found.append(node)
    elif isinstance(node, dict):
        for value in node.values():
            found.extend(listed_paths(value))
    elif isinstance(node, list):
        for value in node:
            found.extend(listed_paths(value))
    return found


class SecurityAuditSkillTest(unittest.TestCase):
    def test_classify_requires_owned_repo_and_independent_pass(self) -> None:
        classify = load_verify().classify
        self.assertEqual("stop", classify(False, False, True))
        self.assertEqual("unconfirmed", classify(True, True, True))
        self.assertEqual("unconfirmed", classify(True, False, False))
        self.assertEqual("confirmed", classify(True, False, True))

    def test_skill_states_scope_and_refuses_exploit_procedures(self) -> None:
        text = SKILL.read_text(encoding="utf-8")
        self.assertTrue(
            "repositories we own" in text or "repository we own" in text
        )
        self.assertIn("second pass", text)
        self.assertIn("Do not include exploit procedures", text)
        self.assertLessEqual(len(folded_description(text)), 220)

    def test_skill_is_not_on_the_lean_chain_or_router(self) -> None:
        budget = json.loads(BUDGET.read_text(encoding="utf-8"))
        paths = listed_paths(budget["lean_chain"])
        self.assertTrue(paths)
        self.assertFalse(any("security-audit" in path for path in paths))
        self.assertNotIn(
            "skills/security-audit/SKILL.md",
            ROUTER.read_text(encoding="utf-8"),
        )


if __name__ == "__main__":
    unittest.main()
