"""Portable ChaosEngine core and project-profile contract tests (#4792)."""

import json
import re
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
CORE = ROOT / "chaos-engine"
CANONICAL_SKILL = CORE / "skills/chaos-engine/SKILL.md"
REPOSITORY_ADAPTER = ROOT / ".agents/skills/chaos-engine/SKILL.md"
COMPATIBILITY_ALIAS = ROOT / ".agents/skills/act-as-mohab/SKILL.md"
SHAFT_PROFILE = CORE / "profiles/shaft/profile.json"
POSIX_ABSOLUTE_PATH = re.compile(
    r"(?:^|[\s`\"'(=])(/[A-Za-z0-9._~-]+(?:/[A-Za-z0-9._~*{}-]+)+)",
    re.MULTILINE,
)


class ChaosEnginePortableCoreTest(unittest.TestCase):
    def test_chaos_engine_is_the_canonical_skill_and_act_as_mohab_is_only_an_alias(self):
        canonical = CANONICAL_SKILL.read_text(encoding="utf-8")
        repository_adapter = REPOSITORY_ADAPTER.read_text(encoding="utf-8")
        alias = COMPATIBILITY_ALIAS.read_text(encoding="utf-8")

        self.assertRegex(canonical, r"(?m)^name: chaos-engine$")
        self.assertIn("../../../chaos-engine/skills/chaos-engine/SKILL.md", repository_adapter)
        self.assertIn("../../../chaos-engine/skills/chaos-engine/SKILL.md", alias)
        self.assertLessEqual(len(alias.splitlines()), 12, "compatibility alias must not duplicate policy")
        self.assertNotIn("## Iron laws", alias)

    def test_generic_core_has_no_shaft_or_machine_specific_paths(self):
        forbidden = {
            "ShaftHQ": re.compile(r"ShaftHQ", re.IGNORECASE),
            "SHAFT_ENGINE": re.compile(r"SHAFT_ENGINE", re.IGNORECASE),
            "user guide": re.compile(r"shafthq\.github\.io", re.IGNORECASE),
            "Windows absolute path": re.compile(r"(?<![A-Za-z0-9+.-])[A-Za-z]:[\\/]"),
            "POSIX absolute path": POSIX_ABSOLUTE_PATH,
            "hard-coded default branch": re.compile(r"origin/main", re.IGNORECASE),
        }
        sources = sorted(
            path
            for path in CORE.rglob("*")
            if path.is_file()
            and "profiles" not in path.relative_to(CORE).parts
            and "__pycache__" not in path.relative_to(CORE).parts
            and path.suffix != ".pyc"
        )
        self.assertTrue(sources, "portable core sources must exist")
        for path in sources:
            text = path.read_text(encoding="utf-8")
            for label, pattern in forbidden.items():
                with self.subTest(path=path.relative_to(ROOT), forbidden=label):
                    self.assertIsNone(pattern.search(text))

    def test_posix_absolute_path_guard_is_non_vacuous(self):
        self.assertRegex("cache at /opt/private/agent-cache", POSIX_ABSOLUTE_PATH)
        self.assertRegex("config at /root/.config/private-agent", POSIX_ABSOLUTE_PATH)
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("https://example.com/opt/tool"))
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("[role](../../references/roles.md)"))
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("run ./bin/act-as-mohab.pyz"))
        self.assertIsNone(POSIX_ABSOLUTE_PATH.search("/caveman full"))
        windows_absolute = re.compile(r"(?<![A-Za-z0-9+.-])[A-Za-z]:[\\/]")
        self.assertRegex(r"cache at C:\\private\\agent-cache", windows_absolute)
        self.assertIsNone(windows_absolute.search("https://example.com/tool"))

    def test_shaft_behavior_is_selected_by_a_project_profile(self):
        profile = json.loads(SHAFT_PROFILE.read_text(encoding="utf-8"))

        self.assertEqual(1, profile["schemaVersion"])
        self.assertEqual("shaft", profile["name"])
        self.assertEqual("ChaosEngine/", profile["taskBranchPrefix"])
        self.assertEqual("ShaftHQ/SHAFT_ENGINE", profile["repository"])
        self.assertEqual("ShaftHQ/shafthq.github.io", profile["companionRepositories"][0]["repository"])
        self.assertNotIn("localRoot", profile["companionRepositories"][0])

    def test_every_compatibility_alias_selects_the_repository_profile(self):
        for alias in (
            ROOT / ".agents/skills/act-as-mohab/SKILL.md",
            ROOT / ".claude/skills/act-as-mohab/SKILL.md",
        ):
            with self.subTest(alias=alias.relative_to(ROOT)):
                content = alias.read_text(encoding="utf-8")
                links = re.findall(r"\[[^]]+\]\(([^)]+)\)", content)
                self.assertIn(REPOSITORY_ADAPTER.resolve(), {(alias.parent / link).resolve() for link in links})

    def test_portable_contract_is_scanned_and_run_by_pull_request_ci(self):
        budget = json.loads(
            (ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        for key in ("active_guidance_globs", "total_guidance_globs", "reference_scan_globs"):
            self.assertIn("chaos-engine/references/*.md", budget[key], key)
        self.assertIn(
            "tests/scripts/test_chaos_engine_portable_core.py",
            budget["harness_reachability"]["element_globs"],
        )
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        self.assertIn("tests.scripts.test_chaos_engine_portable_core", workflow)


if __name__ == "__main__":
    unittest.main()
