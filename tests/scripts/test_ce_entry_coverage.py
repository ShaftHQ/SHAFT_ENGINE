"""Entry-point coverage for ChaosEngine rules/hooks/skills and shaft-skills.

Directive on #7731: every rule, hook, skill, CLI, MCP, role, workflow and
memory card must stay reachable from the single ChaosEngine entry used across
platforms, and a unit gate must fail when anything under chaos-engine/ or
shaft-skills/ is unreachable — or when a required hook event is declared but
not wired on a hook-capable host config.

Coverage snapshot for this PR (expanded element_globs; orphans = uncovered):

- BEFORE linking (globs only): 916 elements, 44 orphans, 95.20% covered;
  new PR artifacts 2/14 covered.
- AFTER bot-entry (portable-safe) + agents README specialist inventory: 917 elements, 0 orphans, 100% covered;
  new PR artifacts 14/14 covered.
"""

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path

_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_ROOT))
sys.path.insert(0, str(_ROOT / "chaos-engine"))

from scripts.ci.harness_reachability import harness_report  # noqa: E402
import hosts  # noqa: E402

ROOT = _ROOT

# Platforms that ship lifecycle hooks through hooks/launch.py (not instruction-only).
HOOK_CAPABLE_CONFIGS = (
    (".claude/settings.json", "claude"),
    (".codex/hooks.json", "codex"),
    (".grok/hooks/lifecycle.json", "grok"),
)


def _covered(report: dict) -> set[str]:
    return set(report["by_link"]) | set(report["by_exact_token"]) | set(report["reached"])


class EntryCoverageTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.report = harness_report(ROOT)
        cls.covered = _covered(cls.report)

    def test_no_harness_orphans(self) -> None:
        self.assertEqual(self.report["orphans"], [], self.report["orphans"][:20])
        self.assertEqual(self.report["broken_links"], [])
        self.assertEqual(self.report["exemption_problems"], [])

    def test_full_element_coverage_pct(self) -> None:
        elements = self.report["elements"]
        orphans = self.report["orphans"]
        pct = 100.0 * (len(elements) - len(orphans)) / max(len(elements), 1)
        self.assertEqual(len(orphans), 0)
        self.assertGreaterEqual(pct, 100.0)
        # Floor: refuse silent inventory shrink (917 observed on #7731 tip; margin 17).
        self.assertGreaterEqual(len(elements), 900, len(elements))

    def test_every_chaos_engine_rule_is_reachable(self) -> None:
        rules = sorted(p.relative_to(ROOT).as_posix() for p in (ROOT / "chaos-engine/rules").glob("*.py"))
        self.assertTrue(rules)
        missing = [p for p in rules if p not in self.covered]
        self.assertEqual(missing, [])

    def test_every_hook_controller_is_reachable(self) -> None:
        missing = []
        for name in hosts.HOOK_CONTROLLER_NAMES:
            rel = f"chaos-engine/hooks/{name}"
            if not (ROOT / rel).exists():
                continue
            if rel not in self.covered:
                missing.append(rel)
        self.assertEqual(missing, [])

    def test_every_chaos_engine_skill_is_reachable(self) -> None:
        skills = sorted(
            p.relative_to(ROOT).as_posix()
            for p in (ROOT / "chaos-engine/skills").glob("*/SKILL.md")
        )
        self.assertTrue(skills)
        missing = [p for p in skills if p not in self.covered]
        self.assertEqual(missing, [])

    def test_every_shaft_skill_is_reachable(self) -> None:
        skills = sorted(
            p.relative_to(ROOT).as_posix()
            for p in (ROOT / "shaft-skills").glob("*/SKILL.md")
        )
        self.assertGreaterEqual(len(skills), 30)
        missing = [p for p in skills if p not in self.covered]
        self.assertEqual(missing, [])

    PR_ADDED_PATHS = (
        "chaos-engine/rules/lessons_2026_11_02.py",
        "chaos-engine/rules/lessons_repeat_failure_pdca.py",
        "chaos-engine/references/ce-lessons-2026-11-02-SPEC.md",
        "chaos-engine/references/ce-repeat-failure-pdca-SPEC.md",
        "shaft-skills/INVENTORY.md",
        "shaft-skills/shaft-mobile-env/SKILL.md",
        "shaft-skills/shaft-browserstack-native/SKILL.md",
        "shaft-skills/shaft-ios-ipa-acceptance/SKILL.md",
        "shaft-skills/shaft-android-apk-select/SKILL.md",
        "shaft-skills/shaft-docs-hygiene/SKILL.md",
        "shaft-skills/shaft-mobile-retest-hygiene/SKILL.md",
        "scripts/mobile/android_apk_select.py",
        "scripts/mobile/ios_ipa_acceptance.py",
        "scripts/ci/merge_queue_awaiting_checks_heal.py",
        "scripts/ci/ce_version.py",
        "scripts/ci/flutter_emulator_preflight.sh",
        "scripts/ci/run_ubuntu_flutter_emulator_e2e.sh",
    )

    def test_new_pr_tools_are_reachable(self) -> None:
        missing = [p for p in self.PR_ADDED_PATHS if p not in self.covered]
        self.assertEqual(missing, [])

    def test_pr_paths_have_route_or_playbook_link(self) -> None:
        """PR-added paths must not be covered only via exact-token inventory."""
        by_link = set(self.report["by_link"]) | set(self.report["reached"])
        by_tok = set(self.report["by_exact_token"])
        token_only = [
            p for p in self.PR_ADDED_PATHS
            if p in by_tok and p not in by_link
        ]
        self.assertEqual(
            token_only,
            [],
            "add a bot-entry/ChaosEngine route or playbook markdown link; "
            f"token-only paths: {token_only}",
        )

    def test_bigger_picture_first_is_the_first_bot_entry_step(self) -> None:
        """The step-back rule is linked from both entries, ahead of retrieve."""
        path = "chaos-engine/references/bigger-picture-first.md"
        self.assertIn(path, self.report["by_link"])
        for name in ("cli", "bot"):
            self.assertIn(path, self.report["entry_reach"][name], name)
        bot = (ROOT / "chaos-engine/references/bot-entry.md").read_text(encoding="utf-8")
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        section = bot.split("## Every task, before discovery", 1)[1].split("\n## ", 1)[0]
        first = section.split("\n2. ", 1)[0]
        self.assertIn("bigger-picture-first.md", first)
        self.assertIn("goal", first)
        self.assertIn("local hotfix", first)
        self.assertLess(first.find("bigger-picture-first.md"), section.find("tool.py retrieve"))
        self.assertIn("## Bigger picture first", skill)
        self.assertIn("../../references/bigger-picture-first.md", skill)
        self.assertIn("what a local hotfix", skill.split("## Triage", 1)[0])
        worker = (
            ROOT / "chaos-engine/skills/local-agency/scripts/worker_ce_entry.py"
        ).read_text(encoding="utf-8")
        self.assertNotIn("bigger-picture", worker)
        self.assertNotIn("bigger_picture", worker)

    def test_required_hook_events_wired_on_each_platform(self) -> None:
        # Host hook configs under .claude/.codex/.grok are gitignored install
        # artifacts; assert the generated lifecycle documents instead.
        required = set(hosts.REQUIRED_HOOK_EVENTS)
        problems: list[str] = []
        for host in ("claude", "codex", "grok"):
            blob = hosts.lifecycle_hooks_document(host).decode("utf-8")
            doc = json.loads(blob)
            present = set(doc.get("hooks") or {})
            expect = set(hosts.CLAUDE_HOOK_EVENTS) if host == "claude" else required
            missing = sorted(expect - present)
            if missing:
                problems.append(f"{host} lifecycle missing {missing}")
            if f"launch.py {host}" not in blob and f"launch.py','{host}" not in blob:
                problems.append(f"{host} lifecycle does not invoke hooks/launch.py")
        self.assertEqual(problems, [])


if __name__ == "__main__":
    unittest.main()
