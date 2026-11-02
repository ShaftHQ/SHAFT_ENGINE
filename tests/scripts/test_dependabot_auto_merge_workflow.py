"""Contract: Dependabot auto-merge every real gh call names -R/$repo."""

from __future__ import annotations

import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "dependabot-auto-merge.yml"
REFERENCE = ROOT / "chaos-engine" / "references" / "automerge-rearm.md"


def _real_gh_lines(text: str) -> list[str]:
    """Return shell lines that invoke gh (not comment / warning echo text)."""
    lines: list[str] = []
    for raw in text.splitlines():
        stripped = raw.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if stripped.startswith("echo ") or stripped.startswith("::"):
            continue
        if "gh " not in stripped:
            continue
        if re.search(r"\$\(gh\s", stripped) or re.search(r"\bgh\s+pr\s+", stripped):
            lines.append(stripped)
    return lines


class DependabotAutoMergeWorkflowTest(unittest.TestCase):
    def test_schedule_rearm_passes_repo_flag_to_gh_pr_list(self) -> None:
        text = WORKFLOW.read_text(encoding="utf-8")
        self.assertIn("gh pr list -R", text)
        self.assertIn("GITHUB_REPOSITORY: ${{ github.repository }}", text)
        self.assertIn('repo="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is required}"', text)
        self.assertNotIn("actions/checkout", text)

    def test_every_real_gh_invocation_passes_repo_flag(self) -> None:
        """L74/#7672 residual: strip -R from merge/view must turn RED, not only list."""
        text = WORKFLOW.read_text(encoding="utf-8")
        gh_lines = _real_gh_lines(text)
        self.assertGreaterEqual(len(gh_lines), 5, gh_lines)
        for line in gh_lines:
            self.assertRegex(
                line,
                r'(-R|--repo)\s+"\$repo"',
                msg=f'gh call missing -R/"$repo": {line}',
            )

    def test_automerge_rearm_reference_requires_repo_flag(self) -> None:
        text = REFERENCE.read_text(encoding="utf-8")
        self.assertIn("-R", text)
        self.assertIn("GITHUB_REPOSITORY", text)
        self.assertIn("empty list", text.lower())


class ChaosEngineVersionFloorTest(unittest.TestCase):
    def test_version_at_least_2026_10_31_12(self) -> None:
        version = (ROOT / "chaos-engine" / "VERSION").read_text(encoding="utf-8").strip()
        import importlib.util as u
        spec = u.spec_from_file_location("ce_version", ROOT / "scripts/ci/ce_version.py")
        mod = u.module_from_spec(spec)
        spec.loader.exec_module(mod)
        self.assertGreaterEqual(mod.version_key(version), mod.version_key("2026.10.31.12"))


if __name__ == "__main__":
    unittest.main()
