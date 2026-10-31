"""ChaosEngine 2026.10.31.5: retro cards + version-hop matrix through .5."""
from __future__ import annotations

import importlib.util
import json
import shutil
import subprocess  # nosec B404
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _git(cwd: Path, *args: str) -> None:
    git = shutil.which("git")
    if git is None:
        raise RuntimeError("git not found on PATH")
    subprocess.run(  # nosec B603 B607
        [git, *args], cwd=cwd, check=True, capture_output=True
    )


def _init_repo(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)
    _git(path, "init")
    _git(path, "config", "user.email", "ce@test")
    _git(path, "config", "user.name", "CE Test")


def _seed_overlay(root: Path, version: str, *, extra_agents: bool = True) -> None:
    router = root / ".chaos-engine/skills/chaos-engine/SKILL.md"
    router.parent.mkdir(parents=True, exist_ok=True)
    router.write_text(f"router-{version}\n", encoding="utf-8")
    (root / ".chaos-engine/hooks").mkdir(parents=True, exist_ok=True)
    (root / ".chaos-engine/hooks/guard.py").write_text(f"# guard-{version}\n", encoding="utf-8")
    (root / ".chaos-engine/VERSION").write_text(version + "\n", encoding="utf-8")
    (root / ".chaos-engine/always-loaded-budget.json").write_text(
        json.dumps({"version": version}) + "\n", encoding="utf-8"
    )
    if extra_agents:
        for host, suffix in ((".claude/agents", ".md"), (".codex/agents", ".toml")):
            agents = root / host
            agents.mkdir(parents=True, exist_ok=True)
            for role in (
                "orchestrator",
                "implementer",
                "reviewer",
                "tester",
                "mechanical-helper",
            ):
                (agents / f"chaos-engine-{role}{suffix}").write_text(
                    f"{role}-{version}{suffix}\n", encoding="utf-8"
                )
        for rel, body in (
            (".claude/settings.json", '{"version":"%s"}' % version),
            (".codex/config.toml", f'model = "{version}"\n'),
            (".codex/hooks.json", '{"version":"%s"}' % version),
            (".gemini/settings.json", '{"version":"%s"}' % version),
            (".github/hooks/chaos-engine.json", '{"version":"%s"}' % version),
            (".claude/skills/chaos-engine/SKILL.md", f"claude-skill-{version}\n"),
            (".gemini/skills/chaos-engine/SKILL.md", f"gemini-skill-{version}\n"),
            (".github/skills/chaos-engine/SKILL.md", f"copilot-skill-{version}\n"),
            (".agents/skills/chaos-engine/SKILL.md", f"agents-skill-{version}\n"),
            (".grok/hooks/lifecycle.json", '{"version":"%s"}' % version),
        ):
            path = root / rel
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(body + ("\n" if not body.endswith("\n") else ""), encoding="utf-8")


class RetroRefsTest(unittest.TestCase):
    def test_refs_exist(self) -> None:
        for name in (
            "fix-bug-class.md",
            "explicit-staging.md",
            "profile-slow-installs.md",
            "gemini-quota-wait.md",
            "appium-driver-npm-recover.md",
        ):
            self.assertTrue((CE / "references" / name).is_file(), name)

    def test_bot_entry_staging(self) -> None:
        text = (CE / "references/bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("set -e", text)
        self.assertIn("git commit -a", text)

    def test_profile_installs_helper(self) -> None:
        helper = load("ce_profile_installs_315", CE / "profile_installs.py")
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "receipt.json"
            path.write_text(
                json.dumps({"session": "/workspace/wt-a", "elapsed_s": 42.5, "status": "ok"}),
                encoding="utf-8",
            )
            rows = helper.load_rows(path)
            self.assertEqual(1, len(rows))
            self.assertEqual(42.5, rows[0]["elapsed_s"])


class UpgradeMatrixThroughFiveTest(unittest.TestCase):
    VERSIONS = (
        "2026.10.31.3",
        "2026.10.31.4",
        "2026.10.31.5",
    )

    def test_matrix_hops_doctor_ok(self) -> None:
        overlay = load("ce_matrix_315", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            _init_repo(primary)
            (primary / ".gitignore").write_text(
                "\n".join(
                    [
                        ".chaos-engine/",
                        ".claude/",
                        ".codex/",
                        ".gemini/",
                        ".grok/",
                        ".agents/",
                        ".github/hooks/",
                        ".github/skills/",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            _git(primary, "add", ".gitignore")
            _git(primary, "commit", "-m", "base")
            session = base / "wt0"
            _git(primary, "worktree", "add", str(session), "HEAD")
            _seed_overlay(primary, self.VERSIONS[0])
            _seed_overlay(session, self.VERSIONS[0])
            _git(session, "add", "-f", ".codex/agents/chaos-engine-implementer.toml")
            _git(session, "add", "-f", ".claude/agents/chaos-engine-implementer.md")
            for version in self.VERSIONS[1:]:
                _seed_overlay(primary, version)
                result = overlay.upgrade_session(primary, session, repair_plugins=False)
                self.assertEqual(version, result["session_version"])
                self.assertTrue(
                    result.get("doctor_ok"),
                    msg=f"@{version}: {result.get('post_verify')}",
                )
                self.assertEqual("ok", result.get("status"))
                self.assertFalse(overlay.session_is_half_applied(primary, session))


class VersionPinTest(unittest.TestCase):
    def test_version_file(self) -> None:
        value = (CE / "VERSION").read_text(encoding="utf-8").strip()
        self.assertRegex(value, r"^20\d{2}\.\d{2}\.\d{2}\.\d+$")
        floor = tuple(int(p) for p in "2026.10.31.5".split("."))
        self.assertGreaterEqual(tuple(int(p) for p in value.split(".")), floor)


if __name__ == "__main__":
    unittest.main()
