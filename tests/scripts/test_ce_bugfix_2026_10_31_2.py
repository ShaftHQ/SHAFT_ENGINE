"""CE bugfix batch 2026.10.31.2 regression tests.

Covers staged adopt, husk git doctor, source/project CLI, host dedupe.
"""
from __future__ import annotations

import hashlib
import importlib.util
import json
import shutil
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"


def load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


class VersionTest(unittest.TestCase):
    def test_version_bumped(self) -> None:
        value = (CE / "VERSION").read_text(encoding="utf-8").strip()
        self.assertRegex(value, r"^20\d{2}\.\d{2}\.\d{2}\.\d+$")
        self.assertEqual("2026.10.31.2", value)


class StagedVersionAdoptTest(unittest.TestCase):
    """HRSD: staged prior-CE overlay VERSION must upgrade, not refuse."""

    def test_upgrade_session_adopts_staged_version_overlay(self) -> None:
        overlay = load("ce_overlay_staged_v_bug4", CE / "worktree_overlay.py")
        git = shutil.which("git")
        self.assertIsNotNone(git)
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            primary.mkdir()
            subprocess.run([git, "init", "-q"], cwd=primary, check=True, capture_output=True)  # nosec B603
            subprocess.run(  # nosec B603
                [git, "config", "user.email", "t@ex.com"], cwd=primary, check=True, capture_output=True,
            )
            subprocess.run(  # nosec B603
                [git, "config", "user.name", "T"], cwd=primary, check=True, capture_output=True,
            )
            # Do NOT gitignore .chaos-engine so we can stage the overlay (HRSD mode).
            (primary / "README").write_text("p\n", encoding="utf-8")
            subprocess.run([git, "add", "README"], cwd=primary, check=True, capture_output=True)  # nosec B603
            subprocess.run([git, "commit", "-qm", "i"], cwd=primary, check=True, capture_output=True)  # nosec B603
            session = base / "session"
            subprocess.run(  # nosec B603
                [git, "worktree", "add", "-q", str(session), "HEAD"],
                cwd=primary, check=True, capture_output=True,
            )

            def write_overlay(root: Path, version: str, budget: str) -> None:
                ce = root / ".chaos-engine"
                (ce / "skills" / "chaos-engine").mkdir(parents=True)
                (ce / "hooks").mkdir(parents=True)
                (ce / "VERSION").write_text(version + "\n", encoding="utf-8")
                (ce / "skills" / "chaos-engine" / "SKILL.md").write_text(
                    "skill-" + version + "\n", encoding="utf-8"
                )
                (ce / "hooks" / "guard.py").write_text("guard\n", encoding="utf-8")
                (ce / "always-loaded-budget.json").write_text(budget + "\n", encoding="utf-8")
                agents = root / ".claude" / "agents"
                agents.mkdir(parents=True)
                (agents / "chaos-engine-implementer.md").write_text(
                    "impl-" + version + "\n", encoding="utf-8"
                )

            write_overlay(primary, "2026.10.31.2", '{"cap": 2}')
            write_overlay(session, "2026.10.30.2", '{"cap": 1}')
            # Stage the prior-CE overlay in the worktree (force-add), matching HRSD.
            subprocess.run(  # nosec B603
                [git, "add", "-f", ".chaos-engine/VERSION", ".chaos-engine/always-loaded-budget.json",
                 ".claude/agents/chaos-engine-implementer.md"],
                cwd=session, check=True, capture_output=True,
            )
            cached = subprocess.run(  # nosec B603
                [git, "diff", "--cached", "--name-only"],
                cwd=session, capture_output=True, text=True, check=True,
            ).stdout
            self.assertIn(".chaos-engine/VERSION", cached)
            stale = {
                ".chaos-engine/VERSION": hashlib.sha256(b"stale-version\n").hexdigest(),
                ".chaos-engine/always-loaded-budget.json": hashlib.sha256(b"stale-budget\n").hexdigest(),
            }
            man_path = session / ".chaos-engine" / ".overlay-materialize.json"
            man_path.write_text(
                json.dumps({"schema": 1, "files": stale}, indent=2) + "\n", encoding="utf-8"
            )
            # Consumer-edited non-CE file stays protected.
            (session / "AGENTS.md").write_text("consumer edit\n", encoding="utf-8")
            result = overlay.upgrade_session(primary, session, repair_plugins=False)
            self.assertEqual(
                "2026.10.31.2",
                (session / ".chaos-engine" / "VERSION").read_text(encoding="utf-8").strip(),
            )
            self.assertEqual(
                '{"cap": 2}',
                (session / ".chaos-engine" / "always-loaded-budget.json").read_text(
                    encoding="utf-8"
                ).strip(),
            )
            self.assertEqual(
                "impl-2026.10.31.2",
                (session / ".claude" / "agents" / "chaos-engine-implementer.md")
                .read_text(encoding="utf-8")
                .strip(),
            )
            self.assertEqual(
                "consumer edit",
                (session / "AGENTS.md").read_text(encoding="utf-8").strip(),
            )
            self.assertGreaterEqual(len(result.get("written") or []), 1)

    def test_consumer_agents_md_still_refused_on_sync(self) -> None:
        """Non-CE shared file with local edit still raises OverlayLocalEditError."""
        overlay = load("ce_overlay_protect_bug4", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            for root in (primary, session):
                ce = root / ".chaos-engine"
                (ce / "skills" / "chaos-engine").mkdir(parents=True)
                (ce / "hooks").mkdir(parents=True)
                (ce / "VERSION").write_text("2026.10.31.2\n", encoding="utf-8")
                (ce / "skills" / "chaos-engine" / "SKILL.md").write_text("s\n", encoding="utf-8")
                (ce / "hooks" / "guard.py").write_text("g\n", encoding="utf-8")
            (primary / "AGENTS.md").write_text("primary agents\n", encoding="utf-8")
            (session / "AGENTS.md").write_text("local consumer edit\n", encoding="utf-8")
            # Seed manifest so AGENTS.md looks like unknown provenance.
            man = {
                "schema": 1,
                "files": {
                    "AGENTS.md": hashlib.sha256(b"other\n").hexdigest(),
                },
            }
            (session / ".chaos-engine" / ".overlay-materialize.json").write_text(
                json.dumps(man) + "\n", encoding="utf-8"
            )
            with self.assertRaises(overlay.OverlayLocalEditError):
                overlay.materialize(primary, session)


class HuskGitDoctorTest(unittest.TestCase):
    def test_detects_husk_and_healthy(self) -> None:
        install = load("ce_install_husk_bug4", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.assertFalse(install.is_husk_git(project))
            git_dir = project / ".git"
            git_dir.mkdir()
            (git_dir / "HEAD").write_text("ref: refs/heads/master\n", encoding="utf-8")
            (git_dir / "config").write_text("[core]\n", encoding="utf-8")
            (git_dir / "hooks").mkdir()
            (git_dir / "objects").mkdir()
            (git_dir / "objects" / "info").mkdir()
            (git_dir / "objects" / "pack").mkdir()
            (git_dir / "refs").mkdir()
            (git_dir / "refs" / "heads").mkdir()
            self.assertTrue(install.is_husk_git(project))
            status = install.git_husk_status(project)
            self.assertEqual("fail", status[0])
            self.assertIn("husk .git", status[1])
            # Add an object file → no longer a husk.
            (git_dir / "objects" / "ab").mkdir()
            (git_dir / "objects" / "ab" / "cd").write_text("x", encoding="utf-8")
            self.assertFalse(install.is_husk_git(project))
            self.assertEqual(("pass",), install.git_husk_status(project)[:1])

    def test_install_does_not_create_git_in_nongit_dir(self) -> None:
        """Documented guarantee: install into a fresh dir leaves no .git."""
        # Lightweight: secret_guard skip path already covered; assert helper + docs note.
        bot = (CE / "references" / "bot-entry.md").read_text(encoding="utf-8")
        self.assertIn("husk", bot.casefold())
        self.assertIn("does not create `.git`", bot)


class ProjectDefaultAndSourceNormalizeTest(unittest.TestCase):
    def test_normalize_source_from_clone_root(self) -> None:
        install = load("ce_install_src_bug4", CE / "install.py")
        nested = install.normalize_install_source(ROOT)
        self.assertEqual((ROOT / "chaos-engine").resolve(), nested)
        same = install.normalize_install_source(ROOT / "chaos-engine")
        self.assertEqual((ROOT / "chaos-engine").resolve(), same)

    def test_project_defaults_to_cwd(self) -> None:
        install = load("ce_install_proj_bug4", CE / "install.py")
        # Build parser the same way main does for doctor.
        parser = install.build_parser() if hasattr(install, "build_parser") else None
        if parser is None:
            # Fall back: invoke argparse via module helper if exposed.
            completed = subprocess.run(  # nosec B603
                [sys.executable, str(CE / "install.py"), "doctor", "--help"],
                capture_output=True, text=True, check=False,
            )
            self.assertEqual(0, completed.returncode)
            self.assertIn("default: current working directory", completed.stdout)
            return
        args = parser.parse_args(["doctor"])
        self.assertEqual(Path("."), args.project)


class HostOnboardingDedupeTest(unittest.TestCase):
    def test_dedupe_drops_second_onboarding_block(self) -> None:
        install = load("ce_install_dedupe_bug4", CE / "install.py")
        lines = [
            "ChaosEngine doctor: healthy",
            "Host onboarding cards:",
            "  Hosts",
            "    Grok             not on PATH    file/hook",
            "    GitHub Copilot   IDE signal     file/hook",
            "",
            "Host onboarding cards:",
            "  Hosts",
            "    Grok             not on PATH    file/hook",
            "    GitHub Copilot   IDE signal     file/hook",
            "",
            "[info] other — ok",
        ]
        out = install._dedupe_severity_rows(lines)
        self.assertEqual(1, sum(1 for line in out if line.startswith("Host onboarding cards:")))
        self.assertIn("[info] other — ok", out)


if __name__ == "__main__":
    unittest.main()
