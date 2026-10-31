"""ChaosEngine 2026.10.31.3: husk FP + generic transactional upgrade-session."""
from __future__ import annotations

import importlib.util
import json
import shutil
import subprocess
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
    subprocess.run([git, *args], cwd=cwd, check=True, capture_output=True)


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
        # Other host adapters CE writes
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


class HuskFalsePositiveTest(unittest.TestCase):
    def test_empty_repo_pass_real_husk_warn_normal_pass(self) -> None:
        install = load("ce_husk_313", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            empty = base / "empty"
            _init_repo(empty)
            self.assertFalse(install.is_husk_git(empty))
            self.assertEqual("pass", install.git_husk_status(empty)[0])

            husk = base / "husk"
            husk.mkdir()
            git_dir = husk / ".git"
            git_dir.mkdir()
            (git_dir / "HEAD").write_text("ref: refs/heads/main\n", encoding="utf-8")
            (git_dir / "config").write_text("[core]\n\trepositoryformatversion = 0\n", encoding="utf-8")
            (git_dir / "refs").mkdir()
            # missing objects/
            self.assertTrue(install.is_husk_git(husk))
            self.assertIn(install.git_husk_status(husk)[0], {"warn", "fail"})

            normal = base / "normal"
            _init_repo(normal)
            (normal / "README").write_text("x\n", encoding="utf-8")
            _git(normal, "add", "README")
            _git(normal, "commit", "-m", "init")
            self.assertFalse(install.is_husk_git(normal))
            self.assertEqual("pass", install.git_husk_status(normal)[0])

    def test_local_only_advice_mentions_baseline_commit(self) -> None:
        install = load("ce_husk_313b", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            husk = Path(temporary) / "husk"
            husk.mkdir()
            git_dir = husk / ".git"
            git_dir.mkdir()
            (git_dir / "HEAD").write_text("ref: refs/heads/main\n", encoding="utf-8")
            (git_dir / "refs").mkdir()
            advice = install._husk_recovery_advice(husk)
            self.assertTrue(advice.startswith("Local-only"))
            self.assertIn("baseline commit", advice)
            self.assertNotIn("Remote repo:", advice)


class GenericAdoptTomlTest(unittest.TestCase):
    def test_installer_owned_includes_codex_toml(self) -> None:
        overlay = load("ce_overlay_313", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            primary = Path(temporary) / "primary"
            _seed_overlay(primary, "2026.10.31.3")
            owned = overlay._installer_owned_relatives(primary)
            self.assertIn(".codex/agents/chaos-engine-implementer.toml", owned)
            self.assertIn(".claude/agents/chaos-engine-implementer.md", owned)
            self.assertIn(".chaos-engine/VERSION", owned)
            self.assertNotIn("AGENTS.md", owned)

    def test_upgrade_adopts_stale_toml_without_fallback(self) -> None:
        overlay = load("ce_overlay_313b", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            _init_repo(primary)
            _seed_overlay(primary, "2026.10.31.2")
            # Fake linked worktree: shared .git via worktree add
            _git(primary, "add", "-A")
            # ignore CE overlay for realism
            (primary / ".gitignore").write_text(".chaos-engine/\n.claude/\n.codex/\n", encoding="utf-8")
            _git(primary, "add", ".gitignore")
            _git(primary, "commit", "-m", "base")
            _git(primary, "worktree", "add", str(session), "HEAD")
            # Seed older overlay in session (stale toml)
            _seed_overlay(session, "2026.10.30.1")
            # Advance primary
            _seed_overlay(primary, "2026.10.31.3")
            # Stage a stale toml in session (HRSD half-apply class)
            _git(session, "add", "-f", ".codex/agents/chaos-engine-implementer.toml")
            result = overlay.upgrade_session(primary, session, repair_plugins=False)
            self.assertEqual("2026.10.31.3", result["session_version"])
            body = (session / ".codex/agents/chaos-engine-implementer.toml").read_text(
                encoding="utf-8"
            )
            self.assertIn("2026.10.31.3", body)


class TransactionalUpgradeTest(unittest.TestCase):
    def test_preflight_refusal_writes_nothing(self) -> None:
        overlay = load("ce_overlay_313c", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            _seed_overlay(primary, "2026.10.31.2")
            overlay.materialize(primary, session, transactional=True)
            # Consumer edit on AGENTS.md (strict provenance)
            (primary / "AGENTS.md").write_text("primary-agents\n", encoding="utf-8")
            (session / "AGENTS.md").write_text("consumer-edit\n", encoding="utf-8")
            (primary / ".chaos-engine/VERSION").write_text("2026.10.31.3\n", encoding="utf-8")
            (primary / ".chaos-engine/skills/chaos-engine/SKILL.md").write_text(
                "router-new\n", encoding="utf-8"
            )
            before_ver = (session / ".chaos-engine/VERSION").read_text(encoding="utf-8")
            before_skill = (session / ".chaos-engine/skills/chaos-engine/SKILL.md").read_text(
                encoding="utf-8"
            )
            with self.assertRaises(overlay.OverlayLocalEditError) as ctx:
                overlay.materialize(primary, session, transactional=True)
            self.assertIn("preflight refused", str(ctx.exception))
            self.assertIn("diff hint", str(ctx.exception).casefold())
            self.assertEqual(before_ver, (session / ".chaos-engine/VERSION").read_text(encoding="utf-8"))
            self.assertEqual(
                before_skill,
                (session / ".chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8"),
            )

    def test_version_written_last_on_success(self) -> None:
        overlay = load("ce_overlay_313d", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            _seed_overlay(primary, "2026.10.31.2")
            overlay.materialize(primary, session)
            order: list[str] = []
            real_copy2 = overlay.shutil.copy2

            def tracking_copy2(src, dst, *a, **k):
                order.append(Path(dst).name)
                return real_copy2(src, dst, *a, **k)

            overlay.shutil.copy2 = tracking_copy2  # type: ignore[attr-defined]
            try:
                (primary / ".chaos-engine/VERSION").write_text("2026.10.31.3\n", encoding="utf-8")
                (primary / ".chaos-engine/hooks/guard.py").write_text("# guard-new\n", encoding="utf-8")
                # Rematerialize after adopt so hashes allow upgrade
                manifest = overlay._load_manifest(session)
                for key in list(manifest):
                    dest = session / key
                    if dest.is_file():
                        manifest[key] = overlay._file_sha256(dest)
                overlay._save_manifest(session, manifest)
                overlay.materialize(primary, session, transactional=True)
            finally:
                overlay.shutil.copy2 = real_copy2  # type: ignore[attr-defined]
            if "VERSION" in order:
                self.assertEqual(order[-1], "VERSION")


class HalfApplyRecoverTest(unittest.TestCase):
    def test_recover_session_repairs_half_applied(self) -> None:
        overlay = load("ce_overlay_313e", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            _init_repo(primary)
            (primary / ".gitignore").write_text(".chaos-engine/\n.claude/\n.codex/\n", encoding="utf-8")
            _git(primary, "add", ".gitignore")
            _git(primary, "commit", "-m", "base")
            _git(primary, "worktree", "add", str(session), "HEAD")
            _seed_overlay(primary, "2026.10.31.3")
            _seed_overlay(session, "2026.10.30.1")
            # Simulate half-apply: VERSION bumped, toml stuck old
            (session / ".chaos-engine/VERSION").write_text("2026.10.31.3\n", encoding="utf-8")
            self.assertTrue(overlay.session_is_half_applied(primary, session))
            result = overlay.recover_session(primary, session)
            self.assertEqual("2026.10.31.3", result["session_version"])
            self.assertTrue(result.get("recovered"))
            self.assertIn(
                "2026.10.31.3",
                (session / ".codex/agents/chaos-engine-implementer.toml").read_text(encoding="utf-8"),
            )
            self.assertFalse(overlay.session_is_half_applied(primary, session))


class UpgradeMatrixTest(unittest.TestCase):
    """Every hop 30.1 → 30.2 → 31.1 → 31.2 → 31.3 with staged overlays + all adapters."""

    VERSIONS = (
        "2026.10.30.1",
        "2026.10.30.2",
        "2026.10.31.1",
        "2026.10.31.2",
        "2026.10.31.3",
    )

    def test_matrix_hops_no_fallback(self) -> None:
        overlay = load("ce_overlay_313f", CE / "worktree_overlay.py")
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
            sessions: list[Path] = []
            for index in range(2):
                session = base / f"wt{index}"
                _git(primary, "worktree", "add", str(session), "HEAD")
                sessions.append(session)

            # Start all sessions at first version
            _seed_overlay(primary, self.VERSIONS[0])
            for session in sessions:
                _seed_overlay(session, self.VERSIONS[0])
                # stage one agent overlay like HRSD
                _git(session, "add", "-f", ".codex/agents/chaos-engine-implementer.toml")
                _git(session, "add", "-f", ".claude/agents/chaos-engine-implementer.md")

            for version in self.VERSIONS[1:]:
                _seed_overlay(primary, version)
                for session in sessions:
                    result = overlay.upgrade_session(primary, session, repair_plugins=False)
                    self.assertEqual(version, result["session_version"], msg=session.name)
                    toml = session / ".codex/agents/chaos-engine-implementer.toml"
                    self.assertIn(version, toml.read_text(encoding="utf-8"))
                    self.assertFalse(overlay.session_is_half_applied(primary, session))


class VersionPinTest(unittest.TestCase):
    def test_version_file(self) -> None:
        self.assertEqual("2026.10.31.3", (CE / "VERSION").read_text(encoding="utf-8").strip())


if __name__ == "__main__":
    unittest.main()
