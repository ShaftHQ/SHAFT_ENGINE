"""ChaosEngine 2026.10.31.4: upgrade post-verify + doctor all-failures + fix-next map."""
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


class FixNextRepairMapTest(unittest.TestCase):
    def test_owned_excludes_maps_to_mempalace(self) -> None:
        install = load("ce_fixnext_314", CE / "install.py")
        overlay = load("ce_overlay_fix_314", CE / "overlay_match.py")
        self.assertEqual(
            "mempalace",
            install.doctor_check_repair_component("mempalace-owned-excludes"),
        )
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(
                "version: 1.0\nexclude_patterns:\n  - 'user-only/**'\n",
                encoding="utf-8",
            )
            result = {"status": "healthy", "components": {}}
            overlay.apply_mempalace_owned_excludes_doctor(result, project)
            row = result["components"]["mempalace-owned-excludes"]
            fix = row["fixNext"]
            self.assertIn("--component mempalace", fix)
            self.assertNotIn("retrieval-config", fix)
            self.assertNotIn("mempalace-owned-excludes", fix.split("--component", 1)[-1])

    def test_every_fix_next_component_parses(self) -> None:
        install = load("ce_fixnext_parse_314", CE / "install.py")
        samples = [
            ("hosts", {"status": "recovery-required", "code": "CE_HOST_ADAPTER_DRIFT"}),
            ("mempalace", {"status": "recovery-required"}),
            ("graphify", {"status": "degraded"}),
            ("mempalace-owned-excludes", {"status": "degraded", "reason": "owned-excludes-missing"}),
            ("mempalace-backend", {"status": "recovery-required", "reason": "backend-mismatch"}),
            ("retrieve-mempalace", {"status": "recovery-required"}),
            ("plugins", {"status": "recovery-required"}),
            ("policy-overlay", {"status": "recovery-required"}),
            ("hooks", {"status": "recovery-required"}),
            ("skills", {"status": "recovery-required"}),
            ("roles", {"status": "recovery-required"}),
            ("tools", {"status": "recovery-required"}),
            ("mcps", {"status": "broken"}),
            ("core", {"status": "recovery-required", "code": "CE_CORE_MISSING"}),
            ("memory", {"status": "recovery-required"}),
            ("maven-tools-mcp", {"status": "invalid"}),
        ]
        for name, item in samples:
            fix = install.component_fix_next(name, item)
            self.assertIsNotNone(fix, msg=name)
            for token in install.extract_repair_components_from_text(fix or ""):
                mapped = install.DOCTOR_CHECK_REPAIR_MAP.get(token, token)
                self.assertIn(
                    mapped,
                    install.REPAIRABLE_COMPONENTS,
                    msg=f"{name} fixNext has invalid --component {token} (mapped {mapped}): {fix}",
                )
            choice = install.doctor_check_repair_component(name, item)
            if choice is not None:
                self.assertIn(choice, install.REPAIRABLE_COMPONENTS)


class AgentSummaryAllFailuresTest(unittest.TestCase):
    def test_lists_every_failing_component(self) -> None:
        install = load("ce_summary_314", CE / "install.py")
        document = {
            "status": "recovery-required",
            "policySha256": "a" * 64,
            "components": {
                "hosts": {
                    "status": "recovery-required",
                    "code": "CE_HOST_ADAPTER_DRIFT",
                    "detail": "host adapter drift",
                },
                "mempalace-owned-excludes": {
                    "status": "degraded",
                    "reason": "owned-excludes-missing",
                },
                "core": {"status": "healthy"},
            },
        }
        rendered = install.format_agent_summary(document)
        self.assertIn("hosts recovery-required", rendered)
        self.assertIn("mempalace-owned-excludes degraded", rendered)
        self.assertTrue(rendered.startswith("doctor: fail"))
        self.assertLessEqual(len(rendered.splitlines()), install.AGENT_SUMMARY_MAX_LINES)


class BackendDoesNotMaskOwnedExcludesTest(unittest.TestCase):
    def test_backend_mismatch_still_flags_owned_excludes(self) -> None:
        overlay = load("ce_backend_mask_314", CE / "overlay_match.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / "mempalace.yaml").write_text(
                "version: 1.0\nexclude_patterns:\n  - 'user-only/**'\n",
                encoding="utf-8",
            )
            (project / ".mcp.json").write_text(
                json.dumps(
                    {
                        "mcpServers": {
                            "mempalace": {
                                "command": "npx",
                                "args": ["--backend", "chroma"],
                            }
                        }
                    }
                ),
                encoding="utf-8",
            )
            result = {"status": "healthy", "components": {}}
            overlay.apply_mempalace_backend_doctor(result, project)
            self.assertIn("mempalace-backend", result["components"])
            self.assertIn("mempalace-owned-excludes", result["components"])


class UpgradePostVerifyTest(unittest.TestCase):
    def test_false_success_rejected_when_half_applied_after_light_repair(self) -> None:
        overlay = load("ce_post_verify_314", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            _init_repo(primary)
            (primary / ".gitignore").write_text(".chaos-engine/\n.claude/\n.codex/\n", encoding="utf-8")
            _git(primary, "add", ".gitignore")
            _git(primary, "commit", "-m", "base")
            _git(primary, "worktree", "add", str(session), "HEAD")
            _seed_overlay(primary, "2026.10.31.4")
            _seed_overlay(session, "2026.10.31.3")
            # Force a stuck host adapter after VERSION bump to simulate light-repair false success
            # without going through materialize adopt: corrupt post-materialize by rewriting toml.
            result = overlay.upgrade_session(primary, session, repair_plugins=False)
            self.assertEqual("2026.10.31.4", result["session_version"])
            self.assertTrue(result.get("doctor_ok"), msg=result.get("post_verify"))
            self.assertEqual("ok", result.get("status"))
            # Corrupt after success: recover_session must not claim ok while half-applied.
            (session / ".codex/agents/chaos-engine-implementer.toml").write_text(
                "stuck-old\n", encoding="utf-8"
            )
            # VERSION already matches → half-applied
            self.assertTrue(overlay.session_is_half_applied(primary, session))
            verify = overlay._post_upgrade_repair_and_verify(primary, session)
            self.assertFalse(verify["doctor_ok"])
            self.assertIn("half-applied", verify["remaining"])


class UpgradeMatrixDoctorOkTest(unittest.TestCase):
    """Every hop with linked worktrees + all host adapters; doctor_ok after each."""

    VERSIONS = (
        "2026.10.30.1",
        "2026.10.30.2",
        "2026.10.31.1",
        "2026.10.31.2",
        "2026.10.31.3",
        "2026.10.31.4",
    )

    def test_matrix_hops_doctor_ok(self) -> None:
        overlay = load("ce_matrix_314", CE / "worktree_overlay.py")
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

            _seed_overlay(primary, self.VERSIONS[0])
            for session in sessions:
                _seed_overlay(session, self.VERSIONS[0])
                _git(session, "add", "-f", ".codex/agents/chaos-engine-implementer.toml")
                _git(session, "add", "-f", ".claude/agents/chaos-engine-implementer.md")

            for version in self.VERSIONS[1:]:
                _seed_overlay(primary, version)
                for session in sessions:
                    result = overlay.upgrade_session(primary, session, repair_plugins=False)
                    self.assertEqual(version, result["session_version"], msg=session.name)
                    self.assertTrue(
                        result.get("doctor_ok"),
                        msg=f"{session.name}@{version}: {result.get('post_verify')}",
                    )
                    self.assertEqual("ok", result.get("status"))
                    self.assertFalse(overlay.session_is_half_applied(primary, session))
                    toml = session / ".codex/agents/chaos-engine-implementer.toml"
                    self.assertIn(version, toml.read_text(encoding="utf-8"))


class VersionPinTest(unittest.TestCase):
    def test_version_file(self) -> None:
        self.assertEqual(
            "2026.10.31.4", (CE / "VERSION").read_text(encoding="utf-8").strip()
        )


if __name__ == "__main__":
    unittest.main()
