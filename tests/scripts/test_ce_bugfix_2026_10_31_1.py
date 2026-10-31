"""CE bugfix batch 2026.10.31.1 regression tests."""
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
import unittest.mock

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
        self.assertEqual("2026.10.31.1", (CE / "VERSION").read_text(encoding="utf-8").strip())


class ShortCommitResolveTest(unittest.TestCase):
    def test_short_sha_resolves_to_full(self) -> None:
        install = load("ce_install_short_sha", CE / "install.py")
        full = install.detect_full_commit(ROOT)
        self.assertIsNotNone(full)
        assert full is not None
        short = full[:7]
        resolved = install.resolve_commit_sha(short, cwd=ROOT)
        self.assertEqual(full, resolved)
        self.assertRegex(resolved, r"^[0-9a-f]{40}$")

    def test_install_accepts_short_sha(self) -> None:
        install = load("ce_install_short_sha_cli", CE / "install.py")
        full = install.detect_full_commit(ROOT)
        assert full is not None
        short = full[:9]
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            result = subprocess.run(  # nosec B603
                [
                    sys.executable,
                    str(CE / "install.py"),
                    "install",
                    "--project",
                    str(project),
                    "--source",
                    str(CE),
                    "--commit",
                    short,
                    "--skip-tools",
                    "--no-secret-guard",
                ],
                capture_output=True,
                text=True,
                timeout=180,
                check=False,
            )
            output = result.stdout + result.stderr
            self.assertEqual(0, result.returncode, output)
            manifest = json.loads((project / ".chaos-engine" / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(full, manifest["source"]["commit"])

    def test_symbolic_ref_still_refused(self) -> None:
        result = subprocess.run(  # nosec B603
            [
                sys.executable,
                str(CE / "install.py"),
                "install",
                "--project",
                str(tempfile.mkdtemp()),
                "--source",
                str(CE),
                "--commit",
                "HEAD",
                "--skip-tools",
            ],
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        self.assertNotEqual(0, result.returncode)
        self.assertIn("hex sha", (result.stdout + result.stderr).casefold())


class UvToolLockTest(unittest.TestCase):
    def test_lock_serializes_and_retries_directory_not_empty(self) -> None:
        deps = load("ce_deps_uv_lock", CE / "dependencies.py")
        with tempfile.TemporaryDirectory() as temporary:
            tools = Path(temporary) / "uv-tools"
            tools.mkdir()
            calls = {"n": 0}

            def flaky(command, project, runner=None):  # noqa: ANN001
                calls["n"] += 1
                if calls["n"] < 3:
                    raise deps._AccountCommandError(
                        "dependency command failed: uv: failed to remove "
                        f"{tools}/graphifyy/lib: Directory not empty",
                        stderr="failed to remove .../graphifyy/lib: Directory not empty",
                        stdout="",
                    )
                return subprocess.CompletedProcess(command, 0, "", "")

            cmd = ["uv", "tool", "install", "--force", "graphifyy==0.9.97"]
            with unittest.mock.patch.object(deps, "default_uv_tools_dir", return_value=tools):
                result = deps.run_uv_tool_install_with_lock(
                    cmd, Path(temporary), run_command=flaky
                )
            self.assertEqual(0, result.returncode)
            self.assertEqual(3, calls["n"])
            self.assertTrue(deps.is_uv_tool_install_command(cmd))
            self.assertFalse(deps.is_uv_tool_install_command(["npm", "install", "-g", "x"]))


class AgentSummaryDegradedTest(unittest.TestCase):
    def test_graphify_degraded_fails_summary(self) -> None:
        install = load("ce_install_summary_deg", CE / "install.py")
        document = {
            "status": "healthy",
            "policySha256": "ab" * 32,
            "components": {
                "core": {"status": "healthy"},
                "graphify": {
                    "status": "degraded",
                    "detail": "stale - indexed=deadbeef",
                    "fixNext": "repair --component graphify",
                },
            },
        }
        rendered = install.format_agent_summary(document)
        self.assertIn("doctor: fail", rendered)
        self.assertIn("component: graphify degraded", rendered)
        self.assertEqual(1, install.agent_summary_exit_code(document))


class GraphifyAutoRepairTest(unittest.TestCase):
    def test_refresh_stale_graph_repairs_when_still_degraded(self) -> None:
        install = load("ce_install_graphify_repair", CE / "install.py")
        rows = iter(
            [
                {"status": "degraded", "detail": "broken"},
                {"status": "degraded", "detail": "broken"},
                {"status": "healthy", "detail": "ok"},
            ]
        )
        refreshes: list[bool] = []

        def doctor_row(project):  # noqa: ANN001
            return next(rows)

        def refresh(project, if_stale=False, components=None):  # noqa: ANN001
            refreshes.append(if_stale)

        stores = {"graphify_doctor_row": doctor_row, "refresh": refresh}
        with unittest.mock.patch.object(install, "_auto_repair_graphify_after_upgrade") as auto:
            result = install.refresh_stale_graph(Path("."), stores=stores)
        self.assertEqual("repaired", result)
        self.assertEqual([False, False], refreshes)
        auto.assert_called_once()


class SecretGuardBrokenGitTest(unittest.TestCase):
    def test_broken_git_fails_loudly(self) -> None:
        hooks = load("ce_git_hooks_broken", CE / "git_hooks.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            (project / ".git").mkdir()
            result = hooks.install_secret_guard(project, CE / "secret_guard.py")
            self.assertEqual("failed", result["status"])
            self.assertIn("broken git", result["reason"])

    def test_missing_objects_refs_fails(self) -> None:
        hooks = load("ce_git_hooks_objects", CE / "git_hooks.py")
        git = shutil.which("git")
        self.assertIsNotNone(git)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            subprocess.run([git, "init", "-q"], cwd=project, check=True, capture_output=True)  # nosec B603
            common = hooks._git(project, "rev-parse", "--git-common-dir")
            common_dir = Path(common) if Path(common).is_absolute() else project / common
            shutil.rmtree(common_dir / "objects")
            shutil.rmtree(common_dir / "refs")
            result = hooks.install_secret_guard(project, CE / "secret_guard.py")
            self.assertEqual("failed", result["status"])
            self.assertIn("broken git", result["reason"])


class WorktreeOwnershipAndAdoptTest(unittest.TestCase):
    def test_materialize_does_not_trip_ownership_drift(self) -> None:
        overlay = load("ce_overlay_own_bug3", CE / "worktree_overlay.py")
        install = load("ce_install_own_bug3", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            base = Path(temporary)
            primary = base / "primary"
            session = base / "session"
            ce = primary / ".chaos-engine"
            (ce / "skills" / "chaos-engine").mkdir(parents=True)
            (ce / "hooks").mkdir(parents=True)
            (ce / "skills" / "chaos-engine" / "SKILL.md").write_text("v1\n", encoding="utf-8")
            (ce / "hooks" / "guard.py").write_text("g\n", encoding="utf-8")
            (ce / "VERSION").write_text("2026.10.31.1\n", encoding="utf-8")
            overlay.materialize(primary, session)
            bookkeeping = session / ".chaos-engine" / ".overlay-materialize.json"
            self.assertTrue(bookkeeping.is_file())
            payload = install.installed_payload(session / ".chaos-engine")
            self.assertNotIn(".overlay-materialize.json", payload)
            self.assertTrue(
                install.is_worktree_materialize_bookkeeping(Path(".overlay-materialize.json"))
            )

    def test_upgrade_session_adopts_diverged_budget_and_implementer(self) -> None:
        """HRSD: budget + implementer refuse when dest != recorded != primary."""
        overlay = load("ce_overlay_adopt_bug3", CE / "worktree_overlay.py")
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
            (primary / ".gitignore").write_text(".chaos-engine/\n.claude/\n", encoding="utf-8")
            (primary / "README").write_text("p\n", encoding="utf-8")
            subprocess.run([git, "add", "README", ".gitignore"], cwd=primary, check=True, capture_output=True)  # nosec B603
            subprocess.run([git, "commit", "-qm", "i"], cwd=primary, check=True, capture_output=True)  # nosec B603
            session = base / "session"
            subprocess.run(  # nosec B603
                [git, "worktree", "add", "-q", str(session), "HEAD"],
                cwd=primary, check=True, capture_output=True,
            )

            def write_overlay(root: Path, version: str, budget: str, implementer: str) -> None:
                ce = root / ".chaos-engine"
                (ce / "skills" / "chaos-engine").mkdir(parents=True)
                (ce / "hooks").mkdir(parents=True)
                (ce / "VERSION").write_text(version + "\n", encoding="utf-8")
                (ce / "skills" / "chaos-engine" / "SKILL.md").write_text("skill-" + version + "\n", encoding="utf-8")
                (ce / "hooks" / "guard.py").write_text("guard\n", encoding="utf-8")
                (ce / "always-loaded-budget.json").write_text(budget + "\n", encoding="utf-8")
                agents = root / ".claude" / "agents"
                agents.mkdir(parents=True)
                (agents / "chaos-engine-implementer.md").write_text(implementer + "\n", encoding="utf-8")

            write_overlay(primary, "2026.10.31.1", '{"cap": 2}', "implementer-NEW")
            write_overlay(session, "2026.10.30.2", '{"cap": 1}', "implementer-OLD")
            # Seed a stale materialize manifest whose recorded hashes match neither
            # the session (CE rewrote budget) nor primary — the HRSD refusal mode.
            stale = {
                ".chaos-engine/VERSION": hashlib.sha256(b"stale-version\n").hexdigest(),
                ".chaos-engine/always-loaded-budget.json": hashlib.sha256(b"stale-budget\n").hexdigest(),
                ".claude/agents/chaos-engine-implementer.md": hashlib.sha256(b"stale-agent\n").hexdigest(),
            }
            man_path = session / ".chaos-engine" / ".overlay-materialize.json"
            man_path.parent.mkdir(parents=True, exist_ok=True)
            man_path.write_text(
                json.dumps({"schema": 1, "files": stale}, indent=2) + "\n", encoding="utf-8"
            )
            # Consumer-edited non-CE file must stay refused if we ever sync it;
            # smoke: upgrade-session itself succeeds for CE-owned paths.
            result = overlay.upgrade_session(primary, session, repair_plugins=False)
            self.assertEqual(
                "2026.10.31.1",
                (session / ".chaos-engine" / "VERSION").read_text(encoding="utf-8").strip(),
            )
            self.assertEqual(
                '{"cap": 2}',
                (session / ".chaos-engine" / "always-loaded-budget.json").read_text(encoding="utf-8").strip(),
            )
            self.assertEqual(
                "implementer-NEW",
                (session / ".claude" / "agents" / "chaos-engine-implementer.md").read_text(encoding="utf-8").strip(),
            )
            self.assertGreaterEqual(len(result.get("written") or []), 1)


if __name__ == "__main__":
    unittest.main()
