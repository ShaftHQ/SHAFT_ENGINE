"""#7964: a missing `.chaos-engine/tool.py` fails doctor and the next hook denial."""

from __future__ import annotations

import hashlib
import importlib.util
import io
import json
import os
import subprocess  # nosec B404 - the repair test launches the repo interpreter on install.py
import sys
import tempfile
import unittest
import unittest.mock
from pathlib import Path
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
CE = ROOT / "chaos-engine"
REPAIR = "python3 .chaos-engine/install.py repair --project ."


def _load(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise ImportError(path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class MissingToolDoctorTest(unittest.TestCase):
    def test_doctor_names_the_file_and_fails_when_tool_py_is_gone(self) -> None:
        install = _load("ce_install_7964", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            (project / ".chaos-engine").mkdir(parents=True)
            healthy = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
                "policySha256": "ab" * 32,
            }
            args = SimpleNamespace(command="doctor", project=project)
            with unittest.mock.patch.object(install, "status_json", return_value=dict(healthy)):
                result = install._command_result(args, [])
            self.assertEqual("recovery-required", result["status"])
            self.assertIn(".chaos-engine/tool.py", result["components"])
            report = install.format_health_report(result, kind="doctor")
            self.assertIn(".chaos-engine/tool.py", report)
            self.assertEqual(1, install.doctor_exit_code(result))
            summary = install.format_agent_summary(result)
            self.assertTrue(summary.startswith("doctor: fail"))
            self.assertIn(".chaos-engine/tool.py", summary)

    def test_doctor_stays_healthy_when_the_overlay_tool_exists(self) -> None:
        install = _load("ce_install_7964_present", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            tool = project / ".chaos-engine" / "tool.py"
            tool.parent.mkdir(parents=True)
            tool.write_text("print(1)\n", encoding="utf-8")
            healthy = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
                "policySha256": "ab" * 32,
            }
            args = SimpleNamespace(command="doctor", project=project)
            with unittest.mock.patch.object(install, "status_json", return_value=dict(healthy)):
                result = install._command_result(args, [])
            self.assertEqual("healthy", result["status"])
            self.assertNotIn(".chaos-engine/tool.py", result["components"])

    def test_doctor_stays_healthy_when_the_project_has_no_overlay(self) -> None:
        install = _load("ce_install_7964_no_overlay", CE / "install.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            healthy = {
                "status": "healthy",
                "components": {"core": {"status": "healthy"}},
                "policySha256": "ab" * 32,
            }
            args = SimpleNamespace(command="doctor", project=project)
            with unittest.mock.patch.object(install, "status_json", return_value=dict(healthy)):
                result = install._command_result(args, [])
            self.assertEqual("healthy", result["status"])
            self.assertNotIn(".chaos-engine/tool.py", result["components"])

    def test_doctor_cli_names_the_file_when_only_tool_py_is_missing(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            overlay = project / ".chaos-engine"
            (overlay / "hooks").mkdir(parents=True)
            guard = overlay / "hooks" / "guard.py"
            guard.write_text("delete_guard\n", encoding="utf-8")
            digest = hashlib.sha256(guard.read_bytes()).hexdigest()
            manifest = {
                "schemaVersion": 1,
                "distribution": {"id": "portable", "policySha256": "ab" * 32},
                "source": {"commit": "c" * 40, "kind": "local"},
                "files": {"hooks/guard.py": digest, "tool.py": "d" * 64},
                "hostToken": "ab" * 32,
            }
            (overlay / "manifest.json").write_text(json.dumps(manifest), encoding="utf-8")
            command = [sys.executable, str(CE / "install.py"), "doctor", "--project", str(project)]
            human = subprocess.run(command, capture_output=True, text=True, check=False)  # nosec B603
            self.assertEqual(1, human.returncode, human.stderr)
            self.assertIn(".chaos-engine/tool.py", human.stdout + human.stderr)
            payload = subprocess.run(  # nosec B603 - fixed interpreter and install.py argv
                [*command, "--json"],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(1, payload.returncode, payload.stderr)
            self.assertIn(".chaos-engine/tool.py", payload.stdout + payload.stderr)

    def test_overlay_doctor_row_names_the_missing_file(self) -> None:
        guard = _load("ce_delete_7964_doctor", CE / "delete_guard.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / ".chaos-engine"
            (root / "hooks").mkdir(parents=True)
            (root / "hooks" / "guard.py").write_text("delete_guard\n", encoding="utf-8")
            status, reason = guard.doctor_check(root)
            self.assertEqual("fail", status)
            self.assertIn(".chaos-engine/tool.py", reason)


class MissingToolRepairTest(unittest.TestCase):
    def test_repair_without_component_restores_tool_py_from_source(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            source = project / "chaos-engine" / "tool.py"
            source.parent.mkdir(parents=True)
            source.write_bytes((CE / "tool.py").read_bytes())
            overlay = project / ".chaos-engine"
            overlay.mkdir()
            completed = subprocess.run(  # nosec B603 - fixed interpreter and install.py argv
                [sys.executable, str(CE / "install.py"), "repair", "--project", str(project)],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, completed.returncode, completed.stderr)
            restored = overlay / "tool.py"
            self.assertEqual(source.read_bytes(), restored.read_bytes())
            again = subprocess.run(  # nosec B603 - fixed interpreter and install.py argv
                [sys.executable, str(CE / "install.py"), "repair", "--project", str(project)],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertNotEqual(0, again.returncode)
            self.assertIn("--component", again.stderr + again.stdout)


class MissingToolHookTest(unittest.TestCase):
    def _event(self, cwd: Path, target: Path) -> dict:
        return {
            "hook_event_name": "PreToolUse",
            "tool_name": "Bash",
            "tool_input": {"command": "rm -f " + str(target)},
            "cwd": str(cwd),
        }

    def test_hook_denial_names_the_repair_when_tool_py_is_gone(self) -> None:
        guard_mod = _load("ce_guard_7964", CE / "hooks" / "guard.py")
        real_loader = guard_mod._delete_guard

        def loader():
            module = real_loader()
            module.DEFAULT_ALLOW = ()
            return module

        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            project.mkdir()
            (project / ".chaos-engine" / "hooks").mkdir(parents=True)
            target = project / "sub" / "notes.txt"
            target.parent.mkdir()
            target.write_text("keep\n", encoding="utf-8")
            with (
                unittest.mock.patch.object(guard_mod, "_delete_guard", loader),
                unittest.mock.patch.object(guard_mod, "_record_denial_with_significance"),
            ):
                buffer = io.StringIO()
                with unittest.mock.patch("sys.stdout", buffer):
                    code = guard_mod._guarded_event(self._event(target.parent, target), "claude")
            self.assertEqual(2, code)
            self.assertIn(REPAIR, buffer.getvalue())
            tool = project / ".chaos-engine" / "tool.py"
            tool.write_text("print(1)\n", encoding="utf-8")
            buffer = io.StringIO()
            with (
                unittest.mock.patch.object(guard_mod, "_delete_guard", loader),
                unittest.mock.patch.object(guard_mod, "_record_denial_with_significance"),
                unittest.mock.patch("sys.stdout", buffer),
            ):
                code = guard_mod._guarded_event(self._event(target.parent, target), "claude")
            self.assertEqual(2, code)
            self.assertNotIn(REPAIR, buffer.getvalue())


class AtomicToolReplaceTest(unittest.TestCase):
    def test_sync_keeps_the_old_file_until_the_new_bytes_exist(self) -> None:
        overlay = _load("ce_overlay_7964", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "primary" / ".chaos-engine" / "tool.py"
            source.parent.mkdir(parents=True)
            source.write_text("new-tool\n", encoding="utf-8")
            session = root / "session"
            dest = session / ".chaos-engine" / "tool.py"
            dest.parent.mkdir(parents=True)
            dest.write_text("old-tool\n", encoding="utf-8")
            seen: dict[str, str] = {}
            real_replace = os.replace
            recorded = overlay._file_sha256(dest)

            def spy(src, dst):
                seen["before"] = Path(dst).read_text(encoding="utf-8")
                seen["staged"] = Path(src).read_text(encoding="utf-8")
                return real_replace(src, dst)

            with unittest.mock.patch("os.replace", spy):
                overlay._sync_file(
                    source,
                    dest,
                    session=session,
                    manifest={overlay._rel_key(session, dest): recorded},
                    upgraded=[],
                    copied=[],
                    unchanged=[],
                )
            self.assertEqual("old-tool\n", seen["before"])
            self.assertEqual("new-tool\n", seen["staged"])
            self.assertEqual("new-tool\n", dest.read_text(encoding="utf-8"))
            self.assertFalse(list(dest.parent.glob("*.ce-write")))

    def test_restore_keeps_the_old_file_until_the_backup_bytes_exist(self) -> None:
        overlay = _load("ce_overlay_7964_restore", CE / "worktree_overlay.py")
        with tempfile.TemporaryDirectory() as temporary:
            session = Path(temporary) / "session"
            dest = session / ".chaos-engine" / "tool.py"
            dest.parent.mkdir(parents=True)
            dest.write_text("old-tool\n", encoding="utf-8")
            backup = Path(temporary) / "backup"
            source = backup / ".chaos-engine" / "tool.py"
            source.parent.mkdir(parents=True)
            source.write_text("backed-up\n", encoding="utf-8")
            (backup / "MANIFEST.json").write_text(
                json.dumps({"files": [".chaos-engine/tool.py"]}) + "\n",
                encoding="utf-8",
            )
            seen: dict[str, str] = {}
            real_replace = os.replace

            def spy(src, dst):
                if Path(dst) == dest:
                    seen["before"] = dest.read_text(encoding="utf-8")
                    seen["staged"] = Path(src).read_text(encoding="utf-8")
                return real_replace(src, dst)

            with unittest.mock.patch("os.replace", spy):
                overlay._restore_session_backup(session, backup)
            self.assertEqual("old-tool\n", seen["before"])
            self.assertEqual("backed-up\n", seen["staged"])
            self.assertEqual("backed-up\n", dest.read_text(encoding="utf-8"))
            self.assertFalse(list(dest.parent.glob("*.ce-write")))

    def test_source_sync_keeps_the_old_file_until_the_new_bytes_exist(self) -> None:
        match = _load("ce_overlay_match_7964", CE / "overlay_match.py")
        with tempfile.TemporaryDirectory() as temporary:
            dest = Path(temporary) / "tool.py"
            dest.write_text("old-tool\n", encoding="utf-8")
            seen: dict[str, str] = {}
            real_replace = Path.replace

            def spy(self, target):
                seen["before"] = Path(target).read_text(encoding="utf-8")
                seen["staged"] = self.read_text(encoding="utf-8")
                return real_replace(self, target)

            with unittest.mock.patch.object(Path, "replace", spy):
                match._replace_bytes(dest, b"new-tool\n")
            self.assertEqual("old-tool\n", seen["before"])
            self.assertEqual("new-tool\n", seen["staged"])
            self.assertEqual("new-tool\n", dest.read_text(encoding="utf-8"))
            self.assertFalse(list(dest.parent.glob("*.ce-write")))

    def test_source_sync_entry_replaces_tool_py_without_truncating_it(self) -> None:
        match = _load("ce_overlay_match_7964_entry", CE / "overlay_match.py")
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "proj"
            source_tool = project / "chaos-engine" / "tool.py"
            skill = project / "chaos-engine" / "skills" / "chaos-engine" / "SKILL.md"
            skill.parent.mkdir(parents=True)
            skill.write_text("router\n", encoding="utf-8")
            source_tool.write_text("new-tool\n", encoding="utf-8")
            dest = project / ".chaos-engine" / "tool.py"
            dest.parent.mkdir(parents=True)
            dest.write_text("old-tool\n", encoding="utf-8")
            seen: dict[str, str] = {}
            real_replace = Path.replace

            def spy(self, target):
                if Path(target) == dest:
                    seen["before"] = dest.read_text(encoding="utf-8")
                    seen["staged"] = self.read_text(encoding="utf-8")
                return real_replace(self, target)

            with (
                unittest.mock.patch.object(match, "owned_source_files", return_value=(source_tool,)),
                unittest.mock.patch.object(match, "_rewrite_manifest_files"),
                unittest.mock.patch.object(Path, "replace", spy),
            ):
                match.sync_overlay_from_source(project)
            self.assertEqual("old-tool\n", seen["before"])
            self.assertEqual("new-tool\n", seen["staged"])
            self.assertEqual("new-tool\n", dest.read_text(encoding="utf-8"))


class GateSelectionTest(unittest.TestCase):
    def test_changed_tool_paths_select_this_module(self) -> None:
        from scripts.ci.harness_pr_gate import classify_paths

        module = "tests.scripts.test_ce_tool_missing_7964"
        for path in (
            "chaos-engine/delete_guard.py",
            "chaos-engine/install.py",
            "chaos-engine/hooks/guard.py",
            "chaos-engine/overlay_match.py",
            "chaos-engine/worktree_overlay.py",
            "tests/scripts/test_ce_tool_missing_7964.py",
        ):
            plan = classify_paths([path])
            self.assertIn(module, plan.test_modules, path)


if __name__ == "__main__":
    unittest.main()
