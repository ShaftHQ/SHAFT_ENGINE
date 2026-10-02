"""
Fresh-clone MemPalace mine runs in the background with a doctor status.

Measured root cause: a fresh SHAFT_ENGINE clone mines 3,196 files into about
56k drawers. CPU embedding runs at about 30 drawers/s, which is well past the
900 s synchronous account-command budget, so install failed with
CE-INSTALL-FAILED. Install now prepares the exact palace synchronously, hands
the incremental mine to a detached runner, and doctor reports
running/interrupted/failed with a fix-next line.
"""

from __future__ import annotations

import importlib.util
import json
import os
import tempfile
from unittest import TestCase, main, mock
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parents[2]
CONTROLLER = ROOT / "chaos-engine/dependencies.py"
SPECIFICATION = ROOT / "chaos-engine/dependencies.json"
INSTALLER = ROOT / "chaos-engine/install.py"
TOOLS = ("uv", "python", "node", "java", "mempalace", "graphify", "memory", "context7")


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class FakeSpawner:
    def __init__(self) -> None:
        self.calls: list[tuple[list[str], dict]] = []

    def __call__(self, argv, **kwargs):
        self.calls.append((list(argv), kwargs))
        return SimpleNamespace(pid=4242)

    def argv(self) -> list[str]:
        return self.calls[0][0]


class BackgroundMineInstallTest(TestCase):
    def setUp(self) -> None:
        self.module = load(CONTROLLER, "ce_deps_background_mine")
        self.specification = json.loads(SPECIFICATION.read_text(encoding="utf-8"))
        self.commands = {"mempalace": "/tools/mempalace", "uv": "/tools/uv", "npm": "/tools/npm"}
        self.local = {name: {"healthy": True, "version": "1.0", "detail": "passed"} for name in TOOLS}
        self.actions = {name: {"action": "reused"} for name in self.local}

    def install(self, project: Path, mine: list[str], **kwargs):
        module = self.module
        pair = (self.local, self.commands)
        with mock.patch.object(module, "discover_account_commands", side_effect=(pair, pair)), \
                mock.patch.object(module, "resolve_account_actions", return_value=self.actions), \
                mock.patch.object(module, "project_setup_plan", return_value=[mine]):
            return module.install_account_dependencies(
                project, self.specification, allow_root=True, **kwargs
            )

    def test_fresh_clone_mine_is_detached_and_install_does_not_block(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            palace = self.module.mempalace_project_palace(project)
            mine = self.module.mempalace_project_cli("/tools/mempalace", "mine", project)
            spawner = FakeSpawner()

            def runner(command, **_kwargs):
                self.fail(f"mine must not run synchronously: {command}")

            with mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE_MINE": ""}):
                self.install(project, mine, runner=runner, mine_spawner=spawner)

            self.assertEqual(1, len(spawner.calls))
            argv, kwargs = spawner.calls[0]
            self.assertEqual(mine, argv[argv.index("--") + 1:])
            self.assertIn(self.module.MEMPALACE_INDEX_SUBCOMMAND, argv)
            self.assertEqual("sqlite_exact", kwargs["env"]["MEMPALACE_BACKEND"])
            if os.name == "nt":
                self.assertTrue(kwargs.get("creationflags"))
            else:
                self.assertTrue(kwargs.get("start_new_session"))
            self.assertIsNotNone(kwargs.get("stdout"))
            # Palace is usable immediately; .mined only after the mine finishes.
            self.assertTrue((palace / "sqlite_exact.sqlite3").is_file())
            self.assertFalse((palace / ".mined").exists())
            status = self.module.mempalace_index_status(project, alive=lambda _pid: True)
            self.assertEqual("running", status["status"])
            self.assertIn("log", status["fixNext"])

    def test_foreground_override_keeps_synchronous_mine(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            mine = self.module.mempalace_project_cli("/tools/mempalace", "mine", project)
            spawner = FakeSpawner()
            calls = []

            def runner(command, **_kwargs):
                calls.append(command)
                return SimpleNamespace(returncode=0, stdout="", stderr="")

            with mock.patch.dict(os.environ, {"CHAOS_ENGINE_MEMPALACE_MINE": "foreground"}):
                self.install(project, mine, runner=runner, mine_spawner=spawner)

            self.assertEqual([mine], calls)
            self.assertEqual([], spawner.calls)


class BackgroundMineRunnerTest(TestCase):
    def setUp(self) -> None:
        self.module = load(CONTROLLER, "ce_deps_background_runner")

    def test_runner_marks_complete_and_mined_after_success(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            self.module.prepare_mempalace_project_target(project)
            mine = self.module.mempalace_project_cli("/tools/mempalace", "mine", project)
            seen = {}

            def runner(_command, **kwargs):
                seen["timeout"] = kwargs.get("timeout")
                status = self.module.mempalace_index_status(project, alive=lambda _pid: True)
                seen["during"] = status["status"]
                return SimpleNamespace(returncode=0, stdout="", stderr="")

            self.assertEqual(0, self.module.run_mempalace_index(project, mine, runner=runner))
            self.assertEqual("running", seen["during"])
            self.assertGreater(seen["timeout"], self.module.ACCOUNT_COMMAND_TIMEOUT_SECONDS)
            self.assertEqual("complete", self.module.mempalace_index_status(project)["status"])
            palace = self.module.mempalace_project_palace(project)
            self.assertEqual(b"current\n", (palace / ".mined").read_bytes())

    def test_failed_and_interrupted_mines_report_fix_next_and_resume(self):
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary)
            project.joinpath("mempalace.yaml").write_text("wing: t\n", encoding="utf-8")
            self.module.prepare_mempalace_project_target(project)
            mine = self.module.mempalace_project_cli("/tools/mempalace", "mine", project)

            def failing(_command, **_kwargs):
                return SimpleNamespace(returncode=1, stdout="", stderr="boom")

            self.assertEqual(1, self.module.run_mempalace_index(project, mine, runner=failing))
            failed = self.module.mempalace_index_status(project)
            self.assertEqual("failed", failed["status"])
            self.assertIn("mine", failed["fixNext"])
            plan = self.module.project_setup_plan(project, {"mempalace": "/tools/mempalace"})
            self.assertIn(mine, plan)

            status_path, _log = self.module.mempalace_index_paths(project)
            status_path.write_text(json.dumps({"schemaVersion": 1, "status": "running", "pid": 99999999,
                                               "command": mine}), encoding="utf-8")
            interrupted = self.module.mempalace_index_status(project, alive=lambda _pid: False)
            self.assertEqual("interrupted", interrupted["status"])
            self.assertIn("mine", interrupted["fixNext"])
            with mock.patch.object(self.module, "_process_alive", return_value=False):
                self.assertIn(mine, self.module.project_setup_plan(project, {"mempalace": "/tools/mempalace"}))
            with mock.patch.object(self.module, "_process_alive", return_value=True):
                self.assertNotIn(mine, self.module.project_setup_plan(project, {"mempalace": "/tools/mempalace"}))


class DoctorIndexFindingTest(TestCase):
    def test_doctor_prints_index_finding_with_fix_next(self):
        installer = load(INSTALLER, "ce_installer_index_finding")
        document = {"components": {"mempalace": {"status": "healthy", "index": {
            "status": "running", "detail": "MemPalace initial index running in background",
            "fixNext": "progress log: /x/mempalace-index.log",
        }}}}
        lines = installer.format_host_environment_findings(document)
        self.assertIn("[info] mempalace/index — MemPalace initial index running in background", lines)
        self.assertIn("  fix-next: progress log: /x/mempalace-index.log", lines)
        document["components"]["mempalace"]["index"]["status"] = "complete"
        self.assertEqual([], installer.format_host_environment_findings(document))


if __name__ == "__main__":
    main()
