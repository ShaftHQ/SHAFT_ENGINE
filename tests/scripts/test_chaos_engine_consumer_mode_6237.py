"""#6237: consumer-repository install keeps the overlay out of `git status`."""

from __future__ import annotations

import importlib.util
import os
import subprocess  # nosec B404 - tests drive local git on temp fixtures.
import sys
import tempfile
import unittest
import unittest.mock as mock
from pathlib import Path

from tests.scripts.test_chaos_engine_installer import MODULE, SOURCE, AccountDependencyController

CONSUMER = SOURCE / "consumer_mode.py"


def git(cwd: Path, *arguments: str) -> str:
    return subprocess.run(  # nosec B603 B607 - fixed git argv on a temp fixture.
        ["git", *arguments], cwd=cwd, capture_output=True, text=True, check=False
    ).stdout


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class ConsumerFixture(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.project = Path(temporary.name).resolve() / "consumer"
        self.project.mkdir()
        git(self.project, "init", "-q", "-b", "main")
        git(self.project, "config", "user.email", "consumer@example.invalid")
        git(self.project, "config", "user.name", "Consumer")
        self.agents = b"# Their agents file\n"
        self.gitignore = b"node_modules/\n"
        (self.project / "AGENTS.md").write_bytes(self.agents)
        (self.project / ".gitignore").write_bytes(self.gitignore)
        (self.project / "README.md").write_text("consumer\n", encoding="utf-8")
        git(self.project, "add", "-A")
        git(self.project, "commit", "-qm", "initial")
        self.exclude = Path(git(self.project, "rev-parse", "--path-format=absolute", "--git-path", "info/exclude").strip())
        self.exclude.parent.mkdir(parents=True, exist_ok=True)
        self.exclude.write_text("# user rule\n*.local\n", encoding="utf-8")
        load_controller = MODULE.load_dependency_controller
        patcher = mock.patch.object(
            MODULE,
            "load_dependency_controller",
            side_effect=lambda root: AccountDependencyController(load_controller(root)),
        )
        patcher.start()
        self.addCleanup(patcher.stop)

    def install(self, consumer: bool = True):
        value = {"CHAOS_ENGINE_CONSUMER": "1"} if consumer else {}
        with mock.patch.dict(os.environ, value):
            if not consumer:
                os.environ.pop("CHAOS_ENGINE_CONSUMER", None)
            MODULE.install_with_dependencies(self.project, SOURCE, "1" * 40)

    def status(self) -> str:
        return git(self.project, "status", "--porcelain", "--untracked-files=all")



class ConsumerModeInstallTest(ConsumerFixture):
    def test_consumer_install_leaves_git_status_empty_and_tracked_files_untouched(self):
        self.install()
        self.assertEqual("", self.status())
        self.assertEqual(self.agents, (self.project / "AGENTS.md").read_bytes())
        self.assertEqual(self.gitignore, (self.project / ".gitignore").read_bytes())
        self.assertTrue((self.project / ".chaos-engine/skills/chaos-engine/SKILL.md").is_file())
        exclude = self.exclude.read_text(encoding="utf-8")
        self.assertTrue(exclude.startswith("# user rule\n*.local\n"))
        self.assertEqual(1, exclude.count("# CHAOSENGINE-CONSUMER:START"))
        self.assertIn("/.chaos-engine/", exclude)
        self.assertIn("/.mcp.json", exclude)

    def test_consumer_install_reports_untouched_tracked_files_and_stays_verifiable(self):
        self.install()
        self.assertEqual(["AGENTS.md"], [item for item in MODULE.consumer_untouched_files(self.project) if item != ".gitignore"])
        hosts = load(self.project / ".chaos-engine/hosts.py", "ce_hosts_consumer_verify")
        self.assertEqual("healthy", hosts.verify(self.project)["status"])
        self.assertEqual([], hosts.competing_policy_errors(self.project))

    def test_mode_persists_so_reinstall_keeps_one_block_and_a_clean_status(self):
        self.install()
        self.install(consumer=False)
        self.assertEqual("", self.status())
        self.assertEqual(1, self.exclude.read_text(encoding="utf-8").count("# CHAOSENGINE-CONSUMER:START"))

    def test_uninstall_removes_only_the_consumer_block(self):
        self.install()
        MODULE.uninstall_with_dependencies(self.project)
        self.assertEqual("# user rule\n*.local\n", self.exclude.read_text(encoding="utf-8"))
        self.assertEqual(self.agents, (self.project / "AGENTS.md").read_bytes())
        self.assertEqual(self.gitignore, (self.project / ".gitignore").read_bytes())

    def test_default_adopter_install_keeps_the_tracked_overlay_behavior(self):
        self.install(consumer=False)
        self.assertIn("CHAOSENGINE-RUNTIME:START", (self.project / ".gitignore").read_text(encoding="utf-8"))
        self.assertNotIn("CHAOSENGINE-CONSUMER", self.exclude.read_text(encoding="utf-8"))

    def test_linked_worktrees_share_the_common_exclude_file(self):
        consumer = load(CONSUMER, "ce_consumer_worktree")
        linked = self.project.parent / "linked"
        git(self.project, "worktree", "add", "-q", "--detach", str(linked), "HEAD")
        self.assertEqual(self.exclude.resolve(), consumer.exclude_file(linked).resolve())

    def test_cli_and_bootstrap_expose_the_consumer_flag(self):
        args = MODULE.parser().parse_args(
            ["install", "--project", ".", "--source", ".", "--commit", "1" * 40, "--consumer"]
        )
        self.assertTrue(args.consumer)
        bootstrap = load(SOURCE / "bootstrap.py", "ce_bootstrap_consumer")
        self.assertTrue(bootstrap.parser().parse_args(["--repository", "a/b", "--consumer"]).consumer)


if __name__ == "__main__":
    unittest.main()


class ConsumerModeCliTest(ConsumerFixture):
    """The documented CLI path (`install --consumer`) end to end, core only."""

    def test_cli_consumer_install_is_invisible_to_git_status(self):
        environment = {key: value for key, value in os.environ.items() if key != "CHAOS_ENGINE_CONSUMER"}
        completed = subprocess.run(  # nosec B603 - fixed interpreter and installer.
            [
                sys.executable, str(SOURCE / "install.py"), "install", "--project", str(self.project),
                "--source", str(SOURCE), "--commit", "1" * 40, "--skip-tools", "--consumer",
            ],
            capture_output=True, text=True, check=False, env=environment, timeout=300,
        )
        self.assertEqual(0, completed.returncode, completed.stderr)
        self.assertEqual("", self.status())
        self.assertTrue((self.project / ".chaos-engine-state/consumer-mode").is_file())
