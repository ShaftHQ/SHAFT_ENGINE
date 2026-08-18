"""Safety contract for the installer-owned nightly SHAFT clone."""

import importlib.util
import json
import os
import shutil
import stat
import subprocess  # nosec B404 - fixed executable and repository-owned test scripts.
from contextlib import contextmanager, nullcontext
from pathlib import Path
import tempfile
import unittest
import unittest.mock as mock


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools/agent-infra/shaft_knowledge_refresh.py"
ORIGIN = "https://github.com/ShaftHQ/SHAFT_ENGINE"
SHA = "a" * 40
TRUST_MODEL = "exclusive-maintenance-home-v1"


class ShaftKnowledgeRefreshTest(unittest.TestCase):
    @staticmethod
    def module():
        spec = importlib.util.spec_from_file_location("shaft_knowledge_refresh", SCRIPT)
        module = importlib.util.module_from_spec(spec)
        if spec.loader is None:
            raise RuntimeError(f"cannot load test subject: {SCRIPT}")
        spec.loader.exec_module(module)
        return module

    def test_refresh_uses_approved_url_and_verified_sha_not_mutable_names(self):
        module = self.module()
        root = Path(tempfile.mkdtemp()) / "SHAFT_ENGINE-main"
        sentinel = root.parent / ".shaft-nightly-maintenance.json"
        commands = []

        def run(arguments, cwd, environment=None, allowed_exit_codes=(0,)):
            del cwd, allowed_exit_codes
            commands.append((arguments, environment))
            if arguments[1:2] == ["rev-parse"]:
                return SHA + "\n"
            if arguments[1:2] == ["ls-remote"]:
                return f"{SHA}\trefs/heads/main\n"
            return ""

        graph_guard = []

        @contextmanager
        def protect(_path):
            graph_guard.append("enter")
            yield
            graph_guard.append("exit")

        with mock.patch.object(
            module, "validate_owned_clone", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "validate_owned_paths", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "required", side_effect=lambda value: value
        ), mock.patch.object(module, "run", side_effect=run), mock.patch.object(
            module, "job_lock", return_value=nullcontext()
        ), mock.patch.object(
            module, "trusted_graph_output", side_effect=protect
        ), mock.patch.dict(
            os.environ,
            {
                "GIT_DIR": "poisoned",
                "GIT_INDEX_FILE": "poisoned-index",
                "GIT_OBJECT_DIRECTORY": "poisoned-objects",
                "GIT_CONFIG_COUNT": "1",
                "GIT_CONFIG_KEY_0": "url.https://evil.invalid/.insteadOf",
                "GIT_CONFIG_VALUE_0": ORIGIN,
            },
        ):
            module.refresh(root, sentinel)

        invoked = [command for command, _environment in commands]
        self.assertIn(
            ["git", "fetch", "--prune", "--no-tags", ORIGIN,
             "+refs/heads/main:refs/shaft-maintenance/main"], invoked
        )
        self.assertIn(["git", "ls-remote", "--exit-code", ORIGIN, "refs/heads/main"], invoked)
        self.assertIn(["git", "reset", "--hard", SHA], invoked)
        self.assertIn(["git", "clean", "-ffd"], invoked)
        self.assertFalse(any("--dry-run" in command for command in invoked))
        self.assertIn(
            ["mempalace", "sync", str(root.resolve()), "--wing", "shaft_engine_main", "--apply"],
            invoked,
        )
        child_commands = [
            next(
                item for item in commands
                if any(value.endswith("graphify_maintenance.py") for value in item[0])
            ),
            next(item for item in commands if item[0][0:2] == ["mempalace", "sync"]),
            next(item for item in commands if item[0][0:2] == ["mempalace", "mine"]),
        ]
        for _command, environment in child_commands:
            self.assertIsNotNone(environment)
            self.assertNotIn("GIT_DIR", environment)
            self.assertNotIn("GIT_INDEX_FILE", environment)
            self.assertNotIn("GIT_OBJECT_DIRECTORY", environment)
            self.assertEqual(os.devnull, environment["GIT_CONFIG_GLOBAL"])
            self.assertEqual("1", environment["GIT_CONFIG_NOSYSTEM"])
        self.assertEqual(["enter", "exit"], graph_guard)

    def test_store_maintenance_reports_both_outcomes_when_either_store_fails(self):
        module = self.module()
        root = Path(tempfile.mkdtemp()) / "SHAFT_ENGINE-main"
        sentinel = root.parent / ".shaft-nightly-maintenance.json"
        commands = []

        def run(arguments, cwd, environment=None, allowed_exit_codes=(0,)):
            del cwd, environment, allowed_exit_codes
            commands.append(arguments)
            if arguments[1:2] == ["rev-parse"]:
                return SHA + "\n"
            if arguments[1:2] == ["ls-remote"]:
                return f"{SHA}\trefs/heads/main\n"
            if any(value.endswith("graphify_maintenance.py") for value in arguments):
                raise subprocess.CalledProcessError(7, arguments)
            return ""

        with mock.patch.object(
            module, "validate_owned_clone", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "validate_owned_paths", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "required", side_effect=lambda value: value
        ), mock.patch.object(module, "run", side_effect=run), mock.patch.object(
            module, "job_lock", return_value=nullcontext()
        ), mock.patch.object(
            module, "trusted_graph_output", return_value=nullcontext()
        ):
            with self.assertRaisesRegex(RuntimeError, "Graphify=failed.*MemPalace=healthy"):
                module.refresh(root, sentinel)

        self.assertTrue(any(command[0:2] == ["mempalace", "sync"] for command in commands))
        self.assertTrue(any(command[0:2] == ["mempalace", "mine"] for command in commands))

    def test_refresh_force_includes_exact_promote_paths_on_mine(self):
        module = self.module()
        root = Path(tempfile.mkdtemp()) / "SHAFT_ENGINE-main"
        sentinel = root.parent / ".shaft-nightly-maintenance.json"
        commands = []

        def run(arguments, cwd, environment=None, allowed_exit_codes=(0,)):
            del cwd, environment, allowed_exit_codes
            commands.append(arguments)
            if arguments[1:2] == ["rev-parse"]:
                return SHA + "\n"
            if arguments[1:2] == ["ls-remote"]:
                return f"{SHA}\trefs/heads/main\n"
            return ""

        with mock.patch.object(
            module, "validate_owned_clone", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "validate_owned_paths", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "required", side_effect=lambda value: value
        ), mock.patch.object(module, "run", side_effect=run), mock.patch.object(
            module, "job_lock", return_value=nullcontext()
        ), mock.patch.object(
            module, "trusted_graph_output", return_value=nullcontext()
        ), mock.patch.object(
            module,
            "list_promote_paths",
            return_value=["src/custom.properties", "META-INF/plugin.xml"],
        ):
            module.refresh(root, sentinel)

        mines = [command for command in commands if command[0:2] == ["mempalace", "mine"]]
        self.assertTrue(mines)
        included = " ".join(" ".join(command) for command in mines)
        self.assertIn("--include-ignored", included)
        self.assertIn("src/custom.properties", included)
        self.assertIn("META-INF/plugin.xml", included)

    def test_mempalace_failure_keeps_the_successful_graphify_outcome(self):
        module = self.module()
        root = Path(tempfile.mkdtemp()) / "SHAFT_ENGINE-main"
        sentinel = root.parent / ".shaft-nightly-maintenance.json"
        commands = []

        def run(arguments, cwd, environment=None, allowed_exit_codes=(0,)):
            del cwd, environment, allowed_exit_codes
            commands.append(arguments)
            if arguments[1:2] == ["rev-parse"]:
                return SHA + "\n"
            if arguments[1:2] == ["ls-remote"]:
                return f"{SHA}\trefs/heads/main\n"
            if arguments[0:2] == ["mempalace", "sync"]:
                raise subprocess.CalledProcessError(9, arguments)
            return ""

        with mock.patch.object(
            module, "validate_owned_clone", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "validate_owned_paths", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(
            module, "required", side_effect=lambda value: value
        ), mock.patch.object(module, "run", side_effect=run), mock.patch.object(
            module, "job_lock", return_value=nullcontext()
        ), mock.patch.object(
            module, "trusted_graph_output", return_value=nullcontext()
        ):
            with self.assertRaisesRegex(RuntimeError, "Graphify=healthy.*MemPalace=failed"):
                module.refresh(root, sentinel)

        self.assertTrue(
            any(value.endswith("graphify_maintenance.py") for command in commands for value in command)
        )

    def test_local_url_rewrite_stops_before_fetch(self):
        module = self.module()
        root = Path(tempfile.mkdtemp()) / "SHAFT_ENGINE-main"
        sentinel = root.parent / ".shaft-nightly-maintenance.json"
        commands = []

        def run(arguments, cwd, environment=None, allowed_exit_codes=(0,)):
            del cwd, environment, allowed_exit_codes
            commands.append(arguments)
            if arguments[1:2] == ["config"]:
                return f"url.https://evil.invalid/.insteadof {ORIGIN}\n"
            return ""

        with mock.patch.object(
            module, "validate_owned_clone", return_value=(root.resolve(), sentinel.resolve())
        ), mock.patch.object(module, "required", side_effect=lambda value: value), mock.patch.object(
            module, "run", side_effect=run
        ), mock.patch.object(module, "job_lock", return_value=nullcontext()):
            with self.assertRaisesRegex(ValueError, "URL rewrites"):
                module.refresh(root, sentinel)

        self.assertFalse(any(command[1:2] == ["fetch"] for command in commands))

    def test_owned_clone_requires_exact_sentinel_and_matching_git_token(self):
        module = self.module()
        parent = Path(tempfile.mkdtemp())
        root = parent / "SHAFT_ENGINE-main"
        root.mkdir()
        (root / ".git").mkdir()
        sentinel = parent / ".shaft-nightly-maintenance.json"
        sentinel.write_text(json.dumps({
            "schema_version": 1, "repository_root": str(root.resolve()),
            "origin": ORIGIN, "owner_token": "".join(("unit", "-", "owner")),
            "trust_model": TRUST_MODEL
        }), encoding="utf-8")

        with mock.patch.object(module, "is_system_drive", return_value=False), mock.patch.object(
            module, "git", side_effect=[
                f"{root}\n{root / '.git'}\n", ORIGIN + "\n", "unit-owner\n"
            ]
        ):
            module.validate_owned_clone(root, sentinel)
        with mock.patch.object(module, "is_system_drive", return_value=False), mock.patch.object(
            module, "git", side_effect=[f"{root}\n{root / '.git'}\n", ORIGIN + "\n", "different\n"]
        ):
            with self.assertRaisesRegex(ValueError, "owner token"):
                module.validate_owned_clone(root, sentinel)

        data = json.loads(sentinel.read_text(encoding="utf-8"))
        data["schema_version"] = True
        sentinel.write_text(json.dumps(data), encoding="utf-8")
        with mock.patch.object(module, "is_system_drive", return_value=False), mock.patch.object(
            module, "git", side_effect=[
                f"{root}\n{root / '.git'}\n", ORIGIN + "\n", "unit-owner\n"
            ]
        ):
            with self.assertRaisesRegex(ValueError, "schema"):
                module.validate_owned_clone(root, sentinel)

    def test_validation_rejects_a_reparse_ancestor_before_resolution(self):
        module = self.module()
        root = Path(tempfile.mkdtemp()) / "alias" / "SHAFT_ENGINE-main"
        sentinel = root.parent / ".shaft-nightly-maintenance.json"
        with mock.patch.object(
            module, "is_reparse", side_effect=lambda path: path.name == "alias"
        ), mock.patch.object(
            module, "is_system_drive", return_value=False
        ):
            with self.assertRaisesRegex(ValueError, "reparse"):
                module.validate_owned_clone(root, sentinel)

    def test_reparse_detection_uses_the_windows_file_attribute(self):
        module = self.module()
        path = mock.Mock()
        path.is_symlink.return_value = False
        path.is_junction.return_value = False
        path.stat.return_value.st_file_attributes = getattr(
            stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0x400
        )

        self.assertTrue(module.is_reparse(path))

    def test_reparse_inspection_errors_fail_closed(self):
        module = self.module()
        path = mock.Mock()
        path.is_symlink.return_value = False
        path.is_junction.return_value = False
        path.stat.side_effect = PermissionError("denied")

        with self.assertRaisesRegex(ValueError, "cannot inspect"):
            module.is_reparse(path)

    def test_validation_scans_sentinel_ancestors_and_graph_output(self):
        module = self.module()
        parent = Path(tempfile.mkdtemp())
        root = parent / "SHAFT_ENGINE-main"
        sentinel = parent / "sentinel-alias" / ".." / ".shaft-nightly-maintenance.json"
        seen = []

        with mock.patch.object(
            module, "is_reparse", side_effect=lambda path: seen.append(path) or path.name == "graphify-out"
        ), mock.patch.object(
            module, "is_system_drive", return_value=False
        ):
            with self.assertRaisesRegex(ValueError, "reparse"):
                module.validate_owned_clone(root, sentinel)

        self.assertIn(Path(os.path.abspath(sentinel)).parent, seen)
        self.assertIn(Path(os.path.abspath(root)) / "graphify-out", seen)

    def test_validation_rejects_nested_reparse_points_in_owned_mutation_trees(self):
        module = self.module()
        parent = Path(tempfile.mkdtemp())
        root = parent / "SHAFT_ENGINE-main"
        root.mkdir()
        (root / ".git").mkdir()
        sentinel = parent / ".shaft-nightly-maintenance.json"

        with mock.patch.object(module, "is_system_drive", return_value=False), mock.patch.object(
            module, "has_unsafe_descendant", side_effect=lambda path: path == root.resolve().parent
        ):
            with self.assertRaisesRegex(ValueError, "unsafe descendant"):
                module.validate_owned_clone(root, sentinel)

    def test_git_inspection_does_not_echo_the_owner_token(self):
        module = self.module()
        completed = mock.Mock(stdout="secret-owner-token\n")
        with mock.patch.object(module, "required", return_value="git"), mock.patch.object(
            module.subprocess, "run", return_value=completed
        ), mock.patch("builtins.print") as output:
            result = module.git(
                ["config", "--local", "--get", "shaft.maintenanceOwner"], Path(".")
            )

        self.assertEqual("secret-owner-token\n", result)
        output.assert_not_called()

    def test_maintenance_home_rejects_multi_link_files(self):
        module = self.module()
        parent = Path(tempfile.mkdtemp())
        home = parent / "maintenance"
        home.mkdir()
        first = home / "first.log"
        second = home / "second.log"
        first.write_text("same inode", encoding="utf-8")
        os.link(first, second)

        with mock.patch.object(module, "is_system_drive", return_value=False):
            with self.assertRaisesRegex(ValueError, "unsafe descendant"):
                module.validate_lexical_home(home / "SHAFT_ENGINE-main")

    def test_pending_receipt_is_exact_and_independent(self):
        module = self.module()
        parent = Path(tempfile.mkdtemp())
        root = parent / "SHAFT_ENGINE-main"
        pending = parent / ".shaft-install-pending.json"
        valid = {
            "schema_version": 1,
            "repository_root": str(root.resolve()),
            "origin": ORIGIN,
            "owner_token": "a" * 32,
            "trust_model": TRUST_MODEL,
        }
        pending.write_text(json.dumps(valid), encoding="utf-8")
        self.assertEqual(valid, module.validate_pending_receipt(pending, root))

        for mutation in (
            {**valid, "schema_version": True},
            {**valid, "repository_root": str(parent / "other")},
            {**valid, "owner_token": "".join(("invalid", "-", "owner"))},
            {**valid, "extra": "field"},
        ):
            pending.write_text(json.dumps(mutation), encoding="utf-8")
            with self.assertRaises(ValueError):
                module.validate_pending_receipt(pending, root)

    def test_installer_owns_clone_rotates_logs_and_migrates_legacy_task(self):
        installer = (ROOT / "tools/agent-infra/install-agent-tasks.ps1").read_text(encoding="utf-8")
        wrapper = (ROOT / "tools/agent-infra/graphify-refresh.cmd").read_text(encoding="utf-8")
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        for value in (
            "git clone", "shaft.maintenanceOwner", ".shaft-nightly-maintenance.json",
            "SHAFT-Nightly-Knowledge-Refresh", "-AllowStartIfOnBatteries",
            "-StartWhenAvailable", "$legacyTasks[0] | Unregister-ScheduledTask",
            "--validate-only",
            "-c shaft.maintenanceOwner=",
            "Recovering installer-owned clone",
            "--validate-paths-only",
            "GIT_OPTIONAL_LOCKS",
            ".shaft-install-pending.json",
            "exclusive-maintenance-home-v1",
            "Assert-ExclusiveMaintenanceAcl",
            "SetAccessRuleProtection($true, $false)",
            "S-1-5-18",
            "S-1-5-32-544",
            "$Rules.Count -ne 3",
            "Controller",
        ):
            self.assertIn(value, installer)
        lines = [line.strip() for line in installer.splitlines() if line.strip()]
        proof_commands = [
            index for index, line in enumerate(lines)
            if line.startswith(("git clone ", "$head = (git ", "$remoteLine = (git "))
        ]
        self.assertTrue(proof_commands)
        for index in proof_commands:
            self.assertIn("$LASTEXITCODE", lines[index + 1])
        self.assertNotIn("RepositoryRoot", installer)
        self.assertIn("%~dp0shaft_knowledge_refresh.py", wrapper)
        self.assertIn('set "SHAFT_MAINTENANCE_HOME=%~dp0..\\.."', wrapper)
        self.assertNotIn("tools\\agent-infra\\shaft_knowledge_refresh.py", wrapper)
        self.assertIn("shaft-knowledge-refresh.previous.log", wrapper)
        self.assertIn("tests.scripts.test_shaft_knowledge_refresh", workflow)
        self.assertIn("tools/agent-infra/shaft_knowledge_refresh.py", workflow)
        preflight = installer.index("--validate-home-only")
        first_write = installer.index("New-ExclusiveDirectory -Path $lexicalHome")
        acl = installer.index("Assert-ExclusiveMaintenanceAcl $homePath")
        register = installer.index("Register-ScheduledTask")
        legacy_remove = installer.index("Unregister-ScheduledTask")
        self.assertLess(preflight, first_write)
        controller_root = installer.index("$controllerRoot = Join-Path $homePath 'Controller'")
        controller_root_create = installer.index(
            "New-ExclusiveDirectory -Path $controllerRoot", controller_root
        )
        controller_bundle = installer.index(
            "$controllerHome = Install-ControllerBundle", controller_root
        )
        self.assertLess(controller_root, controller_root_create)
        self.assertLess(controller_root_create, controller_bundle)
        install_function = installer[
            installer.index("function Install-ControllerBundle"):
            installer.index("if ($SecureDirectorySelfTest)")
        ]
        self.assertLess(install_function.index("New-ExclusiveDirectory -Path $staging"),
                        install_function.index("Copy-Item -LiteralPath $ControllerSource"))
        self.assertLess(install_function.index("Assert-ControllerBundle -BundleHome $staging"),
                        install_function.index("[IO.Directory]::Move($staging, $final)"))
        self.assertLess(acl, controller_bundle)
        self.assertLess(register, legacy_remove)
        self.assertIn("$legacyTasks = @(Get-ScheduledTask -ErrorAction Stop", installer)
        self.assertIn("$legacyTasks.Count -gt 1", installer)
        self.assertIn(
            "$legacyTasks[0] | Unregister-ScheduledTask -Confirm:$false -ErrorAction Stop",
            installer,
        )
        lock = installer.index("Invoke-WithInstallerLock $homePath {")
        logs = installer.index("New-Item -ItemType Directory -Force $logs")
        self.assertLess(lock, logs)
        self.assertLess(legacy_remove, installer.rindex("\n}"))
        pending_write = installer.index("Write-Utf8NoBomJson $pendingReceipt $pending")
        clone = installer.index("git clone")
        sentinel_move = installer.index('Move-Item -LiteralPath "$sentinel.tmp"')
        pending_remove = installer.index("Remove-Item -LiteralPath $pending -Force")
        self.assertLess(pending_write, clone)
        self.assertLess(clone, sentinel_move)
        self.assertLess(sentinel_move, pending_remove)
        for setting in (
            "-RunOnlyIfNetworkAvailable", "-MultipleInstances IgnoreNew",
            "-ExecutionTimeLimit (New-TimeSpan -Hours 3)", "-RestartCount 3",
            "-RestartInterval (New-TimeSpan -Minutes 15)",
            "-LogonType Interactive -RunLevel Limited",
        ):
            self.assertIn(setting, installer)

        self.assertNotIn("utf8NoBOM", installer)
        self.assertIn("Write-Utf8NoBomJson", installer)

    @unittest.skipUnless(os.name == "nt", "PowerShell bundle identity is Windows-only")
    def test_bundle_identity_changes_with_either_complete_input_hash(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        results = {}
        for fixture in ("baseline", "controller", "wrapper"):
            completed = subprocess.run(  # nosec B603 - repository-owned script in self-test mode.
                [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
                 "-BundleHashSelfTest", fixture],
                check=True,
                capture_output=True,
                text=True,
            )
            results[fixture] = completed.stdout.strip()
        self.assertEqual(3, len(set(results.values())))
        for value in results.values():
            self.assertRegex(value, r"^[0-9A-F]{32}$")

    @unittest.skipUnless(os.name == "nt", "Protected directory creation is Windows-only")
    def test_protected_directory_is_created_with_the_final_acl(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        target = Path(tempfile.mkdtemp())
        target.rmdir()
        try:
            completed = subprocess.run(  # nosec B603 - disposable D: fixture.
                [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
                 "-SecureDirectorySelfTest", str(target)],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
            self.assertEqual("True", completed.stdout.strip())
            self.assertTrue(target.is_dir())
        finally:
            shutil.rmtree(target, ignore_errors=True)

    @unittest.skipUnless(os.name == "nt", "PowerShell ACL predicate is Windows-only")
    def test_acl_predicate_accepts_only_the_exact_owner_and_rule_set(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        results = {}
        for fixture in ("valid", "foreign", "duplicate", "wrong-owner", "unprotected"):
            completed = subprocess.run(  # nosec B603 - repository-owned script in self-test mode.
                [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
                 "-AclPredicateSelfTest", fixture],
                check=True,
                capture_output=True,
                text=True,
            )
            results[fixture] = completed.stdout.strip()
        self.assertEqual("True", results["valid"])
        self.assertEqual("False", results["foreign"])
        self.assertEqual("False", results["duplicate"])
        self.assertEqual("False", results["wrong-owner"])
        self.assertEqual("False", results["unprotected"])

    @unittest.skipUnless(os.name == "nt", "Bundle promotion is Windows-only")
    def test_bundle_promotion_recovers_from_an_orphaned_staging_directory(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        target = Path(tempfile.mkdtemp())
        try:
            completed = subprocess.run(  # nosec B603 - disposable D: fixture.
                [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
                 "-BundlePromotionSelfTest", str(target)],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
            self.assertEqual("True", completed.stdout.strip())
            self.assertFalse(any(target.rglob("*.staging-*")))
        finally:
            shutil.rmtree(target, ignore_errors=True)

    @unittest.skipUnless(os.name == "nt", "PowerShell binding is Windows-only")
    def test_unknown_installer_argument_fails_before_the_first_write(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        target = Path(tempfile.mkdtemp()) / "must-not-exist"
        completed = subprocess.run(  # nosec B603 - negative binding probe.
            [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
             "-MaintenanceHome", str(target), "-AccidentalUndeclaredSelfTest", "value"],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertNotEqual(0, completed.returncode)
        self.assertFalse(target.exists())

    @unittest.skipUnless(os.name == "nt", "Installer lock is Windows-only")
    def test_installer_lock_contention_fails_before_owned_mutation(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        parent = Path(tempfile.mkdtemp())
        target = parent / "lock-home"
        first = subprocess.Popen(  # nosec B603 - disposable D: contention fixture.
            [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
             "-InstallerLockSelfTest", str(target), "-InstallerLockHoldMilliseconds", "4000"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        try:
            lock = target / ".shaft-install.lock"
            for _ in range(80):
                if lock.exists():
                    break
                import time
                time.sleep(0.05)
            self.assertTrue(lock.exists())
            second = subprocess.run(  # nosec B603 - same disposable fixture.
                [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
                 "-InstallerLockSelfTest", str(target)],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertNotEqual(0, second.returncode)
            stdout, stderr = first.communicate(timeout=10)
            self.assertEqual(0, first.returncode, stdout + stderr)
        finally:
            if first.poll() is None:
                first.kill()
            shutil.rmtree(parent, ignore_errors=True)

    @unittest.skipUnless(os.name == "nt", "Installer lock recovery is Windows-only")
    def test_installer_lock_is_released_after_same_host_failure(self):
        powershell = shutil.which("pwsh.exe") or shutil.which("powershell.exe")
        self.assertIsNotNone(powershell)
        installer = ROOT / "tools/agent-infra/install-agent-tasks.ps1"
        target = Path(tempfile.mkdtemp())
        target.rmdir()
        try:
            completed = subprocess.run(  # nosec B603 - disposable same-host fixture.
                [powershell, "-NoProfile", "-NonInteractive", "-File", str(installer),
                 "-InstallerLockFailureSelfTest", str(target)],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(0, completed.returncode, completed.stdout + completed.stderr)
            self.assertEqual("True", completed.stdout.strip())
        finally:
            shutil.rmtree(target, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
