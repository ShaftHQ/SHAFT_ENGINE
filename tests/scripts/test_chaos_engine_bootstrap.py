"""Latest-main ChaosEngine bootstrap acceptance tests (#4798)."""

from __future__ import annotations

import importlib.util
import io
import json
import subprocess  # nosec B404 - tests run fixed local Git commands only.
import tempfile
import unittest
import unittest.mock as mock
import urllib.error
from pathlib import Path
from types import SimpleNamespace
from urllib.parse import unquote, urlparse

from tests.scripts.test_chaos_engine_install_wrappers import (
    has_implicit_string_concat,
    source_segment_for_function,
)


ROOT = Path(__file__).resolve().parents[2]
BOOTSTRAP = ROOT / "chaos-engine/bootstrap.py"
COMMIT_ONE = "1" * 40
COMMIT_TWO = "2" * 40


def load():
    specification = importlib.util.spec_from_file_location("chaos_engine_bootstrap", BOOTSTRAP)
    if specification is None or specification.loader is None:
        raise RuntimeError("bootstrap test module could not be loaded")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module


def source_payload(marker: str) -> dict[str, bytes]:
    payload = {
        path.relative_to(ROOT / "chaos-engine").as_posix(): path.read_bytes()
        for path in (ROOT / "chaos-engine").rglob("*")
        if path.is_file() and "__pycache__" not in path.parts and path.suffix != ".pyc"
    }
    if marker:
        payload["bootstrap-marker.txt"] = marker.encode()
    return payload


class Response(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *_args):
        self.close()


class ChaosEngineBootstrapTest(unittest.TestCase):
    def test_documented_command_contains_the_bounded_initial_fetch_contract(self):
        windows = 'irm "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.ps1" | iex'
        posix = (
            'curl -fsSL "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh"'
            + ' | bash -s -- "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh"'
        )
        for relative in ("chaos-engine/README.md", "chaos-engine/INSTALL.md"):
            document = ROOT.joinpath(relative).read_text(encoding="utf-8")
            self.assertIn(windows, document)
            self.assertIn(posix, document)
            self.assertIn("CHAOS_ENGINE_REPOSITORY", document)
            self.assertNotIn("haftHQ", document)
            self.assertNotIn("HAFT_ENGINE", document)
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        self.assertIn("CHAOS_ENGINE_REPOSITORY", powershell)
        self.assertIn("CHAOS_ENGINE_REPOSITORY", shell)
        self.assertIn("bootstrap.py", powershell)
        self.assertIn("bootstrap.py", shell)
        self.assertIn("--project", powershell)
        self.assertIn("--project", shell)
        self.assertIn("for ($attempt = 0; $attempt -lt 4; $attempt++)", powershell)
        self.assertIn("curl -fsSL --retry 3", shell)

    def test_documented_posix_one_liner_uses_explicit_concatenation(self):
        """#5253: adjacent literals in the documented POSIX one-liner go red."""
        source = Path(__file__).read_text(encoding="utf-8")
        snippet = source_segment_for_function(
            source, "test_documented_command_contains_the_bounded_initial_fetch_contract"
        )
        self.assertTrue(
            has_implicit_string_concat("X = (\n    'a'\n    'b'\n)\n"),
            "detector must catch adjacent literals",
        )
        self.assertFalse(has_implicit_string_concat("X = (\n    'a'\n    + 'b'\n)\n"))
        self.assertFalse(
            has_implicit_string_concat(snippet),
            "documented POSIX one-liner must use explicit + or one line",
        )

    def test_posix_copy_gate_calls_source_tree_helper_after_resolve_only(self):
        """#5232: a helper definition alone must not keep leftover-copy CI green."""
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        self.assertIn("CHAOS_ENGINE_RESOLVE_ONLY", shell)
        after_resolve = shell.split("CHAOS_ENGINE_RESOLVE_ONLY", 1)[1]
        self.assertIn("is_chaos_engine_source_tree", after_resolve)
        self.assertIn('cp "$script_dir/bootstrap.py"', after_resolve)

    def test_install_md_joins_the_installer_lf_attributes_sentence(self):
        """#5234: origin omit-list and LF-attributes stay one paragraph."""
        document = (ROOT / "chaos-engine/INSTALL.md").read_text(encoding="utf-8")
        self.assertIn(
            "into the adopter payload. The installer also merges receipt-bound LF attributes",
            document,
        )
        self.assertNotRegex(document, r"The\s*\n\s*\n\s*installer also merges")

    def test_read_response_retries_a_transient_http_failure(self):
        module = load()
        transient = urllib.error.HTTPError(
            "https://example.invalid/bootstrap.py",
            503,
            "Service Unavailable",
            {},
            None,
        )
        opener = mock.Mock(side_effect=(transient, Response(b"ready")))
        sleeper = mock.Mock()

        try:
            value = module.read_response(
                opener,
                "https://example.invalid/bootstrap.py",
                sleeper=sleeper,
            )
        except RuntimeError:
            value = b""

        self.assertEqual(b"ready", value)
        self.assertEqual(2, opener.call_count)
        sleeper.assert_called_once_with(module.RETRY_BASE_SECONDS)

    def test_read_response_closes_a_transient_http_error(self):
        module = load()
        body = io.BytesIO(b"temporary response")
        transient = urllib.error.HTTPError(
            "https://example.invalid/bootstrap.py",
            503,
            "Service Unavailable",
            {},
            body,
        )
        opener = mock.Mock(side_effect=(transient, Response(b"ready")))

        module.read_response(
            opener,
            "https://example.invalid/bootstrap.py",
            sleeper=mock.Mock(),
        )

        self.assertTrue(body.closed)

    def test_read_response_retries_a_connection_reset(self):
        module = load()
        opener = mock.Mock(side_effect=(ConnectionResetError("reset"), Response(b"ready")))
        sleeper = mock.Mock()

        try:
            value = module.read_response(
                opener,
                "https://example.invalid/bootstrap.py",
                sleeper=sleeper,
            )
        except RuntimeError:
            value = b""

        self.assertEqual(b"ready", value)
        self.assertEqual(2, opener.call_count)
        sleeper.assert_called_once_with(module.RETRY_BASE_SECONDS)

    def test_read_response_exhausts_bounded_transient_retries(self):
        module = load()
        failures = tuple(
            urllib.error.HTTPError(
                "https://example.invalid/bootstrap.py",
                503,
                "Service Unavailable",
                {},
                None,
            )
            for _ in range(module.MAX_READ_ATTEMPTS)
        )
        opener = mock.Mock(side_effect=failures)
        sleeper = mock.Mock()

        with self.assertRaisesRegex(RuntimeError, "resolve latest ChaosEngine"):
            module.read_response(
                opener,
                "https://example.invalid/bootstrap.py",
                sleeper=sleeper,
            )

        self.assertEqual(module.MAX_READ_ATTEMPTS, opener.call_count)
        self.assertEqual(
            [
                mock.call(module.RETRY_BASE_SECONDS),
                mock.call(module.RETRY_BASE_SECONDS * 2),
                mock.call(module.RETRY_BASE_SECONDS * 4),
            ],
            sleeper.call_args_list,
        )

    def test_read_response_does_not_retry_a_permanent_http_failure(self):
        module = load()
        opener = mock.Mock(
            side_effect=urllib.error.HTTPError(
                "https://example.invalid/missing.py",
                404,
                "Not Found",
                {},
                None,
            )
        )
        sleeper = mock.Mock()

        with self.assertRaisesRegex(RuntimeError, "resolve latest ChaosEngine"):
            module.read_response(
                opener,
                "https://example.invalid/missing.py",
                sleeper=sleeper,
            )

        opener.assert_called_once()
        sleeper.assert_not_called()

    def test_read_response_honors_bounded_retry_after(self):
        module = load()
        transient = urllib.error.HTTPError(
            "https://example.invalid/bootstrap.py",
            429,
            "Too Many Requests",
            {"Retry-After": "7"},
            None,
        )
        opener = mock.Mock(side_effect=(transient, Response(b"ready")))
        sleeper = mock.Mock()

        value = module.read_response(
            opener,
            "https://example.invalid/bootstrap.py",
            sleeper=sleeper,
        )

        self.assertEqual(b"ready", value)
        sleeper.assert_called_once_with(7.0)

    def test_read_response_honors_http_date_retry_after(self):
        module = load()
        transient = urllib.error.HTTPError(
            "https://example.invalid/bootstrap.py",
            429,
            "Too Many Requests",
            {"Retry-After": "Thu, 01 Jan 1970 00:00:05 GMT"},
            None,
        )
        opener = mock.Mock(side_effect=(transient, Response(b"ready")))
        sleeper = mock.Mock()

        with mock.patch.object(module.time, "time", return_value=0):
            try:
                value = module.read_response(
                    opener,
                    "https://example.invalid/bootstrap.py",
                    sleeper=sleeper,
                )
            except RuntimeError:
                value = b""

        self.assertEqual(b"ready", value)
        sleeper.assert_called_once_with(5.0)

    def test_read_response_retries_a_rate_limit_signaled_403(self):
        module = load()
        rate_limited = urllib.error.HTTPError(
            "https://example.invalid/bootstrap.py",
            403,
            "Forbidden",
            {"Retry-After": "5"},
            None,
        )
        opener = mock.Mock(side_effect=(rate_limited, Response(b"ready")))
        sleeper = mock.Mock()

        try:
            value = module.read_response(
                opener,
                "https://example.invalid/bootstrap.py",
                sleeper=sleeper,
            )
        except RuntimeError:
            value = b""

        self.assertEqual(b"ready", value)
        sleeper.assert_called_once_with(5.0)

    def test_documented_one_command_contains_valid_python_source(self):
        powershell = (ROOT / "chaos-engine/install.ps1").read_text(encoding="utf-8")
        shell = (ROOT / "chaos-engine/install.sh").read_text(encoding="utf-8")
        self.assertIn("Get-Location", powershell)
        self.assertIn('project="$(pwd)"', shell)
        self.assertNotIn("shaft", powershell.casefold())
        self.assertNotIn("shaft", shell.casefold())

    def test_public_full_flow_activates_clients_and_runs_doctor(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            opener, _ = self.opener([(COMMIT_ONE, "full")])
            installer = mock.Mock()
            installer.install_with_dependencies.return_value = project / ".chaos-engine"
            installer.load_installed_controller.return_value.activate_detected_plugins.return_value = {
                "createdPlugins": ["codex"]
            }
            installer.doctor_with_dependencies.return_value = {"status": "healthy"}

            with mock.patch.object(module, "load_installer", return_value=installer):
                result = module.install_latest(
                    project,
                    repository="Example/Project",
                    branch="main",
                    opener=opener,
                )

            installer.load_installed_controller.assert_called_once_with(
                project / ".chaos-engine", "hosts"
            )
            installer.load_installed_controller.return_value.activate_detected_plugins.assert_called_once_with(
                project.resolve()
            )
            installer.doctor_with_dependencies.assert_called_once_with(
                project.resolve(), verify_clients=False
            )
            self.assertEqual("healthy", result["doctor"]["status"])

    def test_failed_post_install_doctor_removes_new_activation_and_install(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            opener, _ = self.opener([(COMMIT_ONE, "full")])
            installer = mock.Mock()
            installer.install_with_dependencies.return_value = project / ".chaos-engine"
            host = installer.load_installed_controller.return_value
            host.activate_detected_plugins.return_value = {"createdPlugins": ["codex"]}
            installer.doctor_with_dependencies.side_effect = RuntimeError("probe failed")

            with mock.patch.object(module, "load_installer", return_value=installer):
                with self.assertRaisesRegex(RuntimeError, "probe failed"):
                    module.install_latest(
                        project,
                        repository="Example/Project",
                        branch="main",
                        opener=opener,
                    )

            host.activate_detected_plugins.assert_not_called()
            installer.uninstall_with_dependencies.assert_called_once_with(project.resolve())
            installer.rollback.assert_not_called()

    def test_failed_doctor_after_unverified_prior_tree_uninstalls_instead_of_rollback(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            (project / ".chaos-engine").mkdir()
            opener, _ = self.opener([(COMMIT_ONE, "full")])
            installer = mock.Mock()
            installer.install_with_dependencies.return_value = project / ".chaos-engine"
            installer.doctor_with_dependencies.side_effect = RuntimeError("probe failed")

            with mock.patch.object(module, "load_installer", return_value=installer):
                with self.assertRaisesRegex(RuntimeError, "probe failed"):
                    module.install_latest(
                        project,
                        repository="Example/Project",
                        branch="main",
                        opener=opener,
                    )

            installer.uninstall_with_dependencies.assert_called_once_with(project.resolve())
            installer.rollback.assert_not_called()

    def test_failed_doctor_after_prior_install_with_backup_rolls_back(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            (project / ".chaos-engine").mkdir()
            (project / ".chaos-engine.backup").mkdir()
            opener, _ = self.opener([(COMMIT_ONE, "full")])
            installer = mock.Mock()
            installer.install_with_dependencies.return_value = project / ".chaos-engine"
            installer.doctor_with_dependencies.side_effect = RuntimeError("probe failed")

            with mock.patch.object(module, "load_installer", return_value=installer):
                with self.assertRaisesRegex(RuntimeError, "probe failed"):
                    module.install_latest(
                        project,
                        repository="Example/Project",
                        branch="main",
                        opener=opener,
                    )

            installer.rollback.assert_called_once_with(project.resolve())
            installer.uninstall_with_dependencies.assert_not_called()

    def opener(self, commits: list[tuple[str, str]]):
        calls: list[str] = []

        def open_url(request, timeout=0):
            del timeout
            url = request.full_url if hasattr(request, "full_url") else str(request)
            calls.append(url)
            commit, marker = commits[0]
            if "/commits/" in url:
                return Response(json.dumps({"sha": commit}).encode())
            payload = source_payload(marker)
            if "/git/trees/" in url:
                tree = [
                    {
                        "path": f"chaos-engine/{relative}",
                        "mode": "100644",
                        "type": "blob",
                        "size": len(content),
                    }
                    for relative, content in payload.items()
                ]
                return Response(json.dumps({"tree": tree, "truncated": False}).encode())
            self.assertEqual("raw.githubusercontent.com", urlparse(url).netloc)
            prefix = f"/{commit}/chaos-engine/"
            encoded_path = urlparse(url).path.split(prefix, 1)[1]
            return Response(payload[unquote(encoded_path)])

        return open_url, calls

    def test_clean_install_update_and_offline_failure_preserve_last_known_good(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            open_one, calls = self.opener([(COMMIT_ONE, "one")])

            first = module.install_latest(
                project,
                repository="ShaftHQ/SHAFT_ENGINE",
                branch="main",
                skip_tools=True,
                opener=open_one,
            )

            self.assertEqual(COMMIT_ONE, first["commit"])
            manifest = json.loads((project / ".chaos-engine/manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(
                {
                    "kind": "git-digest",
                    "repositorySha256": mock.ANY,
                    "branchSha256": mock.ANY,
                    "commit": COMMIT_ONE,
                },
                manifest["source"],
            )
            self.assertEqual("portable", manifest["distribution"]["id"])
            self.assertNotIn("shaft", (project / ".chaos-engine/manifest.json").read_text().casefold())
            self.assertTrue(any("api.github.com/repos/ShaftHQ/SHAFT_ENGINE/commits/main" in call for call in calls))

            open_two, _ = self.opener([(COMMIT_TWO, "two")])
            updated = module.install_latest(
                project,
                repository="ShaftHQ/SHAFT_ENGINE",
                branch="main",
                skip_tools=True,
                opener=open_two,
            )
            self.assertEqual(COMMIT_TWO, updated["commit"])

            before = (project / ".chaos-engine/manifest.json").read_bytes()
            with self.assertRaisesRegex(RuntimeError, "resolve latest ChaosEngine"):
                module.install_latest(
                    project,
                    repository="ShaftHQ/SHAFT_ENGINE",
                    branch="main",
                    skip_tools=True,
                    opener=mock.Mock(side_effect=OSError("offline")),
                )
            self.assertEqual(before, (project / ".chaos-engine/manifest.json").read_bytes())

    def test_public_default_path_installs_healthy_hosts_and_local_tools(self):
        module = load()
        dependency = importlib.util.spec_from_file_location(
            "bootstrap_dependency", ROOT / "chaos-engine/dependencies.py"
        )
        if dependency is None or dependency.loader is None:
            raise RuntimeError("dependency test module could not be loaded")
        dependency_module = importlib.util.module_from_spec(dependency)
        dependency.loader.exec_module(dependency_module)
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            opener, _ = self.opener([(COMMIT_ONE, "full")])

            def provisioner(runtime, specification):
                def runner(command, environment):
                    del environment
                    executable = Path(command[0])
                    if not executable.exists() and executable.is_relative_to(runtime.parent):
                        executable.parent.mkdir(parents=True, exist_ok=True)
                        executable.write_text("tool\n", encoding="utf-8")
                    return SimpleNamespace(stdout="tool 1.0\n", stderr="")

                return dependency_module.repair(runtime, specification, runner=runner)

            result = module.install_latest(
                project,
                repository="Example/Project",
                branch="main",
                opener=opener,
                provisioner=provisioner,
            )

            installed = module.load_installer(project / ".chaos-engine")
            status = installed.status_with_dependencies(project)
            self.assertEqual(COMMIT_ONE, result["commit"])
            self.assertEqual("healthy", status["status"])
            self.assertEqual("healthy", status["hosts"]["status"])
            self.assertEqual("healthy", status["dependencies"]["status"])
            self.assertEqual(
                {
                    "core",
                    "skills",
                    "playbooks",
                    "hooks",
                    "plugins",
                    "roles",
                    "mcps",
                    "tools",
                    "memory",
                    "mempalace",
                    "graphify",
                    "maven-tools-mcp",
                    "retrieval-config",
                    "projection-policy",
                },
                set(status["components"]),
            )
            self.assertEqual("absent", status["components"]["maven-tools-mcp"]["status"])
            self.assertTrue(all(
                component["status"] == "healthy"
                for name, component in status["components"].items()
                if name != "maven-tools-mcp"
            ))
            self.assertTrue(project.joinpath(".agents/skills/chaos-engine/SKILL.md").is_file())
            self.assertTrue(project.joinpath(".mcp.json").is_file())

            same, _ = self.opener([(COMMIT_ONE, "full")])
            repeated = module.install_latest(
                project,
                repository="example/project",
                branch="main",
                opener=same,
                provisioner=provisioner,
            )
            self.assertEqual(COMMIT_ONE, repeated["commit"])

    def test_existing_local_install_adopts_git_provenance_through_staged_update(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            installer = module.load_installer(ROOT / "chaos-engine")
            installer.install(project, ROOT / "chaos-engine", COMMIT_ONE)
            before = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("local", before["source"]["kind"])
            opener, _ = self.opener([(COMMIT_ONE, "")])

            result = module.install_latest(
                project,
                repository="Example/Project",
                branch="main",
                skip_tools=True,
                opener=opener,
            )

            self.assertEqual(COMMIT_ONE, result["commit"])
            current = json.loads(
                (project / ".chaos-engine/manifest.json").read_text(encoding="utf-8")
            )
            backup = json.loads(
                (project / ".chaos-engine.backup/manifest.json").read_text(encoding="utf-8")
            )
            self.assertEqual("git-digest", current["source"]["kind"])
            self.assertNotIn("repository", current["source"])
            self.assertEqual(before, backup)

    def test_failed_full_migration_restores_the_prior_local_manifest(self):
        module = load()
        with tempfile.TemporaryDirectory() as temporary:
            project = Path(temporary) / "project"
            project.mkdir()
            installer = module.load_installer(ROOT / "chaos-engine")
            installer.install(project, ROOT / "chaos-engine", COMMIT_ONE)
            manifest_path = project / ".chaos-engine/manifest.json"
            before = manifest_path.read_bytes()
            opener, _ = self.opener([(COMMIT_ONE, "")])

            with self.assertRaisesRegex(RuntimeError, "offline"):
                module.install_latest(
                    project,
                    repository="Example/Project",
                    branch="main",
                    opener=opener,
                    provisioner=lambda *_args: (_ for _ in ()).throw(RuntimeError("offline")),
                )

            self.assertEqual(before, manifest_path.read_bytes())

    def test_github_other_git_and_non_git_project_roots_use_the_same_flow(self):
        module = load()
        for kind in ("github", "other-git", "non-git"):
            with self.subTest(kind=kind), tempfile.TemporaryDirectory() as temporary:
                project = Path(temporary) / "project"
                project.mkdir()
                if kind != "non-git":
                    subprocess.run(  # nosec B603 B607 - fixed local Git fixture command.
                        ["git", "init", "--quiet"], cwd=project, check=True
                    )
                    remote = (
                        "https://github.com/example/project.git"
                        if kind == "github"
                        else "https://git.example/project.git"
                    )
                    subprocess.run(  # nosec B603 B607 - fixed local Git fixture command.
                        ["git", "remote", "add", "origin", remote], cwd=project, check=True
                    )
                opener, _ = self.opener([(COMMIT_ONE, kind)])

                result = module.install_latest(
                    project,
                    repository="ShaftHQ/SHAFT_ENGINE",
                    branch="main",
                    skip_tools=True,
                    opener=opener,
                )

                self.assertEqual(COMMIT_ONE, result["commit"])
                self.assertTrue((project / ".chaos-engine/skills/chaos-engine/SKILL.md").is_file())

    def test_omitted_branch_uses_the_configured_upstream_default(self):
        module = load()
        responses = iter(
            (
                Response(json.dumps({"default_branch": "trunk"}).encode()),
                Response(json.dumps({"sha": COMMIT_ONE}).encode()),
            )
        )
        calls: list[str] = []

        def opener(request, **_kwargs):
            calls.append(request.full_url)
            return next(responses)

        commit, branch = module.resolve_latest("example/project", None, opener=opener)

        self.assertEqual((COMMIT_ONE, "trunk"), (commit, branch))
        self.assertEqual("https://api.github.com/repos/example/project", calls[0])
        self.assertTrue(calls[1].endswith("/commits/trunk"))

    def test_repository_dot_segments_are_rejected_before_network(self):
        module = load()
        for repository in ("../repo", "owner/..", "./repo", "owner/."):
            with self.subTest(repository=repository):
                opener = mock.Mock()
                with self.assertRaisesRegex(ValueError, "owner/repository"):
                    module.resolve_latest(repository, "main", opener=opener)
                opener.assert_not_called()

    def test_manifest_owner_rejects_unresolvable_git_provenance(self):
        module = load()
        installer = module.load_installer(ROOT / "chaos-engine")
        invalid = (
            {"kind": "git", "repository": "owner/..", "branch": "main", "commit": COMMIT_ONE},
            {"kind": "git", "repository": "not a repo", "branch": "../main", "commit": COMMIT_ONE},
        )
        for source_record in invalid:
            with self.subTest(source_record=source_record), tempfile.TemporaryDirectory() as temporary:
                project = Path(temporary)
                with self.assertRaisesRegex(ValueError, "source record"):
                    installer.install(
                        project,
                        ROOT / "chaos-engine",
                        COMMIT_ONE,
                        source_record=source_record,
                    )
                self.assertFalse((project / ".chaos-engine").exists())

    def test_git_invalid_branches_are_rejected_by_bootstrap_and_manifest_owner(self):
        module = load()
        installer = module.load_installer(ROOT / "chaos-engine")
        for branch in (
            "/main",
            "main/",
            "main//child",
            "main.",
            "main.lock",
            "HEAD",
            "main\x7fhidden",
        ):
            with self.subTest(branch=branch), tempfile.TemporaryDirectory() as temporary:
                opener = mock.Mock()
                with self.assertRaisesRegex(ValueError, "branch is invalid"):
                    module.resolve_latest("example/project", branch, opener=opener)
                opener.assert_not_called()
                with self.assertRaisesRegex(ValueError, "source record"):
                    installer.install(
                        Path(temporary),
                        ROOT / "chaos-engine",
                        COMMIT_ONE,
                        source_record={
                            "kind": "git",
                            "repository": "example/project",
                            "branch": branch,
                            "commit": COMMIT_ONE,
                        },
                    )

        opener = mock.Mock(return_value=Response(json.dumps({"sha": COMMIT_ONE}).encode()))
        self.assertEqual((COMMIT_ONE, "@"), module.resolve_latest("example/project", "@", opener=opener))

    def test_source_tree_escape_truncation_and_unexpected_layout_are_rejected(self):
        module = load()
        cases = (
            ({"tree": [{"path": "chaos-engine/../escape.txt"}], "truncated": False}, "unsafe"),
            ({"tree": [{"path": "unexpected/file.txt"}], "truncated": False}, "layout"),
            ({"tree": [], "truncated": True}, "incomplete"),
        )
        for tree, message in cases:
            with self.subTest(message=message), tempfile.TemporaryDirectory() as temporary:
                responses = iter(
                    (
                        Response(json.dumps({"sha": COMMIT_ONE}).encode()),
                        Response(json.dumps(tree).encode()),
                    )
                )

                with self.assertRaisesRegex(ValueError, message):
                    module.install_latest(
                        Path(temporary),
                        repository="ShaftHQ/SHAFT_ENGINE",
                        branch="main",
                        skip_tools=True,
                        opener=lambda *_args, **_kwargs: next(responses),
                    )

                self.assertFalse((Path(temporary) / ".chaos-engine").exists())

    def test_download_source_omits_origin_brand_masters(self):
        module = load()
        tree = {
            "truncated": False,
            "tree": [
                {"path": "chaos-engine/skills/chaos-engine/SKILL.md", "type": "blob", "mode": "100644", "size": 5},
                {"path": "chaos-engine/assets/brand/symbol-light.svg", "type": "blob", "mode": "100644", "size": 5},
                {"path": "chaos-engine/RESEARCH.md", "type": "blob", "mode": "100644", "size": 8},
                {"path": "chaos-engine/STANDALONE.md", "type": "blob", "mode": "100644", "size": 9},
                {"path": "chaos-engine/assets/memory-v5/config.schema.json", "type": "blob", "mode": "100644", "size": 6},
            ],
        }
        requested: list[str] = []

        def opener(request, timeout=0):
            del timeout
            url = request.full_url if hasattr(request, "full_url") else request
            requested.append(str(url))
            if "git/trees" in str(url):
                return Response(json.dumps(tree).encode())
            if str(url).endswith("SKILL.md"):
                return Response(b"skill")
            if str(url).endswith("symbol-light.svg"):
                return Response(b"brand")
            if str(url).endswith("RESEARCH.md"):
                return Response(b"research")
            if str(url).endswith("STANDALONE.md"):
                return Response(b"standalone")
            if str(url).endswith("config.schema.json"):
                return Response(b"schema")
            raise AssertionError(url)

        with tempfile.TemporaryDirectory() as temporary:
            source = module.download_source("owner/repo", COMMIT_ONE, Path(temporary), opener=opener)

            self.assertTrue((source / "skills/chaos-engine/SKILL.md").is_file())
            self.assertTrue((source / "assets/memory-v5/config.schema.json").is_file())
            self.assertFalse((source / "assets/brand/symbol-light.svg").exists())
            self.assertFalse((source / "RESEARCH.md").exists())
            self.assertFalse((source / "STANDALONE.md").exists())
            self.assertFalse(
                any(
                    "symbol-light.svg" in url
                    or url.endswith("RESEARCH.md")
                    or url.endswith("STANDALONE.md")
                    for url in requested
                    if "git/trees" not in url
                )
            )

    def test_bootstrap_is_reachable_and_runs_in_three_os_ci(self):
        skill = (ROOT / "chaos-engine/skills/chaos-engine/SKILL.md").read_text(encoding="utf-8")
        workflow = (ROOT / ".github/workflows/pr-gate.yml").read_text(encoding="utf-8")
        budget = json.loads((ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8"))

        self.assertIn("../../bootstrap.py", skill)
        self.assertIn("tests/scripts/test_chaos_engine_bootstrap.py", skill)
        self.assertIn("tests.scripts.test_chaos_engine_bootstrap", workflow)
        for os_name in ("ubuntu-22.04", "macos-15", "windows-2025"):
            self.assertIn(os_name, workflow)
        self.assertIn("tests/scripts/test_chaos_engine_bootstrap.py", budget["harness_reachability"]["element_globs"])


if __name__ == "__main__":
    unittest.main()
