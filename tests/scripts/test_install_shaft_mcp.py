import ast
import importlib.util
import contextlib
import hashlib
import io
import json
import os
import re
import ssl
import subprocess  # nosec B404 -- Windows junction test uses resolved System32 cmd.exe only.
import sys
import tempfile
import tomllib
import unittest
import urllib.error
import zipfile
from pathlib import Path
from unittest import mock

MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "mcp" / "install_shaft_agentic_tools.py"
REPO_ROOT = MODULE_PATH.parents[2]
SPEC = importlib.util.spec_from_file_location("install_shaft_mcp", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
SPEC.loader.exec_module(MODULE)


@contextlib.contextmanager
def temporary_environment(**values):
    original = {name: os.environ.get(name) for name in values}
    os.environ.update(values)
    try:
        yield
    finally:
        for name, value in original.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value


@contextlib.contextmanager
def temporary_current_directory(path: Path):
    original = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(original)


class _InteractiveStdin(io.StringIO):
    """A StringIO that reports isatty() == True, so choose_client() reads
    scripted answers via input() instead of failing the non-interactive check."""

    def isatty(self) -> bool:
        return True


@contextlib.contextmanager
def scripted_stdin(answer: str):
    original_stdin = MODULE.sys.stdin
    MODULE.sys.stdin = _InteractiveStdin(f"{answer}\n")
    try:
        yield
    finally:
        MODULE.sys.stdin = original_stdin


@contextlib.contextmanager
def isolated_grok_env(root: Path, grok_home: Path | None = None, cwd: Path | None = None):
    grok_home = grok_home or (root / "grok-home")
    fake_home = root / "home"
    cwd = cwd or (root / "cwd")
    grok_home.mkdir(parents=True, exist_ok=True)
    fake_home.mkdir(parents=True, exist_ok=True)
    cwd.mkdir(parents=True, exist_ok=True)
    with temporary_environment(GROK_HOME=str(grok_home)), mock.patch.object(
        MODULE, "home", return_value=fake_home
    ), temporary_current_directory(cwd):
        yield grok_home, fake_home, cwd


class InstallShaftMcpTest(unittest.TestCase):
    def test_banner_is_not_repeated_after_bootstrap_banner(self):
        with temporary_environment(SHAFT_MCP_BOOTSTRAP_BANNER_SHOWN="1"):
            stderr = io.StringIO()
            with contextlib.redirect_stderr(stderr):
                MODULE.banner()

        self.assertEqual("", stderr.getvalue())

    def test_parse_runtime_dependency_manifest(self):
        manifest = (
            "The following files have been resolved:\n"
            "   org.example:runtime:jar:1.0.0:runtime -- module runtime\n"
            "   org.example:runtime:jar:1.0.0:runtime -- duplicate\n"
            "   org.example:native:jar:linux-x64:2.0.0:runtime\n"
            "   org.example:test-only:jar:1.0.0:test\n"
            "   org.example:pom-only:pom:1.0.0:runtime\n"
        )

        self.assertEqual(
            MODULE.parse_runtime_dependency_manifest(manifest),
            [
                ("org.example", "runtime", "1.0.0", None),
                ("org.example", "native", "2.0.0", "linux-x64"),
            ],
        )

    def test_parse_intellij_plugin_json_target(self):
        args = MODULE.parse_args(["--intellij-plugin", "--json"])

        self.assertEqual("intellij-plugin", args.client)
        self.assertTrue(args.json)

    def test_parse_rejects_conflicting_shaft_skills_flags(self):
        with self.assertRaises(MODULE.InstallError):
            MODULE.parse_args(["--codex", "--install-shaft-skills", "--skip-shaft-skills"])

    def test_parse_skills_only_without_client_on_non_interactive_stdin(self):
        original_stdin = MODULE.sys.stdin
        MODULE.sys.stdin = io.StringIO("")
        try:
            args = MODULE.parse_args(["--install-shaft-skills"])
        finally:
            MODULE.sys.stdin = original_stdin

        self.assertIsNone(args.client)
        self.assertFalse(args.install_mcp)
        self.assertTrue(args.install_shaft_skills)

    def test_parse_cli_only_without_client_on_non_interactive_stdin(self):
        original_stdin = MODULE.sys.stdin
        MODULE.sys.stdin = io.StringIO("")
        try:
            args = MODULE.parse_args(["--install-shaft-cli"])
        finally:
            MODULE.sys.stdin = original_stdin

        self.assertIsNone(args.client)
        self.assertFalse(args.install_mcp)
        self.assertTrue(args.install_shaft_cli)

    def test_parse_without_selector_requires_one_on_non_interactive_stdin(self):
        original_stdin = MODULE.sys.stdin
        MODULE.sys.stdin = io.StringIO("")
        try:
            with self.assertRaises(MODULE.InstallError) as failure:
                MODULE.parse_args([])
        finally:
            MODULE.sys.stdin = original_stdin

        self.assertIn("selector", str(failure.exception).lower())

    def test_parse_without_selector_interactively_asks_each_component(self):
        with scripted_stdin("\nn\n\n1"):
            args = MODULE.parse_args([])

        self.assertTrue(args.install_mcp)
        self.assertFalse(args.install_shaft_cli)
        self.assertTrue(args.install_shaft_skills)
        self.assertEqual("codex", args.client)

    def test_intellij_plugin_target_does_not_configure_external_client(self):
        MODULE.configure_client("intellij-plugin", Path("java"), Path("shaft-mcp.args"))

    def test_parse_accepts_grok_client(self):
        args = MODULE.parse_args(["--client", "grok"])

        self.assertEqual("grok", args.client)
        self.assertIn("grok", MODULE.TARGETS)

    def test_parse_accepts_grok_flag(self):
        args = MODULE.parse_args(["--grok"])

        self.assertEqual("grok", args.client)

    def test_configuration_path_grok_uses_grok_home(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            grok_home = Path(temp_dir) / "grok-home"
            with temporary_environment(GROK_HOME=str(grok_home)):
                self.assertEqual(grok_home / "config.toml", MODULE.configuration_path("grok"))

    def test_configure_grok_writes_shaft_mcp_and_preserves_siblings(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                config = grok_home / "config.toml"
                config.write_text(
                    "[mcp_servers.other]\n"
                    'command = "npx"\n'
                    'api_key = "secret-do-not-touch"\n'
                    "\n"
                    "[ui]\n"
                    'permission_mode = "ask"\n',
                    encoding="utf-8",
                )
                java = root / "java"
                args_file = root / "shaft-mcp.args"
                MODULE.configure_grok(java, args_file)

            text = config.read_text(encoding="utf-8")
            self.assertIn("[mcp_servers.other]", text)
            self.assertIn('api_key = "secret-do-not-touch"', text)
            self.assertIn('permission_mode = "ask"', text)
            parsed = tomllib.loads(text)
            entry = parsed["mcp_servers"]["shaft-mcp"]
            self.assertEqual(str(java), entry["command"])
            self.assertEqual([f"@{args_file}"], entry["args"])
            self.assertEqual("npx", parsed["mcp_servers"]["other"]["command"])
            self.assertEqual("secret-do-not-touch", parsed["mcp_servers"]["other"]["api_key"])

    def test_configure_grok_is_idempotent(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            java = root / "java"
            args_file = root / "shaft-mcp.args"
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                MODULE.configure_grok(java, args_file)
                first = (grok_home / "config.toml").read_text(encoding="utf-8")
                MODULE.configure_grok(java, args_file)
                second = (grok_home / "config.toml").read_text(encoding="utf-8")
            first_entry = tomllib.loads(first)["mcp_servers"]["shaft-mcp"]
            second_entry = tomllib.loads(second)["mcp_servers"]["shaft-mcp"]
            self.assertEqual(first_entry["command"], second_entry["command"])
            self.assertEqual(first_entry["args"], second_entry["args"])

    def test_configure_grok_fails_closed_on_invalid_toml(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            original = "this is not toml [\n"
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                config = grok_home / "config.toml"
                config.write_text(original, encoding="utf-8")
                with self.assertRaises(MODULE.InstallError):
                    MODULE.configure_grok(Path("java"), Path("shaft-mcp.args"))
            self.assertEqual(original, config.read_text(encoding="utf-8"))

    def test_grok_skills_use_agents_directory(self):
        self.assertEqual((".agents/skills",), MODULE.SHAFT_SKILLS_NATIVE_DIRECTORIES["grok"])

    def test_project_candidates_grok_use_repo_config(self):
        directory = Path("repo")
        self.assertEqual([directory / ".grok" / "config.toml"], MODULE.project_candidates(directory, "grok"))

    def test_configure_grok_creates_config_when_missing_or_empty(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            java = root / "java"
            args_file = root / "shaft-mcp.args"
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                MODULE.configure_grok(java, args_file)
                empty = root / "empty-home"
                empty.mkdir()
                (empty / "config.toml").write_text("", encoding="utf-8")
                with isolated_grok_env(root, grok_home=empty):
                    MODULE.configure_grok(java, args_file)
            parsed = tomllib.loads((grok_home / "config.toml").read_text(encoding="utf-8"))
            empty_parsed = tomllib.loads((empty / "config.toml").read_text(encoding="utf-8"))
            self.assertEqual(str(java), parsed["mcp_servers"]["shaft-mcp"]["command"])
            self.assertTrue(parsed["mcp_servers"]["shaft-mcp"]["enabled"])
            self.assertEqual(str(java), empty_parsed["mcp_servers"]["shaft-mcp"]["command"])

    def test_configure_grok_fails_closed_on_inline_assignment(self):
        original = (
            'mcp_servers.shaft-mcp = { command = "old", args = ["@old"] }\n'
            'mcp_servers.other = { command = "npx", api_key = "secret-do-not-touch" }\n'
            'model = "keep-me"\n'
            "\n"
            "[ui]\n"
            'permission_mode = "ask"\n'
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                config = grok_home / "config.toml"
                config.write_text(original, encoding="utf-8")
                with self.assertRaises(MODULE.InstallError):
                    MODULE.configure_grok(Path("java"), Path("shaft-mcp.args"))
            self.assertEqual(original, config.read_text(encoding="utf-8"))

    def test_configure_grok_fails_closed_on_nested_table(self):
        original = (
            "[mcp_servers]\n"
            'shaft-mcp = { command = "old", args = ["@old"] }\n'
            'other = { command = "npx" }\n'
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                config = grok_home / "config.toml"
                config.write_text(original, encoding="utf-8")
                with self.assertRaises(MODULE.InstallError):
                    MODULE.configure_grok(Path("java"), Path("shaft-mcp.args"))
            self.assertEqual(original, config.read_text(encoding="utf-8"))

    def test_configure_grok_updates_project_config_instead_of_user(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            grok_home = root / "user-grok"
            project = root / "project"
            project_config = project / ".grok" / "config.toml"
            project_config.parent.mkdir(parents=True)
            project_config.write_text(
                '[mcp_servers.shaft-mcp]\ncommand = "old"\nargs = ["@old"]\n',
                encoding="utf-8",
            )
            java = root / "java"
            args_file = root / "shaft-mcp.args"
            with isolated_grok_env(root, grok_home=grok_home, cwd=project):
                MODULE.configure_grok(java, args_file)
            parsed = tomllib.loads(project_config.read_text(encoding="utf-8"))
            self.assertEqual(str(java), parsed["mcp_servers"]["shaft-mcp"]["command"])
            self.assertEqual([f"@{args_file}"], parsed["mcp_servers"]["shaft-mcp"]["args"])
            self.assertFalse((grok_home / "config.toml").exists())

    def test_configure_grok_nested_project_form_fail_closes_without_user_write(self):
        original = (
            "[mcp_servers]\n"
            'shaft-mcp = { command = "old", args = ["@old"] }\n'
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            grok_home = root / "user-grok"
            project = root / "project"
            project_config = project / ".grok" / "config.toml"
            project_config.parent.mkdir(parents=True)
            project_config.write_text(original, encoding="utf-8")
            with isolated_grok_env(root, grok_home=grok_home, cwd=project):
                with self.assertRaises(MODULE.InstallError):
                    MODULE.configure_grok(root / "java", root / "shaft-mcp.args")
            self.assertEqual(original, project_config.read_text(encoding="utf-8"))
            self.assertFalse((grok_home / "config.toml").exists())

    def test_configure_grok_ignores_shaft_mcp_above_the_git_root(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            outside = root / "outside"
            repo = outside / "repo"
            (repo / ".git").mkdir(parents=True)
            outside_config = outside / ".grok" / "config.toml"
            outside_config.parent.mkdir(parents=True)
            outside_config.write_text(
                '[mcp_servers.shaft-mcp]\ncommand = "outside-old"\nargs = ["@outside-old"]\n',
                encoding="utf-8",
            )
            java = root / "java"
            args_file = root / "shaft-mcp.args"
            with isolated_grok_env(root, cwd=repo) as (grok_home, _home, _cwd):
                MODULE.configure_grok(java, args_file)
            parsed = tomllib.loads((grok_home / "config.toml").read_text(encoding="utf-8"))
            outside_parsed = tomllib.loads(outside_config.read_text(encoding="utf-8"))
            self.assertEqual(str(java), parsed["mcp_servers"]["shaft-mcp"]["command"])
            self.assertEqual("outside-old", outside_parsed["mcp_servers"]["shaft-mcp"]["command"])

    def test_configure_grok_prefers_grok_home_over_default_user_file(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            java = root / "java"
            args_file = root / "shaft-mcp.args"
            with isolated_grok_env(root) as (grok_home, fake_home, _cwd):
                default_user = fake_home / ".grok" / "config.toml"
                default_user.parent.mkdir(parents=True)
                default_user.write_text(
                    '[mcp_servers.shaft-mcp]\ncommand = "home-old"\nargs = ["@home-old"]\n',
                    encoding="utf-8",
                )
                MODULE.configure_grok(java, args_file)
            parsed = tomllib.loads((grok_home / "config.toml").read_text(encoding="utf-8"))
            home_parsed = tomllib.loads(default_user.read_text(encoding="utf-8"))
            self.assertEqual(str(java), parsed["mcp_servers"]["shaft-mcp"]["command"])
            self.assertEqual("home-old", home_parsed["mcp_servers"]["shaft-mcp"]["command"])

    def test_configure_grok_idempotent_keeps_siblings(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with isolated_grok_env(root) as (grok_home, _home, _cwd):
                config = grok_home / "config.toml"
                config.write_text(
                    "[mcp_servers.other]\n"
                    'command = "npx"\n'
                    'api_key = "secret-do-not-touch"\n',
                    encoding="utf-8",
                )
                java = root / "java"
                args_file = root / "shaft-mcp.args"
                MODULE.configure_grok(java, args_file)
                MODULE.configure_grok(java, args_file)
            parsed = tomllib.loads(config.read_text(encoding="utf-8"))
            self.assertEqual("npx", parsed["mcp_servers"]["other"]["command"])
            self.assertEqual("secret-do-not-touch", parsed["mcp_servers"]["other"]["api_key"])
            self.assertEqual(str(java), parsed["mcp_servers"]["shaft-mcp"]["command"])

    def test_choose_client_error_uses_current_choice_count(self):
        stdout = io.StringIO()
        with scripted_stdin("0\n1"):
            with contextlib.redirect_stdout(stdout):
                chosen = MODULE.choose_client()
        self.assertEqual("codex", chosen)
        self.assertIn(f"1 to {len(MODULE.TARGET_CHOICES)}", stdout.getvalue())

    def test_has_agent_guidance_scaffold_requires_agents_md(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)

            self.assertFalse(MODULE.has_agent_guidance_scaffold(root))

            (root / "AGENTS.md").write_text("# Guidance\n", encoding="utf-8")
            self.assertTrue(MODULE.has_agent_guidance_scaffold(root))

    def test_agent_validation_script_files_includes_guidance_budget(self):
        # Regression test for issue #3363 bug 9: a fresh onboarding install used to
        # copy the validator scripts without the budget config they require,
        # making the onboarding-referenced `validate_agent_setup.py` crash with
        # FileNotFoundError on any project that installed them.
        self.assertIn("scripts/ci/agent_guidance_budget.json", MODULE.AGENT_VALIDATION_SCRIPT_FILES)

    def test_agent_validation_manifest_ships_every_module_the_validator_imports(self):
        # Same class of defect as issue #3363 bug 9, one level up: the manifest
        # copies validate_agent_setup.py into a user's project, so any sibling
        # module it imports at module scope must travel with it or the
        # installed validator dies on ImportError. Checked by reading the real
        # import statements rather than by listing them again here.
        shipped = set(MODULE.AGENT_VALIDATION_SCRIPT_FILES)
        self.assertEqual(self.agent_validation_manifest_missing_imports(shipped), [])

    def test_agent_validation_manifest_guard_detects_parenthesized_import_omission(self):
        shipped = set(MODULE.AGENT_VALIDATION_SCRIPT_FILES)
        shipped.remove("scripts/ci/worktree_hygiene.py")

        self.assertIn(
            "scripts/ci/validate_agent_setup.py imports scripts.ci.worktree_hygiene",
            self.agent_validation_manifest_missing_imports(shipped),
        )

    def test_downloaded_agent_validation_bundle_imports_from_isolated_project(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            target = Path(temp_dir).resolve()
            (target / "AGENTS.md").write_text("# Installed guidance\n", encoding="utf-8")

            def install_repository_file(_url, destination, _label, **_kwargs):
                relative = destination.relative_to(target)
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes((REPO_ROOT / relative).read_bytes())

            with mock.patch.object(MODULE, "download_file", side_effect=install_repository_file):
                MODULE.download_agent_validation_script_files(target)

            self.assertTrue((target / "scripts" / "agents" / "learning_loop.py").is_file())
            command = (
                "import sys; "
                f"sys.path.insert(0, {str(target)!r}); "
                "import scripts.agents.guard; "
                "import scripts.ci.validate_agent_setup"
            )
            completed = subprocess.run(  # nosec B603 - fixed interpreter and generated local import script.
                [sys.executable, "-I", "-S", "-c", command],
                cwd=target,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(0, completed.returncode, completed.stderr)

    @staticmethod
    def agent_validation_manifest_missing_imports(shipped):
        missing = set()
        repository_root = Path(__file__).resolve().parents[2]

        def module_file(module):
            relative = module.replace(".", "/") + ".py"
            return relative if (repository_root / relative).is_file() else None

        for relative in sorted(shipped):
            if not relative.endswith(".py"):
                continue
            tree = ast.parse((repository_root / relative).read_text(encoding="utf-8"), filename=relative)
            for node in ast.walk(tree):
                dependencies = []
                if isinstance(node, ast.Import):
                    dependencies.extend(
                        dependency
                        for alias in node.names
                        if alias.name.startswith("scripts.")
                        and (dependency := module_file(alias.name)) is not None
                    )
                elif (isinstance(node, ast.ImportFrom)
                      and node.level == 0
                      and node.module
                      and node.module.startswith("scripts.")):
                    direct = module_file(node.module)
                    if direct is not None:
                        dependencies.append(direct)
                    else:
                        dependencies.extend(
                            dependency
                            for alias in node.names
                            if alias.name != "*"
                            and (dependency := module_file(
                                node.module + "." + alias.name)) is not None
                        )
                for dependency in dependencies:
                    if dependency not in shipped:
                        missing.add(
                            f"{relative} imports {dependency[:-3].replace('/', '.')}")
        return sorted(missing)

    def test_render_client_menu_groups_ai_agents_and_advanced_sections(self):
        lines = MODULE.render_client_menu()

        self.assertIn("AI agents:", lines)
        self.assertIn("Advanced / IDE integration:", lines)
        ai_agents_index = lines.index("AI agents:")
        advanced_index = lines.index("Advanced / IDE integration:")
        self.assertLess(ai_agents_index, advanced_index)

        # The intellij-plugin entry's label must appear only after the
        # second (advanced) header, never mixed into the AI agents group.
        plugin_label = dict(MODULE.TARGET_CHOICES)["intellij-plugin"]
        label_line_indexes = [index for index, line in enumerate(lines) if plugin_label in line]
        self.assertEqual(1, len(label_line_indexes))
        self.assertGreater(label_line_indexes[0], advanced_index)

        # Numbered entries stay contiguous 1..N, in TARGET_CHOICES order,
        # regardless of which section they were printed under.
        numbered_lines = [line.strip() for line in lines if line.strip()[:1].isdigit()]
        expected_numbered_lines = [
            f"{index}. {label}" for index, (_, label) in enumerate(MODULE.TARGET_CHOICES, start=1)
        ]
        self.assertEqual(expected_numbered_lines, numbered_lines)

        # A one-line clarifier explains the plugin entry is unnecessary from
        # inside the plugin's own guided setup.
        self.assertTrue(
            any(
                "plugin's own MCP command" in line and "guided setup" in line
                for line in lines
            ),
            f"Expected a clarifier line in: {lines}",
        )

    def test_choose_client_numeric_and_name_input_resolve_to_same_target(self):
        plugin_index = str(len(MODULE.TARGET_CHOICES))
        results = []
        for answer in (plugin_index, "intellij-plugin"):
            with scripted_stdin(answer):
                with contextlib.redirect_stdout(io.StringIO()):
                    results.append(MODULE.choose_client())

        self.assertEqual(["intellij-plugin", "intellij-plugin"], results)

    def test_choose_client_prints_the_grouped_menu(self):
        stdout = io.StringIO()
        with scripted_stdin("6"):
            with contextlib.redirect_stdout(stdout):
                MODULE.choose_client()

        printed_lines = stdout.getvalue().splitlines()
        for line in MODULE.render_client_menu():
            self.assertIn(line, printed_lines)

    def test_client_selection_does_not_implicitly_select_skills(self):
        args = MODULE.parse_args(["--intellij-plugin"])

        self.assertFalse(MODULE.should_install_shaft_skills(args, Path("project").resolve()))

    def test_skip_shaft_skills_flag_disables_install(self):
        args = MODULE.parse_args(["--intellij-plugin", "--skip-shaft-skills"])

        self.assertFalse(MODULE.should_install_shaft_skills(args, Path("project").resolve()))

    def test_skills_only_install_avoids_runtime_version_and_mcp_configuration(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            project = Path(temp_dir)
            args = MODULE.parse_args(["--install-shaft-skills", "--json"])
            installed = project / MODULE.SHAFT_SKILLS_DIRECTORY
            calls = []
            originals = {
                "install_shaft_skills": MODULE.install_shaft_skills,
                "get_java25": MODULE.get_java25,
                "resolve_shaft_mcp_version": MODULE.resolve_shaft_mcp_version,
                "configure_client": MODULE.configure_client,
            }

            def install_skills(current_directory, _root, client):
                calls.append(("skills", current_directory, client))
                installed.mkdir()
                return [installed]

            def unexpected(name):
                def fail_if_called(*_args, **_kwargs):
                    self.fail(f"{name} must not run for a skills-only install")
                return fail_if_called

            MODULE.install_shaft_skills = install_skills
            MODULE.get_java25 = unexpected("Java setup")
            MODULE.resolve_shaft_mcp_version = unexpected("MCP version resolution")
            MODULE.configure_client = unexpected("MCP client configuration")
            stdout = io.StringIO()
            try:
                with temporary_current_directory(project), contextlib.redirect_stdout(stdout), \
                        contextlib.redirect_stderr(io.StringIO()):
                    MODULE.install(args)
            finally:
                for name, original in originals.items():
                    setattr(MODULE, name, original)

        self.assertEqual([("skills", project.resolve(), None)], calls)
        result = json.loads(stdout.getvalue())
        self.assertEqual({"shaftSkills": {
            "installed": True,
            "path": str(installed),
            "paths": [str(installed)],
        }}, result["components"])
        self.assertNotIn("client", result)
        self.assertNotIn("server", result)

    def test_cli_only_install_uses_runtime_without_mcp_client_configuration(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            args = MODULE.parse_args(["--install-shaft-cli"])
            java = root / "java"
            jar = root / "shaft-mcp.jar"
            cli_jar = root / "shaft-cli.jar"
            args_file = root / "shaft-mcp.args"
            launcher = root / "shaft-cli"
            calls = []
            originals = {
                name: getattr(MODULE, name)
                for name in (
                    "bootstrap_root", "get_java25", "java_home_for", "resolve_shaft_mcp_version",
                    "install_shaft_mcp_jar", "install_runtime_dependencies", "write_launcher_args",
                    "probe_stdio", "install_shaft_cli_jar", "write_shaft_cli_launcher",
                    "configure_client", "install_shaft_skills",
                )
            }

            MODULE.bootstrap_root = lambda: root / "bootstrap"
            MODULE.get_java25 = lambda _root: java
            MODULE.java_home_for = lambda _java: root
            MODULE.resolve_shaft_mcp_version = lambda *_args: "1.0.0"
            MODULE.install_shaft_mcp_jar = lambda *_args: jar
            MODULE.install_runtime_dependencies = lambda *_args: []
            MODULE.write_launcher_args = lambda *_args: args_file
            MODULE.probe_stdio = lambda *_args: calls.append("probe")
            MODULE.install_shaft_cli_jar = lambda *_args: cli_jar
            MODULE.write_shaft_cli_launcher = lambda *_args: launcher
            MODULE.configure_client = lambda *_args: self.fail("CLI-only must not configure an MCP client")
            MODULE.install_shaft_skills = lambda *_args: self.fail("CLI-only must not install skills")
            try:
                with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                    MODULE.install(args)
            finally:
                for name, original in originals.items():
                    setattr(MODULE, name, original)

        self.assertEqual(["probe"], calls)

    def test_wrappers_forward_arguments_without_implicit_skills_selection(self):
        shell = (MODULE_PATH.parent / "install-shaft-agentic-tools.sh").read_text(encoding="utf-8")
        powershell = (MODULE_PATH.parent / "install-shaft-agentic-tools.ps1").read_text(encoding="utf-8")

        self.assertIn('exec "$python_path" "$python_script" "$@"', shell)
        self.assertNotIn('set -- "$@" --install-shaft-skills', shell)
        self.assertIn("$installerArguments += $Arguments", powershell)
        self.assertNotIn('$installerArguments += @("--install-shaft-skills")', powershell)

    def test_install_shaft_skills_copies_package_to_native_codex_directory(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with temporary_current_directory(root):
                installed = MODULE.install_shaft_skills(Path.cwd(), root / "bootstrap", "codex")

            self.assertEqual([(root / ".agents" / "skills").resolve()], installed)
            self.assertTrue((installed[0] / "shaft-developer" / "SKILL.md").is_file())
            self.assertTrue((installed[0] / "shaft-mcp" / "agents" / "openai.yaml").is_file())

    def test_install_shaft_skills_without_client_covers_all_native_roots(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)

            installed = MODULE.install_shaft_skills(root, root / "bootstrap", None)

            self.assertEqual([
                (root / ".agents" / "skills").resolve(),
                (root / ".claude" / "skills").resolve(),
                (root / ".github" / "skills").resolve(),
            ], installed)
            for target in installed:
                self.assertTrue((target / "shaft-developer" / "SKILL.md").is_file())
                self.assertEqual(30, len(list(target.glob("*/SKILL.md"))))

    def test_install_shaft_skills_removes_only_owned_retired_skill_directories_idempotently(self):
        retired = (
            "act-as-shaft-dev",
            "analyzing-shaft-failures",
            "choosing-shaft-locators",
            "planning-shaft-tests",
            "recording-shaft-tests-with-mcp",
            "verifying-and-applying-shaft-changes",
            "writing-shaft-tests",
        )
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            target = root / ".agents" / "skills"
            for name in retired:
                owned = target / name
                (owned / "agents").mkdir(parents=True)
                (owned / "SKILL.md").write_text(
                    f"---\nname: {name}\n---\n# Retired SHAFT skill\n", encoding="utf-8")
                (owned / "agents" / "openai.yaml").write_text("interface: {}\n", encoding="utf-8")
            protected = root / ".claude" / "skills" / retired[-1]
            protected.mkdir(parents=True)
            (protected / "SKILL.md").write_text(
                "---\nname: writing-shaft-tests\n---\n# User-owned skill\n", encoding="utf-8")
            custom = target / "custom-skill" / "notes.md"
            custom.parent.mkdir(parents=True)
            custom.write_text("keep me", encoding="utf-8")
            top_level_user_file = target / "user-notes.md"
            top_level_user_file.write_text("keep me too", encoding="utf-8")

            first = MODULE.install_shaft_skills(root, root / "bootstrap", None)
            second = MODULE.install_shaft_skills(root, root / "bootstrap", None)

            self.assertEqual([
                target.resolve(),
                (root / ".claude" / "skills").resolve(),
                (root / ".github" / "skills").resolve(),
            ], first)
            self.assertEqual(first, second)
            self.assertEqual(frozenset(retired), MODULE.RETIRED_SHAFT_SKILL_DIRECTORIES)
            for name in retired:
                self.assertFalse((target / name).exists())
            self.assertTrue(protected.is_dir())
            self.assertEqual("keep me", custom.read_text(encoding="utf-8"))
            self.assertEqual("keep me too", top_level_user_file.read_text(encoding="utf-8"))
            self.assertTrue((target / "shaft-developer" / "SKILL.md").is_file())

    def test_install_shaft_skills_rejects_linked_native_parent_before_legacy_cleanup(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            external = root / "external"
            legacy = external / "skills" / "writing-shaft-tests"
            (legacy / "agents").mkdir(parents=True)
            (legacy / "SKILL.md").write_text(
                "---\nname: writing-shaft-tests\n---\n# Retired SHAFT skill\n", encoding="utf-8")
            (legacy / "agents" / "openai.yaml").write_text("interface: {}\n", encoding="utf-8")
            os.symlink(external, root / ".agents", target_is_directory=True)

            with self.assertRaisesRegex(MODULE.InstallError, "linked native skill path"):
                MODULE.install_shaft_skills(root, root / "bootstrap", "codex")

            self.assertTrue(legacy.is_dir())
            self.assertFalse((external / "skills" / "shaft-developer").exists())

    @unittest.skipUnless(os.name == "nt", "Windows junctions only")
    def test_install_shaft_skills_rejects_junctioned_native_parent_without_path_is_junction(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            external = root / "external"
            legacy = external / "skills" / "writing-shaft-tests"
            (legacy / "agents").mkdir(parents=True)
            (legacy / "SKILL.md").write_text(
                "---\nname: writing-shaft-tests\n---\n# Retired SHAFT skill\n", encoding="utf-8")
            (legacy / "agents" / "openai.yaml").write_text("interface: {}\n", encoding="utf-8")
            command_processor = (
                Path(os.environ["SystemRoot"]) / "System32" / "cmd.exe"
            ).resolve(strict=True)
            result = subprocess.run(  # nosec B603 -- resolved OS binary, fixed switches, trusted temporary paths.
                [str(command_processor), "/d", "/c", "mklink", "/J", str(root / ".agents"), str(external)],
                capture_output=True, text=True, check=False)
            if result.returncode != 0:
                self.skipTest(f"filesystem does not support junctions: {result.stderr.strip()}")
            path_is_junction = getattr(Path, "is_junction", None)
            if path_is_junction is not None:
                delattr(Path, "is_junction")
            try:
                with self.assertRaisesRegex(MODULE.InstallError, "linked native skill path"):
                    MODULE.install_shaft_skills(root, root / "bootstrap", "codex")
            finally:
                if path_is_junction is not None:
                    setattr(Path, "is_junction", path_is_junction)

            self.assertTrue(legacy.is_dir())
            self.assertFalse((external / "skills" / "shaft-developer").exists())

    def test_install_shaft_skills_downloads_raw_files_without_repo_archive(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            calls = []
            original_local_source = MODULE.local_shaft_skills_source
            original_download_file = MODULE.download_file
            original_discovery = MODULE.remote_shaft_skill_files

            def no_local_source():
                return None

            remote_files = (
                "references/shaft-cli-commands.md",
                "references/shaft-mcp-tools.md",
                "shaft-developer/SKILL.md",
                "shaft-developer/agents/openai.yaml",
                "shaft-mcp/SKILL.md",
                "shaft-mcp/agents/openai.yaml",
            )

            def fake_download(url, target, label, **_kwargs):
                calls.append(url)
                self.assertNotIn("/archive/", url)
                self.assertIn("/shaft-skills/", url)
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(f"{label}\n", encoding="utf-8")

            MODULE.local_shaft_skills_source = no_local_source
            MODULE.download_file = fake_download
            MODULE.remote_shaft_skill_files = lambda: remote_files
            try:
                installed = MODULE.install_shaft_skills(
                    root / "project", root / "bootstrap", "claude")
            finally:
                MODULE.local_shaft_skills_source = original_local_source
                MODULE.download_file = original_download_file
                MODULE.remote_shaft_skill_files = original_discovery

            self.assertEqual(len(remote_files), len(calls))
            self.assertEqual([(root / "project" / ".claude" / "skills").resolve()], installed)
            self.assertTrue((installed[0] / "shaft-developer" / "SKILL.md").is_file())
            self.assertTrue((installed[0] / "shaft-mcp" / "agents" / "openai.yaml").is_file())
            self.assertTrue((installed[0] / "references" / "shaft-mcp-tools.md").is_file())
            self.assertTrue((installed[0] / "references" / "shaft-cli-commands.md").is_file())

    def test_remote_manifest_discovery_filters_to_complete_skill_pack(self):
        tree = {"truncated": False, "tree": [
            {"path": "README.md", "type": "blob"},
            {"path": "shaft-skills/evaluation-prompts.md", "type": "blob"},
            {"path": "shaft-skills/references/shaft-mcp-tools.md", "type": "blob"},
            {"path": "shaft-skills/shaft-developer/SKILL.md", "type": "blob"},
            {"path": "shaft-skills/shaft-developer/references/routing.md", "type": "blob"},
            {"path": "shaft-skills/shaft-mcp/SKILL.md", "type": "blob"},
            {"path": "shaft-skills/shaft-mcp/agents/openai.yaml", "type": "blob"},
            {"path": "shaft-skills/unrelated/note.md", "type": "blob"},
        ]}

        with mock.patch.object(MODULE, "download_bytes", return_value=json.dumps(tree).encode()):
            discovered = MODULE.remote_shaft_skill_files()

        self.assertEqual((
            "evaluation-prompts.md",
            "references/shaft-mcp-tools.md",
            "shaft-developer/SKILL.md",
            "shaft-developer/references/routing.md",
            "shaft-mcp/SKILL.md",
            "shaft-mcp/agents/openai.yaml",
        ), discovered)

    def test_shaft_skills_dynamic_manifest_is_sorted_and_complete(self):
        source = MODULE.local_shaft_skills_source()
        self.assertIsNotNone(source, "repo checkout should be detected as a shaft-skills source")
        manifest = MODULE.shaft_skill_files(source)

        self.assertEqual(tuple(sorted(manifest)), manifest)
        self.assertEqual(30, sum(relative.endswith("/SKILL.md") for relative in manifest))
        for relative in manifest:
            self.assertTrue((source / relative).is_file(), f"manifest entry missing on disk: {relative}")

    def test_every_repo_skill_directory_and_support_file_is_in_dynamic_manifest(self):
        source = MODULE.local_shaft_skills_source()
        self.assertIsNotNone(source, "repo checkout should be detected as a shaft-skills source")
        manifest = set(MODULE.shaft_skill_files(source))
        for skill_md in sorted(source.glob("*/SKILL.md")):
            for file in skill_md.parent.rglob("*"):
                if file.is_file():
                    relative = file.relative_to(source).as_posix()
                    self.assertIn(relative, manifest,
                                  f"skill support file missing from download manifest: {relative}")

    def test_marketplace_lists_every_canonical_skill_directory(self):
        """Directories first, `SKILL.md` required second, diff third (#4511).

        Deriving the on-disk set FROM `*/SKILL.md` meant a `shaft-skills/`
        directory that never got one -- half-authored, or one whose `SKILL.md`
        was deleted or renamed -- never entered the set, so it could not be
        reported as unlisted by the diff built from it. Enumerating the
        directories and requiring the file the other way round catches it
        before the diff runs, which is the inversion `test_validate_skills.py`
        took in #4501. That module's copy of this assertion runs on the
        `agent-harness` leg; this one runs on `installer-verify`, gated by the
        `intellij` path filter, so both need it.

        Directories are sorted as `Path` objects, not as names, because the
        comparison list they feed is ordered and sorting `<dir>` reproduces the
        order sorting `<dir>/SKILL.md` produced exactly -- the suffix is
        constant. The `Path`-vs-string ordering difference between the two
        modules is latent while every skill name is lowercase and stays tracked
        separately rather than converged here.
        """
        source = MODULE.local_shaft_skills_source()
        marketplace = json.loads((REPO_ROOT / ".claude-plugin" / "marketplace.json")
                                 .read_text(encoding="utf-8"))
        listed = marketplace["plugins"][0]["skills"]
        directories = sorted(
            entry for entry in source.iterdir()
            if entry.is_dir() and entry.name != "references"
        )
        self.assertEqual(
            [],
            [entry.name for entry in directories if not (entry / "SKILL.md").is_file()],
            "every shaft-skills/ directory must hold a SKILL.md to be installable",
        )
        expected = [f"./{entry.name}" for entry in directories]

        self.assertEqual(expected, listed)
        self.assertIn("./shaft-developer", listed)

    def test_readme_routes_agent_setup_without_website_subpaths(self):
        readme = (REPO_ROOT / "README.md").read_text(encoding="utf-8")

        self.assertIn("`$shaft-developer`", readme)
        self.assertIn("https://shafthq.github.io/", readme)
        self.assertIn("shaft-skills/references/shaft-mcp-tools.md", readme)
        self.assertIn("shaft-skills/references/shaft-cli-commands.md", readme)
        self.assertNotIn("act-as-shaft-dev", readme)
        self.assertNotIn("writing-shaft-tests", readme)

    def test_codex_auto_approval_is_added_to_shaft_mcp_section(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            config = Path(temp_dir) / "config.toml"
            config.write_text(
                '[mcp_servers.shaft-mcp]\n'
                'command = "java"\n'
                'args = ["@shaft-mcp.args"]\n'
                '\n'
                '[mcp_servers.other]\n'
                'default_tools_approval_mode = "prompt"\n',
                encoding="utf-8",
            )

            MODULE.ensure_codex_auto_approval(config)

            text = config.read_text(encoding="utf-8")
            shaft_section = text.split("[mcp_servers.other]", 1)[0]
            self.assertIn('default_tools_approval_mode = "auto"', shaft_section)
            self.assertIn('default_tools_approval_mode = "prompt"', text)

    def test_codex_auto_approval_supports_quoted_section_and_fails_when_missing(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            config = Path(temp_dir) / "config.toml"
            config.write_text(
                '[mcp_servers."shaft-mcp"]\n'
                'command = "java"\n'
                'args = ["@shaft-mcp.args"]\n',
                encoding="utf-8",
            )

            MODULE.ensure_codex_auto_approval(config)

            self.assertIn('default_tools_approval_mode = "auto"', config.read_text(encoding="utf-8"))

        with tempfile.TemporaryDirectory() as temp_dir:
            config = Path(temp_dir) / "config.toml"
            config.write_text('[mcp_servers.other]\ncommand = "java"\n', encoding="utf-8")

            with self.assertRaises(MODULE.InstallError):
                MODULE.ensure_codex_auto_approval(config)

    def test_write_launcher_args(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            jar = root / "shaft-mcp.jar"
            dep = root / "lib" / "dependency.jar"
            dep.parent.mkdir()
            jar.write_bytes(b"")
            dep.write_bytes(b"")

            with temporary_environment(
                    HOME=str(root / "home"),
                    USERPROFILE=str(root / "home"),
                    LOCALAPPDATA=str(root / "local-app-data"),
                    XDG_DATA_HOME=str(root / "xdg-data")):
                args = MODULE.write_launcher_args(jar, [dep])
                runtime_root = MODULE.application_data_root() / "work"

            content = args.read_text(encoding="utf-8")
            self.assertIn(
                MODULE.java_argfile_quote(
                    f"-D{MODULE.FALLBACK_WORKSPACE_SYSTEM_PROPERTY}={runtime_root}"),
                content,
            )
            # Pinning either entry would lock every client into the shared work directory and
            # reject tool calls from the user's real project as outside the MCP workspace.
            self.assertNotIn("-Duser.dir=", content)
            self.assertNotIn("-Dshaft.mcp.workspaceRoot=", content)
            self.assertTrue(runtime_root.is_dir())
            self.assertIn("-cp\n", content)
            self.assertIn("shaft-mcp.jar" + os.pathsep, content)
            self.assertIn("dependency.jar", content)
            self.assertTrue(content.endswith(MODULE.MAIN_CLASS + "\n"))

    def test_install_runtime_dependencies_from_file_repository(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repository = root / "repo"
            dependency = repository / "org" / "example" / "runtime" / "1.0.0" / "runtime-1.0.0.jar"
            dependency.parent.mkdir(parents=True)
            dependency.write_bytes(b"dependency")
            dependency.with_name(dependency.name + ".sha256").write_text(
                hashlib.sha256(b"dependency").hexdigest() + "\n",
                encoding="utf-8",
            )

            jar = root / "install" / "shaft-mcp.jar"
            jar.parent.mkdir()
            with zipfile.ZipFile(jar, "w") as archive:
                archive.writestr(
                    MODULE.RUNTIME_DEPENDENCIES_ENTRY,
                    "The following files have been resolved:\n"
                    "   org.example:runtime:jar:1.0.0:runtime\n",
                )

            maven_repository = root / "m2-repository"
            with temporary_environment(SHAFT_MCP_MAVEN_LOCAL_REPOSITORY=str(maven_repository)), \
                    contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                installed = MODULE.install_runtime_dependencies(jar, repository.as_uri())
                # A second run must skip the download: the artifact already sits in the local
                # Maven repository with a matching checksum.
                reinstalled = MODULE.install_runtime_dependencies(jar, repository.as_uri())

            self.assertEqual(1, len(installed))
            self.assertEqual(b"dependency", installed[0].read_bytes())
            # Dependencies land in the standard local Maven repository layout so future SHAFT
            # Maven builds reuse them and reinstalls skip them.
            self.assertEqual(
                (maven_repository / "org" / "example" / "runtime" / "1.0.0"
                 / "runtime-1.0.0.jar").resolve(),
                installed[0],
            )
            self.assertEqual(installed, reinstalled)

    def test_install_repository_file_keeps_existing_target_when_replace_is_denied(self):
        # Issue #3426 A6: on Windows, a locked local jar (running JVM, antivirus) made the whole
        # install abort with WinError 5 mid-resolution. A locked-but-present target must be kept
        # with a warning instead of failing the installer.
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repository = root / "repo"
            artifact = repository / "org" / "example" / "runtime" / "1.0.0" / "runtime-1.0.0.jar"
            artifact.parent.mkdir(parents=True)
            artifact.write_bytes(b"fresh-bytes")
            artifact.with_name(artifact.name + ".sha256").write_text(
                hashlib.sha256(b"fresh-bytes").hexdigest() + "\n", encoding="utf-8")
            target = root / "m2" / "runtime-1.0.0.jar"
            target.parent.mkdir(parents=True)
            target.write_bytes(b"locally-built-different-bytes")

            original_replace = os.replace

            def denied_replace(source, destination):
                if Path(destination) == target:
                    raise PermissionError(5, "Access is denied", str(destination))
                return original_replace(source, destination)

            original_sleep = MODULE.time.sleep
            os.replace = denied_replace
            MODULE.time.sleep = lambda seconds: None
            try:
                with contextlib.redirect_stdout(io.StringIO()), \
                        contextlib.redirect_stderr(io.StringIO()) as stderr:
                    resolved, downloaded = MODULE.install_repository_file(
                        artifact.as_uri(), target, "org.example:runtime:1.0.0", announce=False)
            finally:
                os.replace = original_replace
                MODULE.time.sleep = original_sleep

            self.assertEqual(target.resolve(), resolved)
            self.assertTrue(downloaded)
            # The pre-existing local file survives untouched and the tmp file is cleaned up.
            self.assertEqual(b"locally-built-different-bytes", target.read_bytes())
            self.assertEqual([target.name], [path.name for path in target.parent.iterdir()])
            self.assertIn("kept the existing local", stderr.getvalue())

    def test_replace_with_retry_recovers_after_transient_denial(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            source = root / "source.tmp"
            source.write_bytes(b"new")
            target = root / "target.jar"
            target.write_bytes(b"old")
            original_replace = os.replace
            failures = {"remaining": 2}

            def flaky_replace(src, dst):
                if failures["remaining"] > 0:
                    failures["remaining"] -= 1
                    raise PermissionError(5, "Access is denied", str(dst))
                return original_replace(src, dst)

            original_sleep = MODULE.time.sleep
            os.replace = flaky_replace
            MODULE.time.sleep = lambda seconds: None
            try:
                self.assertTrue(MODULE.replace_with_retry(source, target))
            finally:
                os.replace = original_replace
                MODULE.time.sleep = original_sleep
            self.assertEqual(b"new", target.read_bytes())

    def test_configured_local_repository_reads_settings_xml(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            settings = Path(temp_dir) / "settings.xml"
            settings.write_text(
                '<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0">'
                "<localRepository>${user.home}/custom-repo</localRepository></settings>",
                encoding="utf-8",
            )
            self.assertEqual(
                (Path.home() / "custom-repo").resolve(),
                MODULE.configured_local_repository(settings),
            )

            settings.write_text(
                "<settings><localRepository>${env.OTHER}/x</localRepository></settings>",
                encoding="utf-8",
            )
            self.assertIsNone(MODULE.configured_local_repository(settings))

            settings.write_text("<settings><mirrors/></settings>", encoding="utf-8")
            self.assertIsNone(MODULE.configured_local_repository(settings))

            self.assertIsNone(
                MODULE.configured_local_repository(Path(temp_dir) / "missing-settings.xml")
            )

    def test_parse_version_normalizes_empty_string_to_latest(self):
        # Test explicit empty string argument
        args = MODULE.parse_args(["--codex", "--version", ""])
        self.assertEqual("LATEST", args.version)

    def test_parse_version_normalizes_whitespace_to_latest(self):
        # Test whitespace-only version argument
        args = MODULE.parse_args(["--codex", "--version", "   "])
        self.assertEqual("LATEST", args.version)

    def test_parse_version_unset_defaults_to_latest(self):
        # Test unset version with no environment variable
        with temporary_environment(SHAFT_MCP_VERSION=""):
            args = MODULE.parse_args(["--codex"])
        self.assertEqual("LATEST", args.version)

    def test_parse_version_env_variable_unset_defaults_to_latest(self):
        # Test completely unset environment variable
        original = os.environ.pop("SHAFT_MCP_VERSION", None)
        try:
            args = MODULE.parse_args(["--codex"])
            self.assertEqual("LATEST", args.version)
        finally:
            if original is not None:
                os.environ["SHAFT_MCP_VERSION"] = original

    def test_parse_version_bare_flag_defaults_to_latest(self):
        # Test bare --version flag without argument (using nargs='?')
        args = MODULE.parse_args(["--codex", "--version"])
        self.assertEqual("LATEST", args.version)

    def test_parse_version_preserves_explicit_version(self):
        # Test that explicit version strings are preserved
        args = MODULE.parse_args(["--codex", "--version", "1.2.3"])
        self.assertEqual("1.2.3", args.version)

    def test_parse_version_from_environment_variable(self):
        # Test that environment variable is read and trimmed
        with temporary_environment(SHAFT_MCP_VERSION="0.5.0"):
            args = MODULE.parse_args(["--codex"])
        self.assertEqual("0.5.0", args.version)

    def test_parse_version_env_variable_with_whitespace(self):
        # Test that environment variable with surrounding whitespace is trimmed
        with temporary_environment(SHAFT_MCP_VERSION="  0.5.0  "):
            args = MODULE.parse_args(["--codex"])
        self.assertEqual("0.5.0", args.version)

    def test_parse_install_shaft_cli_flag_independent_of_client(self):
        args = MODULE.parse_args(["--codex", "--install-shaft-cli"])

        self.assertEqual("codex", args.client)
        self.assertTrue(args.install_shaft_cli)

    def test_install_shaft_cli_jar_from_file_repository(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            repository = root / "repo"
            version = "1.0.0"
            artifact = (repository / "io" / "github" / "shafthq" / "shaft-cli" / version
                        / f"shaft-cli-{version}.jar")
            artifact.parent.mkdir(parents=True)
            artifact.write_bytes(b"shaft-cli-jar-bytes")
            artifact.with_name(artifact.name + ".sha256").write_text(
                hashlib.sha256(b"shaft-cli-jar-bytes").hexdigest() + "\n",
                encoding="utf-8",
            )

            with temporary_environment(
                    HOME=str(root / "home"),
                    USERPROFILE=str(root / "home"),
                    LOCALAPPDATA=str(root / "local-app-data"),
                    XDG_DATA_HOME=str(root / "xdg-data")), \
                    contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                jar = MODULE.install_shaft_cli_jar(version, repository.as_uri(), root)
                # A second run must skip the download: a verified copy already exists locally.
                reinstalled = MODULE.install_shaft_cli_jar(version, repository.as_uri(), root)
                expected = (MODULE.shaft_cli_application_data_root() / "versions" / version
                            / "shaft-cli.jar").resolve()

            self.assertEqual(b"shaft-cli-jar-bytes", jar.read_bytes())
            self.assertEqual(expected, jar)
            self.assertEqual(jar, reinstalled)

    def test_write_shaft_cli_launcher(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            java = root / "java"
            jar = root / "versions" / "1.0.0" / "shaft-cli.jar"
            jar.parent.mkdir(parents=True)
            jar.write_bytes(b"")

            launcher = MODULE.write_shaft_cli_launcher(java, jar)

            args_file = jar.parent / "shaft-cli.args"
            self.assertTrue(args_file.is_file())
            args_content = args_file.read_text(encoding="utf-8")
            self.assertIn("-jar", args_content)
            self.assertIn(MODULE.java_argfile_quote(str(jar.resolve())), args_content)

            self.assertTrue(launcher.is_file())
            launcher_content = launcher.read_text(encoding="utf-8")
            self.assertIn(str(java.resolve()), launcher_content)
            self.assertIn(str(args_file.resolve()), launcher_content)
            if MODULE.system_name() == "Windows":
                self.assertEqual("shaft-cli.cmd", launcher.name)
            else:
                self.assertEqual("shaft-cli", launcher.name)
                self.assertTrue(os.access(launcher, os.X_OK))


class _UrlResponse:
    def __init__(self, payload: bytes) -> None:
        self._payload = payload
        self.headers = {"Content-Length": str(len(payload))}

    def read(self, size: int = -1) -> bytes:
        if size is None or size < 0:
            data = self._payload
            self._payload = b""
            return data
        data = self._payload[:size]
        self._payload = self._payload[size:]
        return data

    def __enter__(self) -> "_UrlResponse":
        return self

    def __exit__(self, *_args) -> None:
        return None


def _record_layer_failure() -> urllib.error.URLError:
    return urllib.error.URLError(ssl.SSLError(1, "[SSL: RECORD_LAYER_FAILURE] record layer failure"))


class TransientDownloadRetryTest(unittest.TestCase):
    def test_url_text_or_none_retries_record_layer_failure_then_succeeds(self):
        attempts = {"n": 0}

        def flaky(_request, timeout=120):
            attempts["n"] += 1
            if attempts["n"] == 1:
                raise _record_layer_failure()
            return _UrlResponse(b"checksum")

        with mock.patch("urllib.request.urlopen", side_effect=flaky), mock.patch.object(
            MODULE.time, "sleep", return_value=None
        ):
            self.assertEqual("checksum", MODULE.url_text_or_none("https://example.invalid/artifact.sha1"))
        self.assertGreaterEqual(attempts["n"], 2)

    def test_url_text_or_none_exhausted_retries_fail_closed(self):
        attempts = {"n": 0}

        def always_fail(_request, timeout=120):
            attempts["n"] += 1
            raise _record_layer_failure()

        with mock.patch("urllib.request.urlopen", side_effect=always_fail), mock.patch.object(
            MODULE.time, "sleep", return_value=None
        ):
            with self.assertRaises(urllib.error.URLError):
                MODULE.url_text_or_none("https://example.invalid/artifact.sha1")
        self.assertGreaterEqual(attempts["n"], 5)

    def test_url_text_or_none_returns_none_on_404_without_retrying(self):
        attempts = {"n": 0}

        def missing(_request, timeout=120):
            attempts["n"] += 1
            raise urllib.error.HTTPError(
                "https://example.invalid/missing.sha1",
                404,
                "Not Found",
                hdrs=None,
                fp=None,
            )

        with mock.patch("urllib.request.urlopen", side_effect=missing), mock.patch.object(
            MODULE.time, "sleep", return_value=None
        ):
            self.assertIsNone(MODULE.url_text_or_none("https://example.invalid/missing.sha1"))
        self.assertEqual(attempts["n"], 1)


class HttpsUrlGuardTest(unittest.TestCase):
    def test_download_bytes_rejects_http_and_custom_schemes(self):
        with mock.patch("urllib.request.urlopen") as urlopen:
            with self.assertRaises(MODULE.InstallError) as http_error:
                MODULE.download_bytes("http://example.invalid/artifact")
            with self.assertRaises(MODULE.InstallError) as ftp_error:
                MODULE.download_bytes("ftp://example.invalid/artifact")
        urlopen.assert_not_called()
        self.assertIn("http", str(http_error.exception))
        self.assertIn("ftp", str(ftp_error.exception))


class AgenticToolsInstallerSurfaceTest(unittest.TestCase):
    def test_banner_names_agentic_tools_and_stays_available(self):
        stderr = io.StringIO()
        with temporary_environment(), contextlib.redirect_stderr(stderr):
            os.environ.pop(MODULE.BOOTSTRAP_BANNER_SHOWN, None)
            MODULE.banner()
        text = stderr.getvalue()
        self.assertIn("Agentic Tools", text)
        self.assertNotIn("MCP installer", text)

    def test_overall_progress_logs_phase_percent_when_stderr_is_not_a_tty(self):
        stderr = io.StringIO()
        stderr.isatty = lambda: False  # type: ignore[method-assign]
        with contextlib.redirect_stderr(stderr):
            MODULE.overall_progress("MCP", 1, 4)
        self.assertRegex(stderr.getvalue(), r"MCP.*25%")

    def test_tty_keeps_overall_progress_on_a_separate_line_from_current_item(self):
        stderr = io.StringIO()
        stderr.isatty = lambda: True  # type: ignore[method-assign]
        with contextlib.redirect_stderr(stderr):
            MODULE.overall_progress("MCP", 1, 4)
            MODULE.progress("jar", 10, 100)
        text = stderr.getvalue()
        self.assertIn("Overall", text)
        self.assertIn("jar", text)
        overall_at = text.find("Overall")
        item_at = text.find("jar")
        self.assertGreater(item_at, overall_at)
        self.assertIn(
            "\n",
            text[overall_at:item_at],
            "overall and current-item bars must not share one \\r overwrite",
        )

    def test_primary_scripts_use_agentic_tools_name_and_old_names_are_shims(self):
        scripts = REPO_ROOT / "scripts" / "mcp"
        self.assertTrue((scripts / "install_shaft_agentic_tools.py").is_file())
        self.assertTrue((scripts / "install-shaft-agentic-tools.ps1").is_file())
        self.assertTrue((scripts / "install-shaft-agentic-tools.sh").is_file())
        ps1 = (scripts / "install-shaft-mcp.ps1").read_text(encoding="utf-8")
        sh = (scripts / "install-shaft-mcp.sh").read_text(encoding="utf-8")
        self.assertRegex(ps1, r"(?i)deprecat")
        self.assertRegex(sh, r"(?i)deprecat")
        self.assertIn("install-shaft-agentic-tools.ps1", ps1)
        self.assertIn("install-shaft-agentic-tools.sh", sh)
        one_liner_ps1 = (scripts / "install.ps1").read_text(encoding="utf-8")
        one_liner_sh = (scripts / "install.sh").read_text(encoding="utf-8")
        self.assertIn("install-shaft-agentic-tools.ps1", one_liner_ps1)
        self.assertIn("Agentic Tools", one_liner_ps1)
        self.assertIn("install-shaft-agentic-tools.sh", one_liner_sh)
        self.assertIn("Agentic Tools", one_liner_sh)
        self.assertNotIn("install-shaft-mcp.ps1", one_liner_ps1)
        self.assertNotIn("install-shaft-mcp.sh", one_liner_sh)


if __name__ == "__main__":
    unittest.main()

