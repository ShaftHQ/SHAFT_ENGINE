import importlib.util
import contextlib
import hashlib
import io
import json
import os
import subprocess  # nosec B404 -- Windows junction test uses resolved System32 cmd.exe only.
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest import mock

MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "mcp" / "install_shaft_mcp.py"
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

        # Numbered entries stay contiguous 1..6, in TARGET_CHOICES order,
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
        results = []
        for answer in ("6", "intellij-plugin"):
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
        shell = (MODULE_PATH.parent / "install-shaft-mcp.sh").read_text(encoding="utf-8")
        powershell = (MODULE_PATH.parent / "install-shaft-mcp.ps1").read_text(encoding="utf-8")

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
        source = MODULE.local_shaft_skills_source()
        marketplace = json.loads((REPO_ROOT / ".claude-plugin" / "marketplace.json")
                                 .read_text(encoding="utf-8"))
        listed = marketplace["plugins"][0]["skills"]
        expected = [f"./{path.parent.name}" for path in sorted(source.glob("*/SKILL.md"))]

        self.assertEqual(expected, listed)
        self.assertIn("./shaft-developer", listed)

    def test_readme_routes_setup_through_router_and_current_tools(self):
        readme = (REPO_ROOT / "README.md").read_text(encoding="utf-8")

        self.assertIn("--skill '*'", readme)
        self.assertIn("--install-shaft-skills --json", readme)
        self.assertIn("`$shaft-developer`", readme)
        self.assertIn("shaft_project_init_agents", readme)
        self.assertIn('shaft-cli guide search query="browser assertions" maxResults=3 --json', readme)
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


if __name__ == "__main__":
    unittest.main()
