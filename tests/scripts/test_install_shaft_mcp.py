import importlib.util
import contextlib
import hashlib
import io
import os
import tempfile
import unittest
import zipfile
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[2] / "scripts" / "mcp" / "install_shaft_mcp.py"
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

    def test_intellij_plugin_target_does_not_configure_external_client(self):
        MODULE.configure_client("intellij-plugin", Path("java"), Path("shaft-mcp.args"))

    def test_unattended_install_defaults_to_shaft_skills(self):
        args = MODULE.parse_args(["--intellij-plugin"])

        stderr = io.StringIO()
        original_stdin = MODULE.sys.stdin
        MODULE.sys.stdin = io.StringIO("")
        try:
            with contextlib.redirect_stderr(stderr):
                should_install = MODULE.should_install_shaft_skills(args, Path("project").resolve())
        finally:
            MODULE.sys.stdin = original_stdin

        self.assertTrue(should_install)
        self.assertIn("Installing SHAFT skills into current directory by default", stderr.getvalue())

    def test_skip_shaft_skills_flag_disables_install(self):
        args = MODULE.parse_args(["--intellij-plugin", "--skip-shaft-skills"])

        self.assertFalse(MODULE.should_install_shaft_skills(args, Path("project").resolve()))

    def test_install_shaft_skills_copies_package_to_current_directory(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            with temporary_current_directory(root):
                installed = MODULE.install_shaft_skills(Path.cwd(), root / "bootstrap")

            self.assertEqual(root / MODULE.SHAFT_SKILLS_DIRECTORY, installed)
            self.assertTrue((installed / "writing-shaft-tests" / "SKILL.md").is_file())
            self.assertTrue((installed / "recording-shaft-tests-with-mcp" / "agents" / "openai.yaml").is_file())

    def test_extract_shaft_skills_from_archive(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            archive = root / "shaft-skills.zip"
            with zipfile.ZipFile(archive, "w") as zip_file:
                for marker in MODULE.SHAFT_SKILLS_SOURCE_MARKERS:
                    zip_file.writestr(f"SHAFT_ENGINE-main/shaft-skills/{marker}", "skill")
                zip_file.writestr(
                    "SHAFT_ENGINE-main/shaft-skills/recording-shaft-tests-with-mcp/agents/openai.yaml",
                    "openai: true",
                )

            installed = MODULE.extract_shaft_skills_from_archive(archive, root / "project" / "shaft-skills")

            self.assertTrue((installed / "writing-shaft-tests" / "SKILL.md").is_file())
            self.assertTrue((installed / "recording-shaft-tests-with-mcp" / "agents" / "openai.yaml").is_file())

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
            self.assertIn(MODULE.java_argfile_quote(f"-Duser.dir={runtime_root}"), content)
            self.assertIn(
                MODULE.java_argfile_quote(f"-D{MODULE.WORKSPACE_SYSTEM_PROPERTY}={runtime_root}"),
                content,
            )
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

            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
                installed = MODULE.install_runtime_dependencies(jar, repository.as_uri())

            self.assertEqual(1, len(installed))
            self.assertEqual(b"dependency", installed[0].read_bytes())
            self.assertTrue(
                installed[0].as_posix().endswith(
                    "/lib/org/example/runtime/1.0.0/runtime-1.0.0.jar"
                )
            )


if __name__ == "__main__":
    unittest.main()
