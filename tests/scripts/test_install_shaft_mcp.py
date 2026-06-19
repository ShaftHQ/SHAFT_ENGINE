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
