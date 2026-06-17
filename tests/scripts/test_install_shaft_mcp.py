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
assert SPEC.loader
SPEC.loader.exec_module(MODULE)


class InstallShaftMcpTest(unittest.TestCase):
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

            args = MODULE.write_launcher_args(jar, [dep])

            content = args.read_text(encoding="utf-8")
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
