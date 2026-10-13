"""The adapter publish helper only downloads the pinned upstream hosts."""

import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

SCRIPT = Path(__file__).resolve().parents[2] / "allure-cucumber8-jvm" / "prepare_upstream_adapter.py"


def load_script():
    spec = importlib.util.spec_from_file_location("prepare_upstream_adapter", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class PrepareUpstreamAdapterTest(unittest.TestCase):
    def test_download_refuses_unapproved_urls(self):
        module = load_script()
        with self.assertRaises(RuntimeError):
            module.download("file:///etc/passwd")
        with self.assertRaises(RuntimeError):
            module.download("https://example.com/allure-cucumber8-jvm.jar")
        with self.assertRaises(RuntimeError):
            module.download("http://github.com/MohabMohie/allure-java/archive/pin.tar.gz")

    def test_windows_batch_toolchain_runs_through_cmd(self):
        module = load_script()
        with patch.object(module.os, "name", "nt"), patch.object(
            module.shutil, "which", side_effect=lambda name: r"C:\Maven\bin\mvn.cmd" if name == "mvn.cmd" else None
        ), patch.dict(module.os.environ, {"JAVA_HOME": "", "MVN": ""}, clear=False), patch.object(
            module.subprocess, "run"
        ) as run:
            resolved = module.toolchain("mvn")
            module.run_tool([resolved, "-version"])
        self.assertEqual(r"C:\Maven\bin\mvn.cmd", resolved)
        run.assert_called_once_with(
            ["cmd.exe", "/d", "/c", r"C:\Maven\bin\mvn.cmd", "-version"],
            check=True,
        )

    def test_java_home_javadoc_exe_is_preferred(self):
        module = load_script()
        with tempfile.TemporaryDirectory() as temporary:
            java_home = Path(temporary)
            executable = java_home / "bin" / "javadoc.exe"
            executable.parent.mkdir()
            executable.write_text("", encoding="utf-8")
            with patch.dict(module.os.environ, {"JAVA_HOME": str(java_home), "MVN": ""}, clear=False):
                self.assertEqual(str(executable), module.toolchain("javadoc"))


if __name__ == "__main__":
    unittest.main()
