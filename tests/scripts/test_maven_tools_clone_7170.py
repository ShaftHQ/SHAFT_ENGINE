"""#7170: the Maven Tools clone must be diagnosable, long-path safe and retry transient failures."""

import importlib.util
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "chaos_engine_java_installer_7170", ROOT / "chaos-engine/packs/java/installer.py"
)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError("java pack installer is not loadable")
SPEC.loader.exec_module(MODULE)
subprocess = MODULE.subprocess


class MavenToolsCloneTest(unittest.TestCase):
    """Behaviour of ``clone_maven_tools_source``."""

    def test_clone_enables_long_paths_and_captures_stderr(self):
        calls = []

        def runner(command, **kwargs):
            calls.append((command, kwargs))
            Path(command[-1]).mkdir(parents=True)
            return subprocess.CompletedProcess(command, 0, "", "")

        with tempfile.TemporaryDirectory() as tmp:
            MODULE.clone_maven_tools_source(runner, "git", "v3.2.3", Path(tmp) / "source", sleep=lambda _: None)
        command, kwargs = calls[0]
        self.assertEqual(command[:3], ["git", "-c", "core.longpaths=true"])
        self.assertIn("clone", command)
        self.assertTrue(kwargs.get("capture_output"))

    def test_transient_failure_is_retried_on_a_clean_directory(self):
        attempts = []

        def runner(command, **kwargs):
            source = Path(command[-1])
            attempts.append(source.exists())
            source.mkdir(parents=True)
            if len(attempts) == 1:
                (source / "partial").write_text("x", encoding="utf-8")
                raise subprocess.CalledProcessError(128, command, "", "fatal: unable to access: Recv failure")
            return subprocess.CompletedProcess(command, 0, "", "")

        with tempfile.TemporaryDirectory() as tmp:
            MODULE.clone_maven_tools_source(runner, "git", "v3.2.3", Path(tmp) / "source", sleep=lambda _: None)
        self.assertEqual(attempts, [False, False])

    def test_final_failure_reports_git_stderr(self):
        def runner(command, **kwargs):
            raise subprocess.CalledProcessError(128, command, "", "fatal: Filename too long")

        with tempfile.TemporaryDirectory() as tmp:
            with self.assertRaises(ValueError) as raised:
                MODULE.clone_maven_tools_source(runner, "git", "v3.2.3", Path(tmp) / "source", sleep=lambda _: None)
        self.assertIn("Filename too long", str(raised.exception))
        self.assertIn("exit 128", str(raised.exception))


if __name__ == "__main__":
    unittest.main()
