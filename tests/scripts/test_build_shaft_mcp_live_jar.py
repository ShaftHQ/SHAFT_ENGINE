import os
import re
import subprocess  # nosec B404 - tests drive the build script with controlled repository state.
import tempfile
import unittest
from pathlib import Path


ACTION_PATH = Path(__file__).resolve().parents[2] / ".github" / "actions" / "build-shaft-mcp-live-jar" / "action.yml"


class BuildShaftMcpLiveJarTest(unittest.TestCase):
    @staticmethod
    def _build_script() -> str:
        action = ACTION_PATH.read_text(encoding="utf-8")
        return action.split("      run: |\n", 1)[1].split(
            "    - name: Write shaft-mcp launcher argfile", 1
        )[0]

    def test_launcher_build_retries_the_complete_maven_sequence(self):
        build_script = self._build_script()

        self.assertIn("for attempt in 1 2 3; do", build_script)
        self.assertIn('sleep "$((attempt * 10))"', build_script)
        self.assertIn('exit "$status"', build_script)
        self.assertRegex(
            build_script,
            re.compile(
                r"mvn --batch-mode -pl shaft-mcp -am install .*?\n"
                r"\s*&& mvn --batch-mode -pl shaft-mcp dependency:copy-dependencies .*?\n"
                r"\s*status=\$\?",
                re.DOTALL,
            ),
        )

    def test_launcher_build_retries_after_copy_dependencies_fails_under_errexit(self):
        with tempfile.TemporaryDirectory() as temporary_directory:
            temporary_path = Path(temporary_directory)
            maven_log = temporary_path / "maven.log"
            fake_maven = temporary_path / "mvn"
            fake_maven.write_bytes(
                b"#!/usr/bin/env bash\n"
                b"log=\"$(dirname \"$0\")/maven.log\"\n"
                b"printf '%s\\n' \"$*\" >> \"$log\"\n"
                b"if [ \"$(wc -l < \"$log\")\" -eq 2 ]; then\n"
                b"  exit 17\n"
                b"fi\n"
            )
            fake_sleep = temporary_path / "sleep"
            fake_sleep.write_bytes(b"#!/usr/bin/env bash\n")
            fake_maven.chmod(0o755)
            fake_sleep.chmod(0o755)
            environment = os.environ | {
                "PATH": f"{temporary_path}{os.pathsep}{os.environ['PATH']}",
            }

            completed = subprocess.run(
                ["bash", "-e"],
                input=(
                    b'PATH="$(dirname "$(command -v mvn)"):$PATH"\n'
                    + self._build_script().encode()
                ),
                capture_output=True,
                env=environment,
            )

            self.assertEqual(completed.returncode, 0, completed.stderr.decode())
            maven_commands = maven_log.read_text(encoding="utf-8").splitlines()
            self.assertEqual(len(maven_commands), 4)
            self.assertIn("install", maven_commands[0])
            self.assertIn("dependency:copy-dependencies", maven_commands[1])
            self.assertIn("install", maven_commands[2])
            self.assertIn("dependency:copy-dependencies", maven_commands[3])

    def test_launcher_build_propagates_the_final_copy_dependencies_failure(self):
        with tempfile.TemporaryDirectory() as temporary_directory:
            temporary_path = Path(temporary_directory)
            maven_log = temporary_path / "maven.log"
            fake_maven = temporary_path / "mvn"
            fake_maven.write_bytes(
                b"#!/usr/bin/env bash\n"
                b"log=\"$(dirname \"$0\")/maven.log\"\n"
                b"printf '%s\\n' \"$*\" >> \"$log\"\n"
                b"case \"$*\" in *dependency:copy-dependencies*) exit 17;; esac\n"
            )
            fake_sleep = temporary_path / "sleep"
            fake_sleep.write_bytes(b"#!/usr/bin/env bash\n")
            fake_maven.chmod(0o755)
            fake_sleep.chmod(0o755)
            environment = os.environ | {
                "PATH": f"{temporary_path}{os.pathsep}{os.environ['PATH']}",
            }

            completed = subprocess.run(
                ["bash", "-e"],
                input=(
                    b'PATH="$(dirname "$(command -v mvn)"):$PATH"\n'
                    + self._build_script().encode()
                ),
                capture_output=True,
                env=environment,
            )

            self.assertEqual(completed.returncode, 17, completed.stderr.decode())
            maven_commands = maven_log.read_text(encoding="utf-8").splitlines()
            self.assertEqual(len(maven_commands), 6)
            for install, copy_dependencies in zip(maven_commands[::2], maven_commands[1::2]):
                self.assertIn("install", install)
                self.assertIn("dependency:copy-dependencies", copy_dependencies)


if __name__ == "__main__":
    unittest.main()
