import re
import unittest
from pathlib import Path


ACTION_PATH = Path(__file__).resolve().parents[2] / ".github" / "actions" / "build-shaft-mcp-live-jar" / "action.yml"


class BuildShaftMcpLiveJarTest(unittest.TestCase):
    def test_launcher_build_retries_the_complete_maven_sequence(self):
        action = ACTION_PATH.read_text(encoding="utf-8")
        build_script = action.split("    - name: Write shaft-mcp launcher argfile", 1)[0]

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


if __name__ == "__main__":
    unittest.main()
