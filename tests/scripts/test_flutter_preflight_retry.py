"""#7730: Flutter E2E preflight gets a long server launch timeout and a session-open failure earns the one authorized retry."""

from __future__ import annotations

import os
import shutil
import stat
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/run_ubuntu_flutter_emulator_e2e.sh"


class FlutterPreflightRetryTest(unittest.TestCase):
    def setUp(self) -> None:
        self.text = SCRIPT.read_text(encoding="utf-8")

    def test_preflight_sets_server_launch_timeout(self) -> None:
        self.assertIn('"appium:flutterServerLaunchTimeout": 180000', self.text)

    def test_preflight_failure_marker_triggers_fallback(self) -> None:
        self.assertIn(': > "${FLUTTER_PREFLIGHT_FAIL_MARKER', self.text)
        fallback = self.text.split('elif [ "$primary_rc" -eq 124 ]', 1)[1].split("then", 1)[0]
        self.assertIn("FLUTTER_PREFLIGHT_FAIL_MARKER", fallback)

    def test_marker_cleared_before_primary(self) -> None:
        head = self.text.split("timeout --signal=TERM", 1)[0]
        self.assertIn('rm -f "${FLUTTER_PREFLIGHT_FAIL_MARKER', head)

    def test_open_flutter_session_writes_marker_when_curl_returns_no_session_id(self) -> None:
        """Behavioral: stub curl/jq path so open_flutter_session sees no sessionId."""
        start = self.text.index("open_flutter_session() {")
        end = self.text.index("\nrun_flutter_maven() {", start)
        function_body = self.text[start:end].rstrip() + "\n"

        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            bin_dir = tmp_path / "bin"
            bin_dir.mkdir()
            curl = bin_dir / "curl"
            curl.write_text(
                "#!/bin/sh\n"
                "# Stub Appium: always return a session create body without sessionId.\n"
                "echo '{\"value\":{}}'\n",
                encoding="utf-8",
            )
            curl.chmod(curl.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

            marker = tmp_path / "flutter-preflight-failed"
            appium_log = tmp_path / "appium-server.log"
            appium_log.write_text("stub appium log\n", encoding="utf-8")

            runner = tmp_path / "run_open_session.sh"
            runner.write_text(
                textwrap.dedent(
                    f"""\
                    #!/usr/bin/env bash
                    set -eu
                    export PATH="{bin_dir}:$PATH"
                    export FLUTTER_PREFLIGHT_FAIL_MARKER="{marker}"
                    appiumLog="{appium_log}"
                    APK_PATH="{tmp_path / "fake.apk"}"
                    {function_body}
                    open_flutter_session
                    """
                ),
                encoding="utf-8",
            )
            runner.chmod(runner.stat().st_mode | stat.S_IXUSR)

            bash = shutil.which("bash")
            self.assertIsNotNone(bash, "bash required for open_flutter_session fixture")
            completed = subprocess.run(  # nosec B603 - resolved bash + generated fixture script
                [bash, str(runner)],
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
                check=False,
                cwd=str(tmp_path),
                env={**os.environ, "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}"},
            )
            self.assertNotEqual(completed.returncode, 0, completed.stdout + completed.stderr)
            self.assertTrue(
                marker.is_file(),
                f"marker missing; stdout={completed.stdout!r} stderr={completed.stderr!r}",
            )
            self.assertIn("no sessionId", completed.stdout + completed.stderr)


if __name__ == "__main__":
    unittest.main()
