"""#7730: Flutter E2E preflight gets a long server launch timeout and a session-open failure earns the one authorized retry."""
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/ci/run_ubuntu_flutter_emulator_e2e.sh"


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
        head = self.text.split('timeout --signal=TERM', 1)[0]
        self.assertIn('rm -f "${FLUTTER_PREFLIGHT_FAIL_MARKER', head)


if __name__ == "__main__":
    unittest.main()
