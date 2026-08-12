import subprocess
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RUNBOOK = ROOT / "tools/intellij-plugin-recording/RUNBOOK.md"
CAPTURE = ROOT / "tools/intellij-plugin-recording/capture-desktop.ps1"


class IntellijRecordingPowerShellTest(unittest.TestCase):
    def test_runbook_uses_powershell7_for_recording_scripts(self):
        text = RUNBOOK.read_text(encoding="utf-8")
        recording_lines = [line for line in text.splitlines()
                           if line.lstrip().startswith(("powershell ", "pwsh ")) and ".ps1" in line]
        self.assertTrue(recording_lines)
        self.assertTrue(all(line.lstrip().startswith("pwsh ") for line in recording_lines))

    def test_capture_fails_before_mutation_on_legacy_powershell(self):
        result = subprocess.run(
            ["powershell", "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", str(CAPTURE),
             "-OutputPath", str(ROOT / "build" / "must-not-exist.mp4")],
            capture_output=True, text=True, check=False,
        )
        self.assertNotEqual(0, result.returncode)
        self.assertIn("PowerShell 7 or newer", result.stderr + result.stdout)
        self.assertFalse((ROOT / "build" / "must-not-exist.mp4").exists())


if __name__ == "__main__":
    unittest.main()
