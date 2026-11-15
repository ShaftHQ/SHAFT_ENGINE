"""#6974/#6973: a selected project pack's postInstall (SHAFT agentic installer) runs automatically."""

from __future__ import annotations

import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]





class PackPostInstallTest(unittest.TestCase):
    def test_shaft_pack_declares_the_agentic_installer_with_skills_and_cli(self) -> None:
        profile = json.loads((ROOT / "shaft-skills/ce-pack/profile.json").read_text(encoding="utf-8"))
        self.assertEqual("scripts/mcp/install_shaft_agentic_tools.py", profile["postInstall"]["script"])
        self.assertEqual(["--install-shaft-skills", "--install-shaft-cli"], profile["postInstall"]["args"])
        self.assertTrue((ROOT / profile["postInstall"]["script"]).is_file())


if __name__ == "__main__":
    unittest.main()
