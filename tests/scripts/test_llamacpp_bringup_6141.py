"""llamacpp bring-up stays loopback and does not use ft launch (#6141)."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


class LlamaCppBringupTest(unittest.TestCase):
    def test_skill_uses_llamacpp_id_and_forbids_ft_launch(self):
        text = (ROOT / "chaos-engine/skills/local-runtimes/references/local-openai-compat.md").read_text(encoding="utf-8")
        self.assertIn("llamacpp", text)
        self.assertNotIn("llama.cpp", text.casefold())
        self.assertIn("Do not use `ft launch`", text)
        self.assertIn("127.0.0.1:8080/health", text)
        self.assertIn("already downloaded GGUF", text)
        self.assertIn("loopback only", text)


if __name__ == "__main__":
    unittest.main()
