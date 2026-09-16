"""Learning contracts for #5882 (enabled_providers) and #5883 (llamacpp spelling)."""

from __future__ import annotations

import json
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SKILLS = ROOT / "chaos-engine" / "skills"
PROOF = ROOT / "chaos-engine" / "guides" / "freetoken-5867-closeout-proof.md"


class Learning5882EnabledProvidersTest(unittest.TestCase):
    def test_dispatch_source_emits_enabled_providers(self):
        text = (SKILLS / "local-agency" / "scripts" / "dispatch.py").read_text(encoding="utf-8")
        self.assertIn('"enabled_providers"', text)
        self.assertIn("OpenCode merges configs", text)

    def test_local_agency_skill_documents_allowlist(self):
        text = (SKILLS / "local-agency" / "SKILL.md").read_text(encoding="utf-8")
        self.assertIn("enabled_providers", text)


class Learning5883LlamacppSpellingTest(unittest.TestCase):
    def test_scanned_skills_prefer_llamacpp_backend_id(self):
        offenders: list[str] = []
        for path in SKILLS.glob("*/SKILL.md"):
            text = path.read_text(encoding="utf-8")
            if "llama.cpp" in text:
                offenders.append(str(path.relative_to(ROOT)))
        self.assertEqual(offenders, [], msg=f"use llamacpp in scanned skills: {offenders}")

    def test_catalog_uses_llamacpp(self):
        catalog = (ROOT / "chaos-engine" / "references" / "level-1-catalog.md").read_text(encoding="utf-8")
        self.assertIn("llamacpp", catalog)
        self.assertNotIn("llama.cpp", catalog)


class FreeToken5867ProofReceiptTest(unittest.TestCase):
    def test_proof_receipt_links_children_and_loop(self):
        text = PROOF.read_text(encoding="utf-8")
        for needle in ("#5868", "#5869", "#5870", "#5871", "#5872", "#5882", "#5883", "enabled_providers", "OpenCode"):
            self.assertIn(needle, text)


if __name__ == "__main__":
    unittest.main()
