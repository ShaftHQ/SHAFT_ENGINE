"""Contract tests for ChaosEngine learning ticket #5852."""

from __future__ import annotations

import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
RETRIEVE = ROOT / "chaos-engine/references/retrieve-first.md"
PROCESS = ROOT / "chaos-engine/references/process-owner-scrum-master.md"
ACTIVATION = ROOT / "chaos-engine/skills/self-improve/references/activation.md"
GOTCHAS = ROOT / ".memory/memory/gotchas"
SLUG = "learning-session-memory-save-is-the-default-write-path"


class Learning5852Test(unittest.TestCase):
    def test_playbook_requires_memory_save_not_manual_sidecars(self) -> None:
        text = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn("memory save --stdin", text)
        self.assertIn("#5852", text)
        self.assertIn("hand-author `.memory/**` sidecars", text)
        self.assertNotIn("`memory remember`", text)

    def test_retrieve_first_completion_uses_memory_save(self) -> None:
        text = RETRIEVE.read_text(encoding="utf-8")
        self.assertIn("`memory save --stdin`", text)
        self.assertIn("#5852", text)
        self.assertIn("hand-edit `.memory/**`", text)
        self.assertNotIn("`memory remember`", text)

    def test_process_owner_and_activation_drop_manual_sidecars(self) -> None:
        process = PROCESS.read_text(encoding="utf-8")
        self.assertIn("memory save --stdin", process)
        self.assertIn("#5852", process)
        self.assertIn("Manual `.memory/**` sidecar authoring", process)
        activation = ACTIVATION.read_text(encoding="utf-8")
        self.assertIn("memory save --stdin", activation)
        self.assertIn("#5852", activation)
        self.assertIn("hand-edit `.memory/**` sidecars", activation)

    def test_gotcha_md_and_json_exist_runtime_shaped_and_cite_issue(self) -> None:
        md = GOTCHAS / f"{SLUG}.md"
        js = GOTCHAS / f"{SLUG}.json"
        self.assertTrue(md.is_file(), md)
        self.assertTrue(js.is_file(), js)
        body = md.read_text(encoding="utf-8")
        self.assertIn("#5852", body)
        self.assertIn("memory save --stdin", body)
        self.assertIn("hand-edit", body)
        sidecar = json.loads(js.read_text(encoding="utf-8"))
        self.assertEqual(f"gotcha.{SLUG}", sidecar["id"])
        self.assertEqual("gotcha", sidecar["type"])
        self.assertNotIn("scope", sidecar)
        self.assertNotIn("facets", sidecar)
        self.assertIn("#5852", json.dumps(sidecar))


if __name__ == "__main__":
    unittest.main()
