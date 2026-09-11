"""#5747: Codacy Complexity gate checklist + soft classifier-edit hook."""

from __future__ import annotations

import importlib.util
import io
import json
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
GATE = ROOT / "chaos-engine/references/codacy-complexity-gate.md"
LEVEL1 = ROOT / "chaos-engine/references/level-1-catalog.md"
SKILL = ROOT / "chaos-engine/skills/chaos-engine/SKILL.md"
PLAYBOOK = ROOT / "chaos-engine/references/work-github-playbook.md"
GUARD = ROOT / "chaos-engine/hooks/guard.py"


def load(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class CodacyComplexityGate5747Tests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.guard = load(GUARD, "ce_guard_codacy_5747")

    def test_checklist_documents_unit_red_parity_and_helpers(self):
        text = GATE.read_text(encoding="utf-8")
        self.assertIn("ACTION_REQUIRED", text)
        self.assertIn("unit", text.casefold())
        self.assertIn("kind-family", text.casefold())
        self.assertIn("classify*", text)
        self.assertIn("#5747", text)

    def test_catalog_and_router_point_at_checklist(self):
        level1 = LEVEL1.read_text(encoding="utf-8")
        skill = SKILL.read_text(encoding="utf-8")
        playbook = PLAYBOOK.read_text(encoding="utf-8")
        self.assertIn("codacy-complexity-gate.md", level1)
        self.assertIn("Codacy Complexity", level1)
        self.assertIn("codacy-complexity-gate.md", skill)
        self.assertIn("| Codacy Complexity |", skill)
        self.assertIn("codacy-complexity-gate.md", playbook)
        self.assertIn("ACTION_REQUIRED", playbook)

    def test_hint_fires_for_element_classifier_edit(self):
        hint = self.guard.classifier_complexity_gate_hint(
            "PreToolUse",
            mutation=True,
            tool_name="Edit",
            tool_input={
                "file_path": (
                    "shaft-engine/src/main/java/com/shaft/gui/element/"
                    "internal/interaction/ElementClassifier.java"
                ),
                "old_string": "a",
                "new_string": "b",
            },
        )
        self.assertIsNotNone(hint)
        self.assertIn("ACTION_REQUIRED", hint)
        self.assertIn("codacy-complexity-gate.md", hint)

    def test_hint_skips_unrelated_mutation(self):
        hint = self.guard.classifier_complexity_gate_hint(
            "PreToolUse",
            mutation=True,
            tool_name="Write",
            tool_input={"file_path": "README.md", "content": "hello"},
        )
        self.assertIsNone(hint)

    def test_hint_skips_non_mutation_and_non_pretool(self):
        self.assertIsNone(
            self.guard.classifier_complexity_gate_hint(
                "PreToolUse",
                mutation=False,
                tool_name="Edit",
                tool_input={"file_path": "ElementClassifier.java"},
            )
        )
        self.assertIsNone(
            self.guard.classifier_complexity_gate_hint(
                "PostToolUse",
                mutation=True,
                tool_name="Edit",
                tool_input={"file_path": "ElementClassifier.java"},
            )
        )

    def test_pretool_use_emits_soft_context_without_blocking(self):
        with tempfile.TemporaryDirectory() as temporary:
            event = {
                "hook_event_name": "PreToolUse",
                "session_id": "codacy-5747-soft",
                "cwd": temporary,
                "tool_name": "Edit",
                "tool_input": {
                    "file_path": (
                        "shaft-engine/src/main/java/com/shaft/gui/element/"
                        "internal/interaction/ElementClassifier.java"
                    ),
                    "old_string": "x",
                    "new_string": "y",
                },
            }
            buffer = io.StringIO()
            with redirect_stdout(buffer):
                code = self.guard._run_event(event, "claude")
            self.assertEqual(0, code)
            payload = json.loads(buffer.getvalue().strip().splitlines()[-1])
            self.assertIn("additionalContext", payload)
            self.assertIn("ACTION_REQUIRED", payload["additionalContext"])
            self.assertNotEqual("block", payload.get("decision"))


if __name__ == "__main__":
    unittest.main()
