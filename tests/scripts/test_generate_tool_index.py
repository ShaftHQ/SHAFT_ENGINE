"""
Tests for scripts/mcp/generate_tool_index.py -- the Python merge half of the tool-index
generation pipeline (design doc Decision 4 / amendment A5): it merges the Java-dumped mechanical
schema (shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index-mechanical.json, produced by
ToolIndexMechanicalDumpTest from the live Spring-registered ToolCallback/@McpTool schemas) with the
hand-curated overlay (tool-index-overlay.json: mutation/sensitive/intentKeywords/slashAlias/
cliCommand/example) into the canonical checked-in shaft-mcp/src/main/resources/META-INF/shaft-mcp/
tool-index.json that shaft-cli, the IntelliJ plugin, and skills all read.
"""

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = REPO_ROOT / "scripts" / "mcp" / "generate_tool_index.py"
SPEC = importlib.util.spec_from_file_location("generate_tool_index", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
if SPEC.loader is None:
    raise ImportError(f"Unable to load {MODULE_PATH}")
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


def _mechanical(tools):
    return {"schemaVersion": "1.0", "tools": tools}


def _overlay(tools):
    return {"schemaVersion": "1.0", "tools": tools}


class MergeToolIndexTest(unittest.TestCase):
    """Unit tests for the pure merge function against small in-memory fixtures."""

    def test_merges_mechanical_and_overlay_fields_for_one_tool(self):
        mechanical = _mechanical([
            {
                "name": "element_click",
                "service": "ElementService",
                "description": "clicks an element",
                "params": [
                    {"name": "locatorStrategy", "type": "string", "required": True, "description": None},
                    {"name": "mode", "type": "string", "required": False, "description": None},
                ],
            }
        ])
        overlay = _overlay({
            "element_click": {
                "mutation": True,
                "sensitive": False,
                "intentKeywords": ["click the button"],
            }
        })

        document = MODULE.merge(mechanical, overlay)

        self.assertEqual("1.0", document["schemaVersion"])
        self.assertEqual(1, len(document["tools"]))
        tool = document["tools"][0]
        self.assertEqual("element_click", tool["name"])
        self.assertEqual("ElementService", tool["service"])
        self.assertEqual("clicks an element", tool["description"])
        self.assertTrue(tool["mutation"])
        self.assertFalse(tool["sensitive"])
        self.assertEqual(["click the button"], tool["intentKeywords"])
        self.assertIsNone(tool["slashAlias"])
        self.assertIsNone(tool["cliCommand"])
        self.assertIsNone(tool["example"])
        self.assertEqual(2, len(tool["params"]))
        self.assertEqual("locatorStrategy", tool["params"][0]["name"])
        self.assertTrue(tool["params"][0]["required"])
        self.assertIsNone(tool["params"][0]["default"])

    def test_merge_applies_curated_param_default_override(self):
        mechanical = _mechanical([
            {
                "name": "browser_delete_cookies",
                "service": "BrowserService",
                "description": "deletes a cookie",
                "params": [{"name": "name", "type": "string", "required": False, "description": None}],
            }
        ])
        overlay = _overlay({
            "browser_delete_cookies": {
                "mutation": True,
                "sensitive": False,
                "paramDefaults": {"name": "all cookies when omitted"},
            }
        })

        document = MODULE.merge(mechanical, overlay)

        self.assertEqual("all cookies when omitted", document["tools"][0]["params"][0]["default"])

    def test_merge_fails_when_overlay_is_missing_a_mechanical_tool(self):
        mechanical = _mechanical([
            {"name": "driver_quit", "service": "EngineService", "description": "quits", "params": []},
        ])
        overlay = _overlay({})

        with self.assertRaises(MODULE.ToolIndexMergeError) as raised:
            MODULE.merge(mechanical, overlay)
        self.assertIn("driver_quit", str(raised.exception))

    def test_merge_fails_when_overlay_references_an_unknown_tool(self):
        mechanical = _mechanical([
            {"name": "driver_quit", "service": "EngineService", "description": "quits", "params": []},
        ])
        overlay = _overlay({
            "driver_quit": {"mutation": True, "sensitive": False},
            "ghost_tool_that_was_deleted": {"mutation": False, "sensitive": False},
        })

        with self.assertRaises(MODULE.ToolIndexMergeError) as raised:
            MODULE.merge(mechanical, overlay)
        self.assertIn("ghost_tool_that_was_deleted", str(raised.exception))


class GenerateToolIndexCheckModeTest(unittest.TestCase):
    """--check mode against real fixture files on disk, mirroring generate_shaft_skills_tool_catalog.py."""

    def test_check_passes_when_output_matches_merge_of_inputs(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            mechanical_path = tmp_path / "tool-index-mechanical.json"
            overlay_path = tmp_path / "tool-index-overlay.json"
            index_path = tmp_path / "tool-index.json"

            mechanical_path.write_text(json.dumps(_mechanical([
                {"name": "driver_quit", "service": "EngineService", "description": "quits", "params": []},
            ])), encoding="utf-8")
            overlay_path.write_text(json.dumps(_overlay({
                "driver_quit": {"mutation": True, "sensitive": False},
            })), encoding="utf-8")

            exit_code = MODULE.main([
                "--check",
                "--mechanical-path", str(mechanical_path),
                "--overlay-path", str(overlay_path),
                "--output-path", str(index_path),
            ])
            self.assertEqual(1, exit_code, "should fail: tool-index.json does not exist yet")

            write_exit_code = MODULE.main([
                "--mechanical-path", str(mechanical_path),
                "--overlay-path", str(overlay_path),
                "--output-path", str(index_path),
            ])
            self.assertEqual(0, write_exit_code)

            recheck_exit_code = MODULE.main([
                "--check",
                "--mechanical-path", str(mechanical_path),
                "--overlay-path", str(overlay_path),
                "--output-path", str(index_path),
            ])
            self.assertEqual(0, recheck_exit_code, "should pass: freshly generated output matches disk")

    def test_real_checked_in_tool_index_matches_the_real_mechanical_and_overlay_fixtures(self):
        """Integration proof: the actual checked-in tool-index.json is not hand-edited drift."""
        exit_code = MODULE.main(["--check"])
        self.assertEqual(0, exit_code,
                          "shaft-mcp/src/main/resources/META-INF/shaft-mcp/tool-index.json is out of date; "
                          "run 'python3 scripts/mcp/generate_tool_index.py' and commit the result")


if __name__ == "__main__":
    unittest.main()
