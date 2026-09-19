"""Regression for #6000: catalog apply inserts are idempotent on tool name."""

from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = REPO_ROOT / "scripts" / "mcp" / "insert_mcp_tool_manifest_entry.py"


def _load_module():
    spec = importlib.util.spec_from_file_location("insert_mcp_tool_manifest_entry", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


MODULE = _load_module()


def _write(path: Path, document: object) -> None:
    path.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")


class InsertMcpToolManifestEntryTest(unittest.TestCase):
    def test_second_insert_does_not_duplicate_design_lint(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            manifest = root / "mcp-tool-manifest.json"
            _write(
                manifest,
                {
                    "schemaVersion": "1.0",
                    "tools": [
                        {
                            "name": "design_coverage",
                            "mutation": False,
                            "sensitive": True,
                            "deprecated": False,
                        },
                        {
                            "name": "doctor_analyze_failed_allure",
                            "mutation": True,
                            "sensitive": True,
                            "deprecated": False,
                        },
                    ],
                },
            )
            argv = [
                "--name",
                "design_lint",
                "--mutation",
                "false",
                "--sensitive",
                "true",
                "--after",
                "design_coverage",
                "--manifest",
                str(manifest),
            ]
            self.assertEqual(0, MODULE.main(argv))
            self.assertEqual(0, MODULE.main(argv))
            tools = json.loads(manifest.read_text(encoding="utf-8"))["tools"]
            names = [tool["name"] for tool in tools]
            self.assertEqual(1, names.count("design_lint"))
            self.assertEqual(
                ["design_coverage", "design_lint", "doctor_analyze_failed_allure"],
                names,
            )

    def test_check_only_fails_on_existing_duplicates(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            manifest = Path(tmp) / "mcp-tool-manifest.json"
            _write(
                manifest,
                {
                    "schemaVersion": "1.0",
                    "tools": [
                        {
                            "name": "design_lint",
                            "mutation": False,
                            "sensitive": True,
                            "deprecated": False,
                        },
                        {
                            "name": "design_lint",
                            "mutation": False,
                            "sensitive": True,
                            "deprecated": False,
                        },
                    ],
                },
            )
            with self.assertRaises(SystemExit) as raised:
                MODULE.main(
                    [
                        "--name",
                        "design_lint",
                        "--mutation",
                        "false",
                        "--sensitive",
                        "true",
                        "--manifest",
                        str(manifest),
                        "--check-only",
                    ]
                )
            self.assertEqual(2, raised.exception.code)

    def test_overlay_insert_is_idempotent_on_key(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            overlay = Path(tmp) / "tool-index-overlay.json"
            _write(
                overlay,
                {
                    "schemaVersion": "1.0",
                    "tools": {
                        "design_coverage": {
                            "mutation": False,
                            "sensitive": True,
                            "deprecated": False,
                        }
                    },
                },
            )
            argv = [
                "--name",
                "design_lint",
                "--mutation",
                "false",
                "--sensitive",
                "true",
                "--manifest",
                str(Path(tmp) / "unused-manifest.json"),
                "--overlay",
                str(overlay),
            ]
            # Manifest path is required by CLI; seed a valid empty-ish list file.
            _write(
                Path(tmp) / "unused-manifest.json",
                {"schemaVersion": "1.0", "tools": []},
            )
            self.assertEqual(0, MODULE.main(argv))
            self.assertEqual(0, MODULE.main(argv))
            tools = json.loads(overlay.read_text(encoding="utf-8"))["tools"]
            self.assertEqual(1, list(tools).count("design_lint"))
            self.assertIn("design_lint", tools)

    def test_live_repo_manifest_is_unique(self) -> None:
        live = (
            REPO_ROOT
            / "shaft-mcp"
            / "src"
            / "test"
            / "resources"
            / "fixtures"
            / "mcp-tool-manifest.json"
        )
        self.assertEqual(
            0,
            MODULE.main(
                [
                    "--name",
                    "design_lint",
                    "--mutation",
                    "false",
                    "--sensitive",
                    "true",
                    "--manifest",
                    str(live),
                    "--check-only",
                ]
            ),
        )


if __name__ == "__main__":
    unittest.main()
