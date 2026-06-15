import json
import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_agent_setup import (
    GENERATED_MEMORY_PATHS,
    validate_memory_setup,
    validate_repository,
)


class ValidateAgentSetupTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.write(
            ".memory/config.json",
            json.dumps(
                {
                    "version": 4,
                    "project": {"id": "project.shaft-engine", "name": "Shaft Engine"},
                    "memory": {
                        "autoIndex": True,
                        "defaultTokenBudget": 600,
                        "saveContextPacks": False,
                    },
                    "git": {"trackContextPacks": False},
                }
            ),
        )
        self.write(".memory/events.jsonl", "")
        for name in ("config", "event", "object", "patch", "relation"):
            self.write(f".memory/schema/{name}.schema.json", "{}")
        self.write(".memory/memory/project.md", "# SHAFT Engine\n")
        self.write(".memory/memory/project.json", "{}")
        self.write(".memory/memory/architecture.md", "# Architecture\n")
        self.write(".memory/memory/architecture.json", "{}")
        self.write(".gitignore", "\n".join(sorted(GENERATED_MEMORY_PATHS)) + "\n")
        self.write(
            ".codex/config.toml",
            """[mcp_servers.shaft-memory]
command = "npx"
args = ["--yes", "--package", "@aictx/memory@0.1.55", "--", "memory-mcp"]
cwd = ".."
enabled_tools = ["load_memory", "search_memory", "inspect_memory", "remember_memory"]
default_tools_approval_mode = "auto"
startup_timeout_sec = 30
tool_timeout_sec = 60
required = false

[mcp_servers.shaft-memory.tools.remember_memory]
approval_mode = "prompt"
""",
        )

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def codes(self):
        return {error["code"] for error in validate_memory_setup(self.root)}

    def test_valid_memory_setup_passes(self):
        self.assertEqual(validate_memory_setup(self.root), [])

    def test_rejects_large_default_memory_budget(self):
        config = json.loads((self.root / ".memory/config.json").read_text(encoding="utf-8"))
        config["memory"]["defaultTokenBudget"] = 6000
        self.write(".memory/config.json", json.dumps(config))
        self.assertIn("memory-config", self.codes())

    def test_rejects_broader_mcp_tool_surface(self):
        path = self.root / ".codex/config.toml"
        content = path.read_text(encoding="utf-8").replace(
            '"remember_memory"]', '"remember_memory", "save_memory_patch"]'
        )
        self.write(".codex/config.toml", content)
        self.assertIn("memory-mcp", self.codes())

    def test_current_repository_setup_is_valid_without_external_calls(self):
        repository_root = Path(__file__).resolve().parents[2]
        errors, _ = validate_repository(repository_root, run_external=False)
        self.assertEqual(errors, [])


if __name__ == "__main__":
    unittest.main()
