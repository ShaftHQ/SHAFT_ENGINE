import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.ci.validate_agent_setup import (
    GENERATED_MEMORY_PATHS,
    KNOWN_SECRET_SCANNER_LANDMINE_FILES,
    run_memory_check,
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

    def test_missing_memory_binary_reports_actionable_error(self):
        with patch("scripts.ci.validate_agent_setup.shutil.which", return_value=None):
            errors = run_memory_check(self.root)
        self.assertEqual(len(errors), 1)
        error = errors[0]
        self.assertEqual(error["code"], "memory-check")
        self.assertEqual(error["path"], "memory")
        self.assertIn("PATH", error["message"])
        self.assertIn("install", error["message"].lower())
        self.assertNotIn("Traceback", error["message"])

    def test_overlong_relation_filename_hints_at_explicit_short_id(self):
        # A real `create_relation` patch left to auto-derive its id from two
        # ~90+ char endpoint ids produces a 222-char basename (issue #4110,
        # reproduced live against the real Memory CLI). The relation's own
        # `id`/`from`/`to` fields carry no length cap (relation.schema.json),
        # and `create_relation` already accepts an explicit custom `id` --
        # so the actionable fix is a short custom id, not a longer/relaxed
        # basename cap (which would reopen the Windows MAX_PATH risk this
        # check exists to catch).
        overlong_name = "gotcha-" + "a" * 160 + "-related-to-fact-" + "b" * 30
        self.write(
            f".memory/relations/{overlong_name}.json",
            json.dumps(
                {
                    "id": f"rel.{overlong_name}",
                    "from": "gotcha." + "a" * 160,
                    "predicate": "related_to",
                    "to": "fact." + "b" * 30,
                    "status": "active",
                    "created_at": "2026-01-01T00:00:00Z",
                    "updated_at": "2026-01-01T00:00:00Z",
                }
            ),
        )
        errors = [
            error
            for error in validate_memory_setup(self.root)
            if error["code"] == "memory-filename-length"
        ]
        self.assertEqual(len(errors), 1)
        self.assertIn("create_relation", errors[0]["message"])
        self.assertIn("explicit", errors[0]["message"])

    def test_rejects_new_memory_file_matching_secret_scanner_landmine(self):
        # The unpatched Aictx Memory CLI's `openai_api_key` rule is
        # `/sk-[A-Za-z0-9_-]{20,}/` with no `\b` anchor before `sk-`, so it
        # matches mid-word inside ordinary hyphenated slugs -- confirmed live
        # (issue #4005) via a from-scratch `npm install @aictx/memory@0.1.55`:
        # `memory check` hard-fails (exit 1) on any canonical file whose text
        # contains a word like "desk-" followed by 20+ word/hyphen characters.
        self.write(
            ".memory/memory/gotchas/new-thing-desk-abcdefghijklmnopqrstuvwxyz.md",
            "desk-abcdefghijklmnopqrstuvwxyz\n",
        )
        self.assertIn("memory-secret-landmine", self.codes())

    def test_known_preexisting_landmine_file_is_grandfathered(self):
        # The two files already committed before this check existed (#4005)
        # must not start failing every build; only NEW occurrences should.
        allowlisted_path = sorted(KNOWN_SECRET_SCANNER_LANDMINE_FILES)[0]
        self.write(allowlisted_path, "desk-abcdefghijklmnopqrstuvwxyz\n")
        self.assertNotIn("memory-secret-landmine", self.codes())

    def test_overlong_non_relation_filename_still_caught_without_relation_hint(self):
        # Negative test: relaxing the relation-file message must not open a
        # hole for genuinely over-long non-relation memory objects, which
        # have no analogous explicit-id override and must still be shortened.
        overlong_name = "gotcha-" + "c" * 170
        self.write(f".memory/memory/gotchas/{overlong_name}.md", "body")
        errors = [
            error
            for error in validate_memory_setup(self.root)
            if error["code"] == "memory-filename-length"
        ]
        self.assertEqual(len(errors), 1)
        self.assertNotIn("create_relation", errors[0]["message"])
        self.assertIn("shorten the object", errors[0]["message"])


if __name__ == "__main__":
    unittest.main()
