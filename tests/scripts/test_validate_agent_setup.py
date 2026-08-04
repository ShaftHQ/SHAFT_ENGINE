import json
import subprocess  # nosec B404 - tests drive the local git binary on fixtures.
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.ci.validate_agent_setup import (
    GENERATED_MEMORY_PATHS,
    KNOWN_SECRET_SCANNER_LANDMINE_FILES,
    collect_worktree_metrics,
    format_banner,
    reduction_percent,
    run_memory_check,
    validate_host_parity,
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

    def test_current_host_parity_matrix_is_complete(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_host_parity(repository_root), [])

    def test_host_parity_rejects_missing_evidence_and_named_check(self):
        self.write(
            "scripts/ci/agent_harness_parity.json",
            json.dumps(
                {
                    "version": 1,
                    "hosts": ["claude", "codex"],
                    "capabilities": [
                        {
                            "id": "entrypoint",
                            "owner": "missing-owner.md",
                            "claude": ["missing-claude.md"],
                            "codex": ["missing-codex.md"],
                            "check": "tests/scripts/test_validate_agent_setup.py::test_not_real",
                            "mode": "shared",
                        }
                    ],
                }
            ),
        )

        errors = validate_host_parity(self.root)

        self.assertIn("host-parity-path", {error["code"] for error in errors})

    def test_host_parity_reports_malformed_rows_without_crashing(self):
        self.write(
            "scripts/ci/agent_harness_parity.json",
            json.dumps({"version": 1, "hosts": ["claude", "codex"], "capabilities": [{"id": []}, 7]}),
        )

        errors = validate_host_parity(self.root)

        self.assertIn("host-parity-schema", {error["code"] for error in errors})

    def test_host_parity_reports_non_object_document_without_crashing(self):
        self.write("scripts/ci/agent_harness_parity.json", "[]")

        errors = validate_host_parity(self.root)

        self.assertEqual(errors[0]["code"], "host-parity-schema")

    def test_host_parity_requires_named_check_to_run_in_ci(self):
        self.write("owner.md", "owner")
        self.write("host.md", "host")
        self.write("tests/test_parity.py", "class TestParity:\n    def test_real(self):\n        pass\n")
        self.write(".github/workflows/pr-gate.yml", "run: python -m unittest tests.test_other\n")
        self.write(
            "scripts/ci/agent_harness_parity.json",
            json.dumps(
                {
                    "version": 1,
                    "hosts": ["claude", "codex"],
                    "capabilities": [
                        {
                            "id": "entrypoint",
                            "owner": "owner.md",
                            "claude": ["host.md"],
                            "codex": ["host.md"],
                            "check": "tests/test_parity.py::test_real",
                            "mode": "shared",
                        }
                    ],
                }
            ),
        )

        errors = validate_host_parity(self.root)

        self.assertIn("host-parity-ci", {error["code"] for error in errors})

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

    def git(self, *arguments):
        return subprocess.run(  # nosec B603 B607 - fixed git commands on a temp fixture.
            ["git", "-c", "core.longpaths=true", *arguments],
            cwd=self.root,
            capture_output=True,
            text=True,
            check=False,
        )

    def test_worktree_metrics_report_uncommitted_work(self):
        # Issue #4437: uncommitted work rotted in worktrees because nothing an
        # agent runs ever mentioned it.
        self.git("init", "-q", "-b", "main", ".")
        self.git("config", "user.email", "harness@example.invalid")
        self.git("config", "user.name", "Harness")
        self.write("notes.md", "committed\n")
        self.git("add", "notes.md")
        self.git("commit", "-qm", "initial")
        self.write("notes.md", "uncommitted edit\n")

        metrics = collect_worktree_metrics(self.root, run_external=False)

        advisories = metrics["worktree_advisories"]
        self.assertEqual(len(advisories), 1)
        self.assertIn("uncommitted", advisories[0])
        self.assertEqual(metrics["worktrees"][0]["state"], "uncommitted")

    def test_worktree_metrics_are_empty_outside_a_repository(self):
        metrics = collect_worktree_metrics(self.root, run_external=False)
        self.assertEqual(metrics["worktrees"], [])
        self.assertEqual(metrics["worktree_advisories"], [])

    def test_validator_carries_the_worktree_report_without_gating_on_it(self):
        # The report must ride along with --skip-external, the invocation
        # AGENTS.md prescribes -- and must never fail it, because concurrent
        # sessions legitimately hold dirty worktrees of their own.
        repository_root = Path(__file__).resolve().parents[2]
        errors, metrics = validate_repository(repository_root, run_external=False)
        self.assertIn("worktrees", metrics)
        self.assertIn("worktree_advisories", metrics)
        self.assertNotIn("worktree", {error["code"] for error in errors})

    def test_unconfigured_reduction_is_reported_as_absent_not_as_zero_percent(self):
        # #3745 retired the global reduction floor on purpose, so no baseline is
        # configured and the percentage branch never runs. Emitting the literal
        # 0 anyway published a non-measurement in the units of a measurement:
        # the banner read "0% reduction", which an agent cannot distinguish from
        # "measured, and nothing was reduced" -- the worse of the two readings.
        repository_root = Path(__file__).resolve().parents[2]
        _, metrics = validate_repository(repository_root, run_external=False)
        budget = json.loads(
            (repository_root / "scripts/ci/agent_guidance_budget.json").read_text(
                encoding="utf-8"
            )
        )
        self.assertNotIn("reduction_baseline_bytes", budget)
        self.assertIsNone(metrics["guidance_reduction_percent"])

    def test_configured_baseline_still_reports_a_real_reduction(self):
        # The absent case must not be bought by breaking the measured one: with
        # a baseline configured the percentage is computed exactly as before.
        self.assertEqual(reduction_percent(75_000, 150_000), 50.0)
        self.assertEqual(reduction_percent(150_000, 150_000), 0.0)
        self.assertIsNone(reduction_percent(129_090, 0))
        self.assertIsNone(reduction_percent(129_090, None))

    def test_banner_omits_the_reduction_clause_when_nothing_measured_it(self):
        banner = format_banner(
            {"guidance_bytes": 129_090, "guidance_reduction_percent": None, "memory_objects": 336}
        )
        self.assertEqual(banner, "Agent setup is valid: 129090 guidance bytes, 336 memory objects.")
        measured = format_banner(
            {"guidance_bytes": 75_000, "guidance_reduction_percent": 50.0, "memory_objects": 336}
        )
        self.assertIn("50.0% reduction", measured)

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
