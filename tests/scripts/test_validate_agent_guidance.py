import json

# subprocess is used only to read this repository's own history with fixed,
# list-args `git show` (never shell=True, no untrusted command construction).
import subprocess  # nosec B404
import sys
import tempfile
import unittest
from pathlib import Path

from scripts.ci.validate_agent_guidance import (
    find_orphaned_sibling_claims,
    parse_frontmatter,
    validate_repository,
)


class ValidateAgentGuidanceTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.write(
            "AGENTS.md",
            "# Agents\n\n[Local](docs/local.md)\n\n"
            "## Routing\n\nEntrypoint: `.agents/skills/act-as-mohab/SKILL.md`.\n",
        )
        self.write("CLAUDE.md", "# Claude\n\n@AGENTS.md\n")
        self.write(".github/copilot-instructions.md", "# Copilot\n")
        self.write(
            ".github/instructions/source.instructions.md",
            '---\napplyTo: "**/src/main/java/**/*.java"\n---\n\n# Source\n',
        )
        self.write(
            ".github/instructions/tests.instructions.md",
            '---\napplyTo: "**/src/test/java/**/*.java"\n---\n\n# Tests\n',
        )
        self.write("module/src/main/java/Example.java", "class Example {}\n")
        self.write("module/src/test/java/ExampleTest.java", "class ExampleTest {}\n")
        self.write("docs/local.md", "# Local\n")
        self.write(
            ".github/skills/example/SKILL.md",
            "---\nname: example\ndescription: Example task workflow.\n---\n\n# Example\n",
        )
        self.write(
            ".agents/skills/example-bridge/SKILL.md",
            "---\nname: example-bridge\ndescription: Load the example workflow.\n---\n\n# Bridge\n",
        )
        self.write(
            ".github/workflows/refresh-agent-instructions.yml",
            """name: Refresh
on:
  workflow_dispatch:
    inputs:
      reason:
        required: true
      force_ai:
        default: false
steps:
  - id: audit
    run: python3 scripts/ci/validate_agent_setup.py
  - if: steps.audit.outputs.needs_ai == 'true'
    uses: openai/codex-action@v1
  - id: changes
    run: |
      case "$file" in
        .agents/skills/*|.claude/skills/*|.github/instructions/*|.github/skills/*)
          ;;
      esac
""",
        )
        self.budget = {
            "file_budgets": {
                "AGENTS.md": {"max_bytes": 1000},
                "CLAUDE.md": {"max_bytes": 1000, "max_lines": 20},
                ".github/copilot-instructions.md": {"max_chars": 3999},
            },
            "host_contexts": {
                "codex": ["AGENTS.md"],
                "claude": ["AGENTS.md", "CLAUDE.md"],
                "copilot": ["AGENTS.md", ".github/copilot-instructions.md"],
            },
            "max_estimated_tokens_per_host": 10000,
            "active_guidance_globs": [
                "AGENTS.md",
                "CLAUDE.md",
                ".agents/skills/*/SKILL.md",
                ".github/copilot-instructions.md",
                ".github/instructions/*.instructions.md",
                ".github/skills/*/SKILL.md",
            ],
            "reference_scan_globs": [
                "AGENTS.md",
                "CLAUDE.md",
                ".agents/skills/*/SKILL.md",
                ".github/copilot-instructions.md",
                ".github/instructions/*.instructions.md",
                ".github/skills/*/SKILL.md",
            ],
            "scope_files": [
                ".github/instructions/source.instructions.md",
                ".github/instructions/tests.instructions.md",
            ],
            "skills_roots": [".agents/skills", ".github/skills"],
            "duplicate_paragraph_min_chars": 80,
            "forbidden_patterns": [
                {"pattern": "(?i)before every commit", "message": "per-commit ceremony"}
            ],
            "stale_references": ["retired.md"],
            "refresh_workflow": ".github/workflows/refresh-agent-instructions.yml",
        }
        self.budget_path = self.root / "scripts/ci/agent_guidance_budget.json"
        self.write_budget()

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def write_budget(self):
        self.budget_path.parent.mkdir(parents=True, exist_ok=True)
        self.budget_path.write_text(json.dumps(self.budget), encoding="utf-8")

    def codes(self):
        return {error["code"] for error in validate_repository(self.root, self.budget_path)}

    def test_valid_repository_passes(self):
        self.assertEqual(validate_repository(self.root, self.budget_path), [])

    def test_rejects_oversized_file(self):
        self.budget["file_budgets"]["AGENTS.md"]["max_bytes"] = 10
        self.write_budget()
        self.assertIn("size-budget", self.codes())

    def test_enforces_copilot_4000_character_limit(self):
        self.write(".github/copilot-instructions.md", "x" * 4000)
        self.assertIn("character-budget", self.codes())

    def test_total_reduction_is_noop_when_unconfigured(self):
        # The global byte-reduction floor (reduction_baseline_bytes /
        # minimum_reduction_percent) was replaced by per-surface caps matched to
        # actual load cost (issue #3745): when those keys are absent from the
        # budget -- as in the default self.budget used across this suite --
        # validate_total_reduction must never fire, no matter how large the
        # scanned guidance is.
        self.budget["total_guidance_globs"] = ["AGENTS.md"]
        self.write_budget()
        self.write("AGENTS.md", "x" * 500_000)
        self.assertNotIn("total-reduction", self.codes())

    def test_a_glob_file_budget_caps_every_file_the_pattern_matches(self):
        # The on-demand reference surface is many files under one directory, and
        # the cost a task actually pays is the single largest one it loads --
        # never their sum, which is what the retired global pool measured
        # (#3745, #4458). Enumerating 31 paths in file_budgets to express that
        # would rot on the next added file, so a budget key may be a glob and
        # binds each matched file on its own.
        self.write(".agents/skills/example-bridge/references/big.md", "x" * 400)
        self.write(".agents/skills/example-bridge/references/small.md", "x\n")
        self.budget["file_budgets"][".agents/skills/*/references/**/*.md"] = {
            "max_bytes": 100
        }
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        offenders = {
            error["path"] for error in errors if error["code"] == "size-budget"
        }
        self.assertEqual(
            offenders, {".agents/skills/example-bridge/references/big.md"}
        )

    def test_a_glob_file_budget_matching_nothing_is_reported_as_missing(self):
        # A cap on a surface that no longer exists is stale config, and it fails
        # open: the pattern quietly matches zero files and the budget looks
        # enforced. Same verdict a configured literal path gets when it is gone.
        self.budget["file_budgets"][".agents/skills/*/references/**/*.md"] = {
            "max_bytes": 100
        }
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn(
            (".agents/skills/*/references/**/*.md", "missing-file"),
            {(error["path"], error["code"]) for error in errors},
        )

    def test_a_glob_file_budget_that_escapes_the_root_is_missing_not_a_crash(self):
        # relative() resolves before it subtracts the root, so a match outside
        # the root raises ValueError there and takes down the whole validation
        # run -- a budget file typo turning into a stack trace instead of an
        # issue. The literal branch was guarded against this; the glob branch
        # was not.
        outside = self.root.parent / "outside-the-root.md"
        outside.write_text("x" * 400, encoding="utf-8")
        self.addCleanup(outside.unlink)
        self.budget["file_budgets"]["../outside-the-root.md"] = {"max_bytes": 10}
        self.budget["file_budgets"]["../outside*.md"] = {"max_bytes": 10}
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn(
            ("../outside*.md", "missing-file"),
            {(error["path"], error["code"]) for error in errors},
        )

    def test_an_active_guidance_glob_matching_nothing_is_reported_as_empty(self):
        # active_guidance_globs feeds validate_forbidden_patterns and
        # validate_duplicate_paragraphs, both "every matched file must hold"
        # checks: a pattern that resolves to zero files -- a moved directory,
        # a typo -- used to shrink the scanned set with no trace, so the check
        # kept passing having verified nothing for it (#4481).
        self.budget["active_guidance_globs"].append(".agents/skills/*/moved-away/**/*.md")
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn(
            (".agents/skills/*/moved-away/**/*.md", "empty-glob"),
            {(error["path"], error["code"]) for error in errors},
        )

    def test_a_reference_scan_glob_matching_nothing_is_reported_as_empty(self):
        # reference_scan_globs feeds validate_local_references and
        # validate_stale_references -- same fail-open shape as
        # active_guidance_globs, a different consuming pair of checks.
        self.budget["reference_scan_globs"].append(".agents/skills/*/moved-away/**/*.md")
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn(
            (".agents/skills/*/moved-away/**/*.md", "empty-glob"),
            {(error["path"], error["code"]) for error in errors},
        )

    def test_a_total_guidance_glob_matching_nothing_is_reported_as_empty(self):
        # total_guidance_globs only runs once reduction_baseline_bytes and
        # minimum_reduction_percent are configured (see
        # test_total_reduction_is_noop_when_unconfigured); once it does, the
        # same empty-glob reporting must apply to its own copy of the list.
        self.budget["reduction_baseline_bytes"] = 1_000_000
        self.budget["minimum_reduction_percent"] = 10
        self.budget["total_guidance_globs"] = [
            "AGENTS.md",
            ".agents/skills/*/moved-away/**/*.md",
        ]
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn(
            (".agents/skills/*/moved-away/**/*.md", "empty-glob"),
            {(error["path"], error["code"]) for error in errors},
        )

    def test_an_active_guidance_glob_key_that_is_deleted_is_reported_as_empty(self):
        # expand_reported_globs only guards patterns *within* an already
        # present list -- deleting the key itself makes its loop run zero
        # times, so no empty-glob issue fires and every downstream check
        # scans nothing. That is #4481 one level up, at the list rather than
        # the pattern; require_glob_list closes it by declaring the key
        # required in code instead of inferring it from the JSON.
        del self.budget["active_guidance_globs"]
        self.write_budget()
        self.assertIn(
            ("active_guidance_globs", "empty-glob-list"),
            {(error["path"], error["code"]) for error in validate_repository(self.root, self.budget_path)},
        )

    def test_an_active_guidance_glob_key_set_to_an_empty_list_is_reported_as_empty(self):
        self.budget["active_guidance_globs"] = []
        self.write_budget()
        self.assertIn(
            ("active_guidance_globs", "empty-glob-list"),
            {(error["path"], error["code"]) for error in validate_repository(self.root, self.budget_path)},
        )

    def test_an_active_guidance_glob_key_that_is_renamed_is_reported_as_empty(self):
        # A future budget restructure renaming the key, or a bad merge that
        # drops it, must be caught the same way as deleting it outright.
        self.budget["active_guidance_globs_renamed"] = self.budget.pop("active_guidance_globs")
        self.write_budget()
        self.assertIn(
            ("active_guidance_globs", "empty-glob-list"),
            {(error["path"], error["code"]) for error in validate_repository(self.root, self.budget_path)},
        )

    def test_a_reference_scan_glob_key_that_is_deleted_is_reported_as_empty(self):
        del self.budget["reference_scan_globs"]
        self.write_budget()
        self.assertIn(
            ("reference_scan_globs", "empty-glob-list"),
            {(error["path"], error["code"]) for error in validate_repository(self.root, self.budget_path)},
        )

    def test_a_total_guidance_glob_key_that_is_deleted_is_reported_as_empty(self):
        # total_guidance_globs only runs once reduction_baseline_bytes and
        # minimum_reduction_percent are configured -- see
        # test_total_reduction_is_noop_when_unconfigured -- but once it does,
        # the same required-key guard must apply to its own copy of the list.
        self.budget["reduction_baseline_bytes"] = 1_000_000
        self.budget["minimum_reduction_percent"] = 10
        self.budget.pop("total_guidance_globs", None)
        self.write_budget()
        self.assertIn(
            ("total_guidance_globs", "empty-glob-list"),
            {(error["path"], error["code"]) for error in validate_repository(self.root, self.budget_path)},
        )

    def test_a_host_skill_metadata_glob_matching_nothing_is_reported_as_empty(self):
        # host_skill_metadata_globs is a {host: [patterns]} dict feeding
        # skill_listing_chars via expand_globs directly -- a separate path
        # from active_guidance_globs/reference_scan_globs/total_guidance_globs,
        # so it needs its own wiring through the reporting resolver, per host.
        self.budget["host_skill_metadata_globs"] = {
            "codex": [".agents/skills/*/moved-away/**/*.md"]
        }
        self.budget["max_skill_listing_chars"] = 10_000
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn(
            (".agents/skills/*/moved-away/**/*.md", "empty-glob"),
            {(error["path"], error["code"]) for error in errors},
        )

    def test_rejects_oversized_skill_md_file(self):
        self.budget["skill_budgets"] = {".github/skills": {"max_skill_md_bytes": 10}}
        self.write_budget()
        self.assertIn("skill-md-byte-budget", self.codes())

    def test_accepts_skill_md_file_at_configured_byte_cap(self):
        content = self.root.joinpath(".github/skills/example/SKILL.md").read_text(encoding="utf-8")
        self.budget["skill_budgets"] = {
            ".github/skills": {"max_skill_md_bytes": len(content.encode("utf-8"))}
        }
        self.write_budget()
        self.assertNotIn("skill-md-byte-budget", self.codes())

    def test_rejects_skill_md_file_one_byte_over_lf_normalized_cap(self):
        # The cap counts LF-normalized bytes (matching the existing per-file
        # byte-budget precedent), so a CRLF-heavy working tree checkout must not
        # be penalized for line-ending width alone.
        content = self.root.joinpath(".github/skills/example/SKILL.md").read_text(encoding="utf-8")
        lf_normalized_bytes = len(content.encode("utf-8"))
        self.root.joinpath(".github/skills/example/SKILL.md").write_bytes(
            content.replace("\n", "\r\n").encode("utf-8")
        )
        self.budget["skill_budgets"] = {
            ".github/skills": {"max_skill_md_bytes": lf_normalized_bytes}
        }
        self.write_budget()
        self.assertNotIn("skill-md-byte-budget", self.codes())

    def test_budgets_always_loaded_body_separately_from_skill_listing(self):
        """Body and listing hit different documented host caps, so they are
        counted against different ceilings and reported with different codes."""
        self.budget["host_contexts"] = {"codex": ["AGENTS.md"]}
        self.budget["host_skill_metadata_globs"] = {
            "codex": [".agents/skills/*/SKILL.md"]
        }
        self.budget["max_always_loaded_body_chars"] = 10
        self.budget["max_skill_listing_chars"] = 10_000
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        codes = {error["code"] for error in errors if error["path"] == "codex"}
        self.assertIn("host-body-budget", codes)
        self.assertNotIn("host-listing-budget", codes)

        self.budget["max_always_loaded_body_chars"] = 10_000
        self.budget["max_skill_listing_chars"] = 5
        self.write_budget()
        errors = validate_repository(self.root, self.budget_path)
        codes = {error["code"] for error in errors if error["path"] == "codex"}
        self.assertIn("host-listing-budget", codes)
        self.assertNotIn("host-body-budget", codes)

    def test_always_loaded_body_uses_utf8_bytes(self):
        self.budget["host_contexts"] = {"codex": ["AGENTS.md"]}
        self.budget["max_always_loaded_body_chars"] = 1
        (self.root / "AGENTS.md").write_text("é", encoding="utf-8")
        self.write_budget()
        codes = {error["code"] for error in validate_repository(self.root, self.budget_path)}
        self.assertIn("host-body-budget", codes)

    def configure_routing_bridges(self, **overrides) -> None:
        self.budget["routing_bridges"] = {
            "source": ".agents/skills/act-as-mohab/references/routing.md",
            "skill_roots": [".agents/skills"],
            **overrides,
        }
        self.write_budget()

    def test_rejects_a_routed_skill_that_does_not_exist(self):
        """The table itself is parsed, so a new ghost row fails without anyone
        remembering to register it in the budget."""
        self.write(
            ".agents/skills/act-as-mohab/references/routing.md",
            "# Routing\n\n| Deliverable | Load |\n| --- | --- |\n| Anything | `ghost-skill` |\n",
        )
        self.configure_routing_bridges()
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn("routing-bridge-missing", {error["code"] for error in errors})

    def test_accepts_a_routed_skill_that_exists(self):
        self.write(
            ".agents/skills/real-skill/SKILL.md",
            "---\nname: real-skill\ndescription: Use when the router points here.\n---\n\n# Real\n",
        )
        self.write(
            ".agents/skills/act-as-mohab/references/routing.md",
            "# Routing\n\n| Deliverable | Load |\n| --- | --- |\n| Anything | `real-skill` |\n",
        )
        self.configure_routing_bridges()
        errors = validate_repository(self.root, self.budget_path)
        bridge_errors = [e for e in errors if e["code"].startswith("routing-bridge")]
        self.assertEqual(bridge_errors, [])

    def test_ignores_paths_and_filenames_that_are_not_skill_names(self):
        """`src/main/java` and `AGENTS.md` are backticked in real routing prose
        and must not be mistaken for skills."""
        self.write(
            ".agents/skills/act-as-mohab/references/routing.md",
            "# Routing\n\nEdit `src/main/java` per `AGENTS.md`, verify with `rg`.\n",
        )
        self.configure_routing_bridges()
        errors = validate_repository(self.root, self.budget_path)
        bridge_errors = [e for e in errors if e["code"].startswith("routing-bridge")]
        self.assertEqual(bridge_errors, [])

    def test_rejects_a_required_handoff_the_router_no_longer_mentions(self):
        self.write(
            ".agents/skills/act-as-mohab/references/routing.md",
            "# Routing\n\nNothing is routed here.\n",
        )
        self.configure_routing_bridges(required_names=["consult-first"])
        errors = validate_repository(self.root, self.budget_path)
        self.assertIn("routing-bridge-unrouted", {error["code"] for error in errors})

    def test_rejects_duplicate_long_paragraphs(self):
        paragraph = "This duplicated instruction is deliberately long enough for deterministic detection. " * 2
        self.write("AGENTS.md", f"# Agents\n\n{paragraph}\n")
        self.write("CLAUDE.md", f"# Claude\n\n{paragraph}\n")
        self.assertIn("duplicate-paragraph", self.codes())

    def test_rejects_broken_local_reference(self):
        self.write("AGENTS.md", "# Agents\n\n[Missing](docs/missing.md)\n")
        self.assertIn("broken-reference", self.codes())

    def test_rejects_local_reference_outside_repository(self):
        self.write("AGENTS.md", "# Agents\n\n[Outside](../outside.md)\n")
        self.assertIn("reference-outside-root", self.codes())

    def test_rejects_invalid_skill_frontmatter(self):
        self.write(".github/skills/example/SKILL.md", "---\nname: wrong\n---\n\n# Example\n")
        codes = self.codes()
        self.assertIn("skill-name", codes)
        self.assertIn("skill-description", codes)

    def test_rejects_invalid_codex_skill_frontmatter(self):
        self.write(
            ".agents/skills/example-bridge/SKILL.md",
            "---\nname: wrong\n---\n\n# Bridge\n",
        )
        codes = self.codes()
        self.assertIn("skill-name", codes)
        self.assertIn("skill-description", codes)

    def test_enforces_exact_skill_set(self):
        self.budget["expected_skill_names"] = {
            ".agents/skills": ["example-bridge"],
            ".github/skills": ["example"],
        }
        self.write_budget()
        self.write(
            ".agents/skills/unexpected/SKILL.md",
            "---\nname: unexpected\ndescription: Unexpected workflow.\n---\n\n# Unexpected\n",
        )
        self.assertIn("skill-set", self.codes())

    def test_requires_valid_codex_skill_metadata(self):
        self.budget["skill_budgets"] = {
            ".agents/skills": {
                "max_description_chars": 100,
                "max_body_chars": 100,
                "require_openai_yaml": True,
            }
        }
        self.write_budget()
        self.assertIn("skill-metadata", self.codes())

        self.write(
            ".agents/skills/example-bridge/agents/openai.yaml",
            """interface:
  display_name: "Example Bridge"
  short_description: "Load the canonical example workflow"
  default_prompt: "Use $example-bridge to handle this example task."
policy:
  allow_implicit_invocation: true
""",
        )
        self.assertNotIn("skill-metadata", self.codes())

    def test_rejects_unmatched_path_scope(self):
        self.write(
            ".github/instructions/source.instructions.md",
            '---\napplyTo: "**/src/missing/**/*.java"\n---\n\n# Source\n',
        )
        self.assertIn("unmatched-scope", self.codes())

    def test_rejects_costly_mandate(self):
        self.write("AGENTS.md", "# Agents\n\nRun the build before every commit.\n")
        self.assertIn("forbidden-mandate", self.codes())

    def test_current_repository_configuration_is_valid(self):
        repository_root = Path(__file__).resolve().parents[2]
        self.assertEqual(validate_repository(repository_root), [])

    def test_parse_frontmatter_reads_folded_block_scalar_description_in_full(self):
        # Regression: the parser used to stop at the "description: >-" marker
        # line itself, so every multi-line folded description in
        # .claude/skills/*/SKILL.md was silently counted as its 2-character
        # marker -- undercounting the "claude" host_context token estimate
        # (validate_host_contexts) for every skill using folded style.
        content = (
            "---\n"
            "name: example\n"
            "description: >-\n"
            "  First line of the description.\n"
            "  Second line, still folded onto the first with a space.\n"
            "---\n\nBody.\n"
        )
        frontmatter = parse_frontmatter(content)
        self.assertEqual(
            frontmatter["description"],
            "First line of the description. Second line, still folded onto the first with a space.",
        )

    def test_parse_frontmatter_reads_literal_block_scalar_preserving_newlines(self):
        content = "---\nname: example\ndescription: |-\n  Line one.\n  Line two.\n---\n\nBody.\n"
        frontmatter = parse_frontmatter(content)
        self.assertEqual(frontmatter["description"], "Line one.\nLine two.")


if __name__ == "__main__":
    unittest.main()


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]


class VendorEnumerationIsEnforcedTest(unittest.TestCase):
    """#4531 gap 3: the vendor list is enumerated, so enforce the enumeration.

    `host_skill_metadata_globs` names codex, claude and grok. `host_contexts`
    names those three plus copilot. Nothing required the two to agree, so a
    host could be fully described in one list and absent from the other -- and
    a fourth vendor dropping in an adapter gets a skill-listing budget only if
    someone remembers to add one.

    The owner requirement is *any manufacturer*. Generalising over vendors is
    not achievable here -- each host's guidance lives at paths only that host
    reads, and there is no pattern that discovers them -- so the honest answer
    is the other one #4531 offers: enumerate, but enforce the enumeration, so
    a gap is loud rather than silent. Same shape as gap 1's fix, and the same
    principle as #4542: an unknown must never be indistinguishable from a no.
    """

    def test_every_host_context_has_a_skill_metadata_budget(self):
        budget = json.loads(
            (REPOSITORY_ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        contexts = set(budget.get("host_contexts", {}))
        budgeted = set(budget.get("host_skill_metadata_globs", {}))
        exempt = set(budget.get("host_skill_metadata_exemptions", {}))
        missing = sorted(contexts - budgeted - exempt)
        self.assertEqual(
            missing,
            [],
            "a host with guidance but no skill-listing budget is a vendor the "
            "size checks silently do not cover; add a glob list or an exemption "
            "with a stated reason",
        )

    def test_every_exemption_states_a_reason(self):
        """An exemption without a reason is the gap wearing a hat (#4489)."""
        budget = json.loads(
            (REPOSITORY_ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        for host, reason in budget.get("host_skill_metadata_exemptions", {}).items():
            with self.subTest(host=host):
                self.assertTrue(
                    isinstance(reason, str) and reason.strip(),
                    f"{host} is exempt from a skill-listing budget with no reason",
                )

    def test_no_exemption_names_a_host_that_does_not_exist(self):
        """A dead exemption is a hatch nobody closed."""
        budget = json.loads(
            (REPOSITORY_ROOT / "scripts/ci/agent_guidance_budget.json").read_text(encoding="utf-8")
        )
        contexts = set(budget.get("host_contexts", {}))
        stale = sorted(set(budget.get("host_skill_metadata_exemptions", {})) - contexts)
        self.assertEqual(stale, [])


class OrphanedSiblingClaimTest(unittest.TestCase):
    """#4567 section 4.4: fixing one docstring must not leave its twin claim behind.

    Round two of PR #4554 corrected `_ledger_records_a_review`, whose docstring
    asserted that a bare `review` ledger event still counted. The identical claim
    survived in `_reviewer_dispatch_event` and cost a whole round-three finding to
    re-discover. Both are in `scripts/agents/guard.py`.
    """

    GUARD = "scripts/agents/guard.py"
    # The round-two fix. Its parent is the revision where BOTH docstrings are stale.
    ROUND_TWO_FIX = "64d46cfdac"
    # PR #4554 as merged: both docstrings corrected.
    MERGED = "5b6d4b2b5f"

    @staticmethod
    def _source_at(revision, path):
        """File content at a revision, or None when this clone cannot resolve it."""
        completed = subprocess.run(  # nosec B603 B607
            ["git", "show", f"{revision}:{path}"],
            capture_output=True, text=True, encoding="utf-8", errors="replace",
            cwd=REPOSITORY_ROOT, check=False,
        )
        return completed.stdout if completed.returncode == 0 else None

    def _pair(self, revision):
        before = self._source_at(f"{revision}^", self.GUARD)
        after = self._source_at(revision, self.GUARD)
        if before is None or after is None:
            self.skipTest(f"{revision} unavailable in this clone")
        return before, after

    def test_the_round_two_fix_left_its_twin_claim_in_a_sibling(self):
        """RED fixture: the real commit whose sibling claim cost a review round."""
        before, after = self._pair(self.ROUND_TWO_FIX)
        findings = find_orphaned_sibling_claims(before, after, self.GUARD)
        self.assertEqual({f["code"] for f in findings}, {"orphaned-sibling-claim"})
        report = " ".join(f["message"] for f in findings)
        self.assertIn("_ledger_records_a_review", report)
        self.assertIn("_reviewer_dispatch_event", report)
        self.assertIn("review", report)

    def test_the_merged_branch_is_clean(self):
        """Both docstrings corrected: the check must go quiet, or it is noise."""
        before, after = self._pair(self.MERGED)
        self.assertEqual(find_orphaned_sibling_claims(before, after, self.GUARD), [])

    def test_a_renamed_owner_is_not_a_surviving_sibling(self):
        """`_skip_gh_global_flags` became `_split_gh_global_flags`; that is a rename, not a twin.

        Structural, not semantic: if the function that lost the claim is gone from
        the file, there is no instance left for a sibling to disagree with.
        """
        before = '''
def _skip_gh_global_flags(tokens):
    """Skip a leading `-R`/`--repo <value>` off `tokens`."""
'''
        after = '''
def _split_gh_global_flags(tokens):
    """Split a leading `-R`/`--repo <value>` off `tokens`."""
'''
        self.assertEqual(find_orphaned_sibling_claims(before, after, "x.py"), [])

    def test_an_untouched_docstring_is_not_a_deleted_claim(self):
        """Two functions may legitimately share wording as long as nothing was edited."""
        source = '''
def a():
    """Reads `HOOK_BUDGET_SECONDS` from the environment."""

def b():
    """Also reads `HOOK_BUDGET_SECONDS` from the environment."""
'''
        self.assertEqual(find_orphaned_sibling_claims(source, source, "x.py"), [])

    def test_a_deleted_sentence_with_no_code_token_is_not_a_claim(self):
        """Judged on backticked tokens only -- never on English prose overlap."""
        before = '''
def a():
    """This helper is careful about the order it reads things in."""

def b():
    """It is careful about the order it reads things in."""
'''
        after = '''
def a():
    """Rewritten."""

def b():
    """It is careful about the order it reads things in."""
'''
        self.assertEqual(find_orphaned_sibling_claims(before, after, "x.py"), [])

    def test_cli_reports_the_sibling_claim_without_failing(self):
        """Reachable from the command line, and advisory: docstring prose never blocks.

        #4567's own finding template ranks docstrings as never-blocking, and the
        scan fires twice across PR #4554 with one of the two a judgement call. A
        gate on that is noise; a printed finding is the review round moved upstream.
        """
        script = REPOSITORY_ROOT / "scripts/ci/validate_agent_guidance.py"
        result = subprocess.run(  # nosec B603
            [sys.executable, str(script), "--docstring-siblings",
             self.ROUND_TWO_FIX, self.GUARD],
            capture_output=True, text=True, cwd=REPOSITORY_ROOT, check=False,
        )
        if "unavailable" in result.stderr:
            self.skipTest("revision unavailable in this clone")
        self.assertEqual(result.returncode, 0)
        self.assertIn("_reviewer_dispatch_event", result.stdout)
