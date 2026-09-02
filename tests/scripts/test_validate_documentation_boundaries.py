import tempfile
import unittest
from pathlib import Path

from scripts.ci.readme_contract import USER_GUIDE_ROOT
from scripts.ci.validate_documentation_boundaries import validate_repository


class ValidateDocumentationBoundariesTest(unittest.TestCase):
    def setUp(self):
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary_directory.name)
        self.valid_readme = f'''<picture>
  <img src="shaft-engine/src/main/resources/images/shaft_standard.png" alt="SHAFT S logo" width="260">
</picture>

[![Build](https://img.shields.io/github/actions/workflow/status/ShaftHQ/SHAFT_ENGINE/pr-gate.yml?branch=main&style=for-the-badge)](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/pr-gate.yml)
[User guide]({USER_GUIDE_ROOT})
[Generate your first project]({USER_GUIDE_ROOT})
[Sponsor SHAFT](https://github.com/sponsors/MohabMohie)

Canonical Maven coordinate: **io.github.shafthq:shaft-engine**

Test intent and configuration enter SHAFT orchestration, fan out across execution surfaces, and return as unified evidence.

```mermaid
flowchart LR
    accTitle: SHAFT execution and evidence workflow
    accDescr: Test intent and configuration enter SHAFT orchestration, run across execution surfaces, and produce unified evidence.
    I[Test intent] --> S[SHAFT orchestration]
    C[Configuration] --> S
    S --> W[Web]
    S --> M[Mobile]
    S --> A[API]
    S --> N[Native, CLI, and Database]
    W --> E[Unified evidence]
    M --> E
    A --> E
    N --> E
```
'''
        self.write("README.md", self.valid_readme)
        self.write(
            "modular-era-feature-catalog.md",
            "[Release history](https://github.com/ShaftHQ/SHAFT_ENGINE/releases)",
        )

    def tearDown(self):
        self.temporary_directory.cleanup()

    def write(self, relative_path, content):
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def test_allows_workflow_readme(self):
        self.write(".github/workflows/README.md", "# GitHub Actions Workflows\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_repository_map_readme(self):
        self.write("tools/repository-map/README.md", "# Repository Map\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_internal_tool_markdown(self):
        self.write("tools/repository-map/details/usage.md", "# Usage\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_only_required_chaos_gauge_task_instructions(self):
        self.write(
            "scripts/ci/chaos_gauge/dataset/example/instruction.md",
            "Repair the seeded workspace.\n",
        )

        self.assertEqual(validate_repository(self.root), [])

        self.write("scripts/ci/chaos_gauge/notes.md", "# Public guide\n")
        self.assertIn(
            "public or unapproved Markdown remains: scripts/ci/chaos_gauge/notes.md",
            validate_repository(self.root),
        )

    def test_allows_codex_internal_markdown(self):
        self.write(".codex/tools/graphify.md", "# Graphify\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_canonical_project_skill(self):
        self.write(".agents/skills/act-as-mohab/SKILL.md", "# Act as Mohab\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_portable_chaos_engine_guidance_and_profile_catalog(self):
        self.write("chaos-engine/skills/chaos-engine/SKILL.md", "# ChaosEngine\n")
        self.write("chaos-engine/profiles/README.md", "# Project profiles\n")
        self.write("chaos-engine/RESEARCH.md", "# Adoption matrix\n")
        self.write(
            "chaos-engine/decision-quality-baseline.md",
            "# Decision-quality baseline\n",
        )
        self.write(
            "chaos-engine/decision-quality-rubric.md",
            "# Decision-quality rubric\n",
        )
        self.write(
            "chaos-engine/decision-quality-calibration.md",
            "# Decision-quality calibration\n",
        )
        self.write("chaos-engine/STANDALONE.md", "# Spec\n")
        self.write("chaos-engine/INSTALL.md", "# Install\n")
        self.write("chaos-engine/THIRD_PARTY_NOTICES.md", "# Notices\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_portable_chaos_engine_readme(self):
        self.write("chaos-engine/README.md", "# ChaosEngine\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_installed_chaos_engine_guidance_and_profile_catalog(self):
        self.write(".chaos-engine/skills/chaos-engine/SKILL.md", "# ChaosEngine\n")
        self.write(".chaos-engine/profiles/README.md", "# Project profiles\n")
        self.write(".chaos-engine/THIRD_PARTY_NOTICES.md", "# Notices\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_installed_host_adapter_markdown(self):
        self.write(".claude/CLAUDE.md", "# Claude adapter\n")
        self.write("GEMINI.md", "# Gemini adapter\n")
        self.write(".gemini/skills/chaos-engine/SKILL.md", "# ChaosEngine\n")
        self.write("plugins/chaos-engine/skills/chaos-engine/SKILL.md", "# ChaosEngine\n")
        self.write("plugins/caveman/UPSTREAM.md", "# Upstream\n")
        self.write("plugins/ponytail/UPSTREAM.md", "# Upstream\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_allows_installable_shaft_skills(self):
        self.write("shaft-skills/evaluation-prompts.md", "# Evaluation Prompts\n")
        self.write("shaft-skills/writing-shaft-tests/SKILL.md", "# Writing SHAFT Tests\n")

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_markdown_inside_claude_worktrees(self):
        self.write(
            ".claude/worktrees/agent-a/docs/anything.md",
            "# Concurrent agent worktree content\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_markdown_inside_memory_recovery(self):
        self.write(
            ".memory/recovery/2026-01-01T00-00-00/memory/gotchas/example.md",
            "# Memory recovery gotcha\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_exact_runtime_directory_at_every_traversal_depth(self):
        self.write(
            ".chaos-engine-runtime/docs/generated.md",
            "# Generated runtime documentation\n",
        )
        self.write(
            "nested/.chaos-engine-runtime/cache/generated.md",
            "# Nested generated runtime documentation\n",
        )

        self.assertEqual(validate_repository(self.root), [])

        self.write(
            ".chaos-engine-runtime-copy/docs/not-generated.md",
            "# Unapproved documentation\n",
        )
        self.assertIn(
            "public or unapproved Markdown remains: .chaos-engine-runtime-copy/docs/not-generated.md",
            validate_repository(self.root),
        )

    def test_ignores_installer_runtime_generation_and_transaction_directories(self):
        self.write(
            ".chaos-engine-runtime-generations/current/node/README.md",
            "# Generated dependency documentation\n",
        )
        self.write(
            ".chaos-engine-runtime-transactions/current/package/README.md",
            "# Transaction dependency documentation\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_rejects_unapproved_nested_readme(self):
        self.write(".github/other/README.md", "# Other\n")

        self.assertIn(
            "non-root README is prohibited: .github/other/README.md",
            validate_repository(self.root),
        )

    def test_rejects_unapproved_nested_readme_inside_allowed_markdown_tree(self):
        self.write("chaos-engine/arbitrary/README.md", "# Arbitrary\n")

        self.assertIn(
            "non-root README is prohibited: chaos-engine/arbitrary/README.md",
            validate_repository(self.root),
        )

    def test_requires_user_guide_landing_page_and_sponsor_cta(self):
        self.write("README.md", "# SHAFT\n")

        errors = validate_repository(self.root)

        self.assertIn(
            f"README.md is missing the user-guide landing page: {USER_GUIDE_ROOT}",
            errors,
        )
        self.assertIn("README.md is missing the GitHub Sponsors call to action", errors)

    def test_rejects_user_guide_subpath_even_when_root_link_exists(self):
        self.write(
            "README.md",
            "\n".join(
                (
                    "[User guide](https://shafthq.github.io/)",
                    "[Deep guide link](https://shafthq.github.io/docs/start/overview)",
                    "[Sponsor SHAFT](https://github.com/sponsors/MohabMohie)",
                )
            ),
        )

        self.assertIn(
            "README.md user-guide links must target https://shafthq.github.io/ exactly",
            validate_repository(self.root),
        )

    def test_rejects_user_guide_subpath_as_linked_badge_destination(self):
        self.write(
            "README.md",
            self.valid_readme
            + "\n[![Guide](https://img.shields.io/badge/guide-live-blue)]"
            "(https://shafthq.github.io/docs/start/overview)\n",
        )

        self.assertIn(
            "README.md user-guide links must target https://shafthq.github.io/ exactly",
            validate_repository(self.root),
        )

    def test_rejects_user_guide_subpath_in_html_destination(self):
        self.write(
            "README.md",
            self.valid_readme
            + '\n<a href="https://shafthq.github.io/docs/start/overview">Guide</a>\n',
        )

        self.assertIn(
            "README.md user-guide links must target https://shafthq.github.io/ exactly",
            validate_repository(self.root),
        )

    def test_rejects_user_guide_subpath_in_html_image_source(self):
        self.write(
            "README.md",
            self.valid_readme
            + '\n<img src="https://shafthq.github.io/docs/preview.png" alt="Preview">\n',
        )

        self.assertIn(
            "README.md user-guide links must target https://shafthq.github.io/ exactly",
            validate_repository(self.root),
        )

    def test_rejects_user_guide_subpath_in_unquoted_html_attributes(self):
        for element in (
            '<a href=https://shafthq.github.io/docs/example>Guide</a>',
            '<img src=https://shafthq.github.io/docs/example.png alt="Example">',
        ):
            with self.subTest(element=element):
                self.write("README.md", self.valid_readme + f"\n{element}\n")

                self.assertIn(
                    "README.md user-guide links must target https://shafthq.github.io/ exactly",
                    validate_repository(self.root),
                )

    def test_rejects_user_guide_subpath_in_reference_definition(self):
        self.write(
            "README.md",
            self.valid_readme
            + "\n[Deep guide][deep-guide]\n"
            "[deep-guide]: https://shafthq.github.io/docs/start/overview\n",
        )

        self.assertIn(
            "README.md user-guide links must target https://shafthq.github.io/ exactly",
            validate_repository(self.root),
        )

    def test_ignores_unused_user_guide_reference_definition(self):
        self.write(
            "README.md",
            self.valid_readme
            + "\n[unused-guide]: https://shafthq.github.io/docs/example\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_rejects_user_guide_subpath_in_autolink(self):
        self.write(
            "README.md",
            self.valid_readme + "\n<https://shafthq.github.io/docs/start/overview>\n",
        )

        self.assertIn(
            "README.md user-guide links must target https://shafthq.github.io/ exactly",
            validate_repository(self.root),
        )

    def test_ignores_destinations_inside_fenced_code(self):
        self.write(
            "README.md",
            self.valid_readme
            + "\n```markdown\n[Example](https://shafthq.github.io/docs/example)\n```\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_fenced_destinations_when_closer_is_longer(self):
        for fenced_example in (
            "```markdown\n[Example](https://shafthq.github.io/docs/example)\n````",
            "~~~markdown\n[Example](https://shafthq.github.io/docs/example)\n~~~~",
        ):
            with self.subTest(fence=fenced_example.splitlines()[0]):
                self.write("README.md", self.valid_readme + f"\n{fenced_example}\n")

                self.assertEqual(validate_repository(self.root), [])

    def test_ignores_destinations_inside_inline_code(self):
        self.write(
            "README.md",
            self.valid_readme
            + "\nExample only: `[Guide](https://shafthq.github.io/docs/example)`\n",
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_ignores_destinations_inside_html_comments(self):
        self.write(
            "README.md",
            self.valid_readme
            + '\n<!-- <a href="https://shafthq.github.io/docs/example">old</a> -->\n',
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_commented_readme_cannot_satisfy_visible_contract(self):
        self.write("README.md", f"<!--\n{self.valid_readme}\n-->\n")

        errors = validate_repository(self.root)

        self.assertIn("README.md must display the SHAFT S logo prominently", errors)
        self.assertIn("README.md is missing the generator-first journey", errors)
        self.assertIn("README.md is missing the canonical Maven coordinate", errors)
        self.assertIn("README.md is missing the pr-gate.yml build badge", errors)

    def test_commented_mermaid_edges_do_not_satisfy_workflow(self):
        readme = self.valid_readme
        for edge in (
            "I[Test intent] --> S[SHAFT orchestration]",
            "C[Configuration] --> S",
            "S --> W[Web, Mobile, API, and Native]",
            "W --> E[Unified evidence]",
        ):
            readme = readme.replace(edge, f"%% {edge}")
        self.write("README.md", readme)

        self.assertIn(
            "README.md is missing the accessible Mermaid evidence workflow",
            validate_repository(self.root),
        )

    def test_mermaid_directives_outside_block_do_not_satisfy_workflow(self):
        self.write(
            "README.md",
            self.valid_readme.replace("    accDescr:", "    description:")
            + "\naccDescr: unrelated prose\n",
        )

        self.assertIn(
            "README.md is missing the accessible Mermaid evidence workflow",
            validate_repository(self.root),
        )

    def test_requires_prominent_s_logo(self):
        self.write("README.md", self.valid_readme.replace('width="260"', 'width="120"'))

        self.assertIn(
            "README.md must display the SHAFT S logo prominently",
            validate_repository(self.root),
        )

    def test_requires_generator_first_journey(self):
        self.write(
            "README.md",
            self.valid_readme.replace("Generate your first project", "Browse features"),
        )

        self.assertIn(
            "README.md is missing the generator-first journey",
            validate_repository(self.root),
        )

    def test_requires_accessible_mermaid_evidence_workflow(self):
        self.write(
            "README.md",
            self.valid_readme.replace("accDescr:", "description:"),
        )

        self.assertIn(
            "README.md is missing the accessible Mermaid evidence workflow",
            validate_repository(self.root),
        )

    def test_requires_canonical_maven_coordinate(self):
        self.write(
            "README.md",
            self.valid_readme.replace(
                "io.github.shafthq:shaft-engine", "io.github.shafthq:SHAFT_ENGINE"
            ),
        )

        self.assertIn(
            "README.md is missing the canonical Maven coordinate",
            validate_repository(self.root),
        )

    def test_inline_code_coordinate_counts_as_visible_content(self):
        self.write(
            "README.md",
            self.valid_readme.replace(
                "**io.github.shafthq:shaft-engine**",
                "`io.github.shafthq:shaft-engine`",
            ),
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_s_logo_attributes_are_order_independent(self):
        self.write(
            "README.md",
            self.valid_readme.replace(
                '<img src="shaft-engine/src/main/resources/images/shaft_standard.png" alt="SHAFT S logo" width="260">',
                '<img width="260" alt="SHAFT S logo" src="shaft-engine/src/main/resources/images/shaft_standard.png">',
            ),
        )

        self.assertEqual(validate_repository(self.root), [])

    def test_requires_pr_gate_build_badge(self):
        self.write(
            "README.md",
            self.valid_readme.replace("pr-gate.yml", "build.yml"),
        )

        self.assertIn(
            "README.md is missing the pr-gate.yml build badge",
            validate_repository(self.root),
        )

    def test_build_workflow_url_outside_badge_does_not_satisfy_badge_contract(self):
        correct_workflow = (
            "https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/pr-gate.yml"
        )
        self.write(
            "README.md",
            self.valid_readme.replace(
                f"]({correct_workflow})",
                "](https://github.com/ShaftHQ/SHAFT_ENGINE/actions/workflows/build.yml)",
                1,
            )
            + f"\n[Workflow]({correct_workflow})\n",
        )

        self.assertIn(
            "README.md is missing the pr-gate.yml build badge",
            validate_repository(self.root),
        )

    def test_rejects_bare_guide_and_sponsor_urls_that_are_not_links(self):
        self.write(
            "README.md",
            f"plain text: {USER_GUIDE_ROOT}\n"
            "plain text: https://github.com/sponsors/MohabMohie",
        )

        errors = validate_repository(self.root)

        self.assertIn(
            f"README.md is missing the user-guide landing page: {USER_GUIDE_ROOT}",
            errors,
        )
        self.assertIn("README.md is missing the GitHub Sponsors call to action", errors)

    def test_requires_catalog_release_history_link(self):
        self.write(
            "modular-era-feature-catalog.md",
            "plain text: https://github.com/ShaftHQ/SHAFT_ENGINE/releases",
        )

        self.assertIn(
            "modular-era-feature-catalog.md is missing the canonical release-history link",
            validate_repository(self.root),
        )


if __name__ == "__main__":
    unittest.main()
