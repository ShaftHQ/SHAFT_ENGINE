MERGED 2026-07-27 memory-hygiene cleanup: earliest/shortest statement of the same gotcha as gotcha.adding-a-shaft-mcp-tool-requires-manifest-skills-catalog-count-sync. Its evidence (PRs #3454/#3459/#3460) was folded into the canonical entry; read that one instead. Original text preserved below.

---

Adding any @Tool to shaft-mcp breaks the installer-verification drift gate on the next CI run unless you regenerate shaft-skills/references/shaft-mcp-tools.md via: py -3 scripts/mcp/generate_shaft_skills_tool_catalog.py (verify with --check) AND update the hardcoded magnitude assertion in tests/scripts/test_generate_shaft_skills_tool_catalog.py (test_real_sources_parse_to_expected_magnitude). Missed twice in PRs #3454/#3459, hotfixed in #3460.