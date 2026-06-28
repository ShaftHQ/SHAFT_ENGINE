---
name: public-behavior-docs-synchronizer
description: Use when planning or verifying user-guide updates for public SHAFT behavior, docs links, release text, or API/config changes.
---

# Public Behavior Docs Synchronizer

Use for user-visible SHAFT behavior changes. Keep docs work separate from code unless the task explicitly asks for both.

## Workflow

1. Identify the public surface: API, property, module, report output, CLI/MCP tool, README link, or release text.
2. Search `C:\Users\Mohab\IdeaProjects\shafthq.github.io` with targeted `rg`; update only guide pages affected by the behavior change.
3. Keep canonical links in `README.md`, `.github/RELEASE_BODY_TEMPLATE.md`, and `legacy-shaft-engine/pom.xml`. Run `python3 scripts/ci/validate_modular_documentation.py` when those links or examples are touched.
4. Run `python3 scripts/ci/validate_documentation_boundaries.py`; for guidance changes also run `python3 scripts/ci/validate_agent_setup.py`.
5. Report the docs PR/link, or the concrete reason no public docs change is needed.

## Output

List the public surface, guide files searched or changed, validation results, and docs PR/blocker.
