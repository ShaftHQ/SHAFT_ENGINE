# Public Behavior Docs Synchronizer

Use for user-visible SHAFT behavior changes. Keep docs work separate from code unless the task explicitly asks for both.

Add or refresh the matching 3–4 line catalog entry at `/docs/features/whats-new` in the companion documentation PR.

Every user-facing SHAFT behavior change opens a companion PR on
`ShaftHQ/shafthq.github.io` `master` in the same delivery: discover the docs
root, or use an explicitly configured root; never a fixed sibling path. Do
not add public guide pages to SHAFT_ENGINE. That companion PR must include a
description of the change, screenshots where a human sees UI, human-facing instructions,
and AI-supported details (locator policy: unique author-written id via the SHAFT
locator builder, then ARIA role, then native relative xpath only; replay-proven
snippets; properties; exact commands).

## Required companion PR content

1. A description of the change.
2. Screenshots for visible UI. Reuse existing `static/` or test screenshots
   when still accurate. If a screenshot cannot be produced headless, ship
   the prose and open one standalone follow-up issue per missing shot. Do
   not launch a GUI browser without asking.
3. Human-facing steps.
4. AI-supported details: locator policy, replay-proven snippets, properties,
   and exact commands.

## Workflow

1. Identify the public surface: API, property, module, report output, CLI/MCP tool, README link, or release text.
2. Discover the local `shafthq.github.io` checkout, or use an explicitly configured root; never a fixed sibling path. Search it with targeted `rg`; update only guide pages affected by the behavior change.
3. Keep canonical links in `README.md`, `.github/RELEASE_BODY_TEMPLATE.md`, and `legacy-shaft-engine/pom.xml`. Run `python3 scripts/ci/validate_modular_documentation.py` when those links or examples are touched.
4. Run `python3 scripts/ci/validate_documentation_boundaries.py`; for guidance changes also run `python3 scripts/ci/validate_agent_setup.py`.
5. Open the companion PR on `master` with the required content above. Report the docs PR/link, or the concrete reason no public docs change is needed.

## Output

List the public surface, guide files searched or changed, validation results, and docs PR/blocker.
