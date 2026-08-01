# Guide-search playbook

## Ten practices

1. Search the live official guide before writing unfamiliar or version-sensitive SHAFT syntax, configuration, migration, or troubleshooting advice. [`SHAFT-GUIDE`]
2. Use exact `shaft_guide_search` through direct MCP when already connected, or `shaft-cli guide search` for a stateless scriptable lookup. [`SHAFT-MCP`]
3. Phrase one narrow query with the surface, action, and constraint, such as `Playwright browser title assertion`; avoid broad product questions. [`SHAFT-GUIDE`]
4. Request only a few high-value matches first, then refine with returned terminology instead of loading the whole guide. [`SHAFT-MCP`]
5. Read the title, section, excerpt, code blocks, guidance rules, warnings, and source URL together; do not treat one snippet as the full contract. [`SHAFT-GUIDE`]
6. Cite the returned official URL beside the claim or code it supports, preserving enough section context for review. [`SHAFT-GUIDE`]
7. Cross-check live guide syntax against the user's installed SHAFT version and current repository exemplars before changing code. [`SHAFT-GUIDE`, `ISTQB-CTFL`]
8. If results are empty or conflicting, reformulate once with official class or method names; report the gap rather than inventing an API. [`SHAFT-GUIDE`]
9. Prefer the current official guide over blogs, generated chat answers, stale examples, or memory; use external sources only for the non-SHAFT discipline they own. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
10. Route the grounded result to the correct specialist and verify compiled behavior; search evidence alone does not prove the user's flow. [`ISTQB-CTFL`]

## Valid examples

- Search `SHAFT Locator Builder ARIA role` before creating a generated web locator, then cite the returned locator-builder page.
- Search `SHAFT.API target status code response assertion` before implementing a service check.
- Search `mobile touch swipe element into view` before composing a mobile gesture chain.
- Search `browser verifyThat versus assertThat` before choosing soft or hard validation semantics.

## Boundary

- Return authoritative guide evidence and current syntax, not an unverified implementation; route code, actions, locators, or assertions to their focused specialist.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
