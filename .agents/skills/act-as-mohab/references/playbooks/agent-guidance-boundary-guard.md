# Agent Guidance Boundary Guard

Use for agent guidance and retrieval/config surfaces. Keep one substantive entrypoint; make every host surface a thin relative adapter.

## Workflow

1. Put cross-provider behavior in `.agents/skills/act-as-mohab/`; link new task playbooks from its routing reference instead of copying them.
2. Keep `AGENTS.md`, `CLAUDE.md`, `.claude/skills/**`, and `.claude/agents/**` lean. They may point to canonical files with relative paths but must not restate policy.
3. Keep one hook implementation under `scripts/agents/`; provider hook files only register its relative command.
4. Preserve restricted native Memory access, deterministic MemPalace retrieval, and Graphify routing. Do not save routine diaries.
5. Update guidance budgets/manifests and add a failing portability test before changing behavior.
6. Assemble portable packages with `python3 scripts/ci/assemble_act_as_mohab_plugin.py <empty-output-directory>` or `python3 scripts/ci/assemble_shaft_skills_plugin.py <empty-output-directory>` and validate them with `scripts/ci/validate_agent_plugins.py`; cover assembly in its focused tests and release assets in `tests/scripts/test_agent_plugin_release.py` before release.
7. Run `python3 scripts/ci/validate_agent_setup.py --skip-external`; run the full command when network/npm access is stable.

## Output

List canonical guidance, adapter, hook, retrieval, and validation changes plus intentional budget headroom.
