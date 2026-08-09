# Agent Guidance Boundary Guard

Use for agent guidance and retrieval/config surfaces. Keep one substantive entrypoint; make every host surface a thin relative adapter.

## Workflow

1. Put cross-provider behavior in `.agents/skills/act-as-mohab/`; link new task playbooks from its routing reference instead of copying them.
2. Keep `AGENTS.md`, `CLAUDE.md`, `.claude/skills/**`, and `.claude/agents/**` lean. They may point to canonical files with relative paths but must not restate policy.
3. Keep one hook implementation under `scripts/agents/`; provider hook files only register its relative command.
4. Preserve restricted native Memory access, deterministic MemPalace retrieval, and Graphify routing. Do not save routine diaries.
5. Update guidance budgets/manifests and add a failing portability test before changing behavior.
6. Assemble portable packages with `python3 scripts/ci/assemble_act_as_mohab_plugin.py <empty-output-directory>` or `python3 scripts/ci/assemble_shaft_skills_plugin.py <empty-output-directory>` and validate them with `scripts/ci/validate_agent_plugins.py`; cover assembly in `tests/scripts/test_assemble_act_as_mohab_plugin.py` and `tests/scripts/test_assemble_shaft_skills_plugin.py`, and release assets in `tests/scripts/test_agent_plugin_release.py` before release. For `shaft-skills`, refresh and validate the generated inventory, reviewed scorecard, and context budget with `python3 scripts/ci/shaft_skill_quality.py --write`, then run `tests/scripts/test_shaft_skill_quality.py`; refresh the canonical routing corpus adapters with `python3 scripts/ci/shaft_skill_routing_eval.py --write` and cover them in `tests/scripts/test_shaft_skill_routing_eval.py`. Validate every online candidate through `python3 scripts/ci/shaft_skill_candidate_intake.py` and `tests/scripts/test_shaft_skill_candidate_intake.py`; never install candidate code into canonical roots or bypass its no-network container gate. Run unauthenticated native-client evidence with `python3 scripts/ci/agent_plugin_client_smoke.py --mode smoke` and cover it in `tests/scripts/test_agent_plugin_client_smoke.py`; scheduled live acceptance in `.github/workflows/agent-plugin-acceptance.yml` records missing model credentials as external blockers.
7. Run `python3 scripts/ci/validate_agent_setup.py --skip-external`; run the full command when network/npm access is stable.

## Output

List canonical guidance, adapter, hook, retrieval, and validation changes plus intentional budget headroom.
