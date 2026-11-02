# Local agency: brief and preflight

Loaded on demand from the [local-agency skill](../SKILL.md).

## CE brief (design turns)

For design/spec-shaped local turns, build a locator-only system brief with [`ce_brief.py`](../../../ce_brief.py) or via dispatch:

```text
python3 .chaos-engine/skills/local-agency/scripts/dispatch.py brief --json
python3 .chaos-engine/skills/local-agency/scripts/dispatch.py --prefer llamacpp config --with-ce-brief
python3 .chaos-engine/skills/local-agency/scripts/dispatch.py --prefer llamacpp chat --prompt '…' --with-ce-brief
```

Do not dump full SKILL bodies into the model context.
Unit/eval contracts: [`ce-brief-unit-fixtures.json`](../../../evals/ce-brief-unit-fixtures.json).

## CE project pointers (OpenCode preflight)

Before emitting `config` / `argv` OpenCode material, `dispatch.py` fail-closes unless the
target worktree has ChaosEngine project pointers:

- `AGENTS.md`
- `.agents/skills/chaos-engine/` (install-generated skill adapter)

`--pure` only disables external plugins; it does **not** load ChaosEngine and does **not**
replace those pointers. Missing pointers → `state=UNHEALTHY` with `ce_pointers.missing`.
