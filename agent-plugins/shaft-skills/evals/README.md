# SHAFT routing evaluations

`cases.json` is the reviewed source for all 30 routing cases. Each prompt expects one
specialist and rejects every sibling recorded in `shaft-skills/quality-review.json`, so the same
case proves positive routing and the directed negative boundary without naming the
answer in the prompt.

Refresh and validate the client-native files with:

```text
python scripts/ci/shaft_skill_routing_eval.py --write
python scripts/ci/shaft_skill_routing_eval.py
python -m unittest tests.scripts.test_shaft_skill_routing_eval -v
```

`claude/evals.json` follows the official skill-creator `evals.json` fields. Codex uses
`codex/cases.jsonl` with `codex/output-schema.json`. The scheduled acceptance workflow
passes the canonical corpus to `agent_plugin_client_smoke.py`; every case then runs in a
fresh native client invocation against the installed package and records the observed
client version, selected specialist, verdict, and context-budget warnings.

The thresholds are deliberately strict: every case and every positive skill must pass.
Missing or disabled model credentials produce per-case `external_blocker` results and do
not masquerade as routing evidence.
All native calls share one 900-second wall-clock execution budget. Routing stops after 600
seconds, active setup stops with 120 seconds reserved for cleanup, and cleanup stops with 60
seconds reserved for artifact writing. Deadline exhaustion preserves completed selections and
blocks only the remaining cases. A nonzero client exit without a recognized authentication,
provider, or network cause is recorded separately as a client failure and does not become a
routing miss.
