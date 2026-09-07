# Silent-on-success / errors-only verify

HumanLayer harness lesson: green verification dumps rotate agents into the
dumb zone. Success must add **zero** context; failure must add **one**
actionable line.

## Contract

| Outcome | Stdout | Stderr | Exit |
| --- | --- | --- | --- |
| Success | empty | empty | `0` |
| Failure | empty | one actionable line | `2` (preferred) |

Companion pattern: doctor `--fix-next-only` (#5582).

## Portable helper

```text
python3 .chaos-engine/silent_verify.py session-start-budget
python3 .chaos-engine/silent_verify.py learning-session-finalize --session-id <id>
python3 .chaos-engine/silent_verify.py eval-parity
python3 .chaos-engine/silent_verify.py run -- python3 -m unittest tests.scripts.test_example -q
python3 .chaos-engine/learning_session.py finalize --session-id <id> --silent
```

Use on Check / Stop hot paths. Do not print essays on green.

## Boundaries

- Never weaken a failing gate to stay silent.
- Metrics/counters may update on success, but must not print.
- Prefer CLI wrappers over MCP for the same verification job (CLI-over-MCP iron law).
