# OmniRoute proof of dispatch

When the adopter or process-owner **opts into OmniRoute** for a delivery,
ChaosEngine must prove a coding dispatch happened. Catalog, candidates, health,
and CredentialHealth probes are not progress.

## Required proof (one is enough)

1. **Runner `omniroute run` receipt** — a ChaosEngine OmniRoute runner terminal
   receipt with `outcome=success` / completed status, or an explicit
   `omnirouteRun` / `dispatchProved` flag on a private receipt object.
2. **Coding completion call_log** — a row from `omniroute --output json usage
   logs` (or `/api/usage/call-logs`) whose `model` is **not** a probe marker
   such as `connection-test` / CredentialHealth, on a successful POST.

## Never counts as proof

- `curl …/api/health`
- `runner.py probe` / `candidates`
- `omniroute --output json models` / `usage quota`
- CredentialHealth / `connection-test` call_logs created at `omniroute serve`
  start or provider refresh
- Native host / `gh` / shell work while READY without launching `omniroute run`

## Harness check

```text
python3 .chaos-engine/skills/omniroute/scripts/runner.py proof --required \
  --receipt /path/to/receipt.json
python3 .chaos-engine/skills/omniroute/scripts/runner.py proof --required \
  --call-logs /path/to/call-logs.json
python3 .chaos-engine/skills/omniroute/scripts/runner.py proof --required \
  --steps probe,candidates
```

Exit `0` when proved or when OmniRoute was not required. Exit `2` with
`state=BLOCKED` and a `blocker` string when required proof is missing.

Learning Session and delivery receipts must record that blocker; do not claim
OmniRoute success from probes alone.

## Host locality

Proof must come from the machine where `omniroute serve` binds
`127.0.0.1:20128`. A remote agent box cannot see that loopback. See
[living-lessons.md](living-lessons.md).
