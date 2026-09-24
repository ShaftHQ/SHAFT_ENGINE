# OmniRoute docs study notes (ROG local gateway)

Studied on **2026-09-16** against the operator gateway on this ROG host:

- Local docs SPA: `http://127.0.0.1:20128/docs` (HTTP 200 while `omniroute serve`
  is up; content is a Next.js client app — use OpenAPI + CLI + package guides
  for durable text).
- Dashboard home: `http://127.0.0.1:20128/home`
- OpenAPI: `http://127.0.0.1:20128/openapi.yaml` (title OmniRoute API; local
  base `http://localhost:20128`)
- Installed CLI on the studied host: `omniroute` **3.8.50**
- Anonymous `/api/health` returns `{status,timestamp}` only — not coding proof
- `/api/usage/call-logs` exists; CLI: `omniroute --output json usage logs`

Companion upstream text aligned with the installed release:

- [CLI Integrations](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.50/docs/guides/CLI-INTEGRATIONS.md)
- [Auto-Combo](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.50/docs/routing/AUTO-COMBO.md)
- [Remote Mode](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.51/docs/guides/REMOTE-MODE.md)
- [Quick Start](https://github.com/diegosouzapw/OmniRoute/blob/release/v3.8.50/docs/getting-started/QUICK-START.md)

## Distilled operator practices

| Topic | Practice for ChaosEngine |
| --- | --- |
| Install / serve | Operator installs OmniRoute; CE never does. Loopback only: `OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open`. |
| Dashboard vs coding | `/home` idle while CredentialHealth/`connection-test` call_logs appear is normal. Coding traffic needs `omniroute run` (or equivalent completion). |
| Run targets | Prefer `omniroute run` over `setup-*` (no config writes). Targets: `claude`, `codex`, `opencode`, `aider`, `goose`, `qwen`, `gemini`. |
| Auto-combo | `auto/coding` and `auto/coding:fast` are category routes when advertised; smoke before long implementer. Fall back to ranked live native ids. |
| Auth / quotas | 429 / exhausted accounts → skip identity, requery, next candidate. Do not claim success. |
| Chaos Mode | Do not dispatch through OmniRoute Chaos Mode (`auto/chaos` / `/dashboard/chaos`). |
| Remote / loopback | CE forbids remote `--base-url` inventiveness. Agent boxes must Shell on the user machine (Local Execution / machineId) where serve is bound. Upstream Remote Mode is operator-owned, not a harness workaround. |
| Missing serve | Guide the user to install/serve from guides/omniroute.md (repo-only `chaos-engine/guides/omniroute.md`). Do not install credentials for the operator. |

## Live observation (same day)

On the studied ROG, `usage logs` were dominated by `model=connection-test`
rows with `tokens=0` across providers — CredentialHealth-style probes, not
coding sessions. That matches issue #5863/#5864: probes must not count as
OmniRoute delivery progress.
