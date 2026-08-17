# Handoff: #5017 local Java coding agent

Continue this branch. Do not restart from `main` without reading this file and querying MemPalace first.

## What already shipped to `main`

- Hook fix merged: [PR #5062](https://github.com/ShaftHQ/SHAFT_ENGINE/pull/5062) / `43bb5b995b` / closes #5061.
- Grok no longer resolves `"command": "python3"` as `.claude\python3`. Restart Grok on a checkout that contains that commit or hooks stay broken.

## This branch (snapshot)

- Branch: `ChaosEngine/5017-local-java-coding-agent`
- Head at handoff: `1999f1d349` plus this file
- Tracker: #5017 (do not close from this PR)
- Subtask: #5060 (this PR should `Closes #5060`)

## Machine state (not in git)

| Item | Where |
| --- | --- |
| Ollama 0.32.14 portable | `D:\AI\ollama\ollama.exe` |
| Models | `D:\AI\ollama\models` — `qwen2.5-coder:7b` Q4_K_M (`dae161e27b0e…`) |
| Aider 0.86.2 | `D:\AI\aider\.venv\Scripts\aider.exe` |
| `~/.ollama` backup | `D:\AI\backups\ollama-20260817T205447` |
| Last report | `D:\AI\reports\20260817T181622Z\report.json` |
| Bind | `127.0.0.1:11434` only. `stop.ps1` may already have stopped the last serve |

## Proven acceptance (throwaway worktree, not this PR)

- Disposable branch `ChaosEngine/5017-accept-tmp` (push if still present): planted RED `expected [SHAFT] but found [broken]`, local coder commit `b65bb16e84`, GREEN `Tests run: 1, Failures: 0` in 84s.
- Aider may print `Unsupported git index version 4`. Non-fatal on this host.

## Next session must do, in order

1. Load ChaosEngine. After the plan is read, **query MemPalace once** for this task (used / skipped-with-irrelevance / degraded). Do not start implementation without that attempt.
2. Open or resume the draft PR for this branch. GitHub GraphQL 503 is common; create/update with REST (`gh api repos/.../pulls`).
3. Keep dirty `5024` work on `ChaosEngine/5024-ios-windows-appium-lifecycle` if that snapshot branch exists. Do not mix it into this PR.
4. Remaining #5017 items: tracker comment with receipts; extra subtask issues if 503 blocked them; independent review of this snapshot; merge only this branch when the tracker checkboxes are honest.
5. Dispatch the local coder only from an isolated worktree via `scripts/local-coding-agent/run_agent.ps1`. Read `report.json` and `diff.patch`. Never trust Aider prose. Never push from the wrapper.

## Dispatch

```powershell
pwsh -NoProfile -File scripts/local-coding-agent/install.ps1
pwsh -NoProfile -File scripts/local-coding-agent/run_agent.ps1 `
  -Worktree <isolated-worktree> `
  -Spec <spec.md> `
  -Allowlist @('path/to/one/file.java') `
  -TestCommand 'mvn.cmd -pl shaft-engine -Dtest=YourTest test "-Dallure.automaticallyOpen=false"'
pwsh -NoProfile -File scripts/local-coding-agent/stop.ps1
```

## Checks already run

- `py -3 -m unittest tests.scripts.test_local_coding_agent` — 15 OK
- Smoke generate returned `ok`; GPU showed `D:\AI\ollama\lib\ollama\llama-server.exe`
- Primary `5024` checkout was not used as the Maven cwd
