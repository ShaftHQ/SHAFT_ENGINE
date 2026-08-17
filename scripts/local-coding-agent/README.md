# Workstation local Java coding agent

This is the maintainer-workstation loop for [#5017](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5017). It is **not** SHAFT managed local AI (`tools/local-ai-poc`, [#4851](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/4851)). That product path stays pinned llama.cpp behind `AiExecutionService`.

No model, cache, binary, or report belongs in git.

## Roles

| Role | Who | What they may do |
| --- | --- | --- |
| Orchestrator / planner / reviewer | The Grok / ChaosEngine main thread | Write the spec, choose the file allowlist, read `report.json`, review the actual diff, decide whether the change is acceptable |
| Coder / tester | Aider `0.86.2` + `qwen2.5-coder:7b` | Edit only allowlisted files, run the focused test the spec names, create a **local** commit |
| Stop | `stop.ps1` | Unload only the launcher-owned Ollama process |

The 7B coder is the machine-fit choice on an RTX 3060 Laptop (6 GiB). A 14B Q4 model does not fit that GPU and will hybrid-offload into RAM that IntelliJ and Maven already need.

## What to install

Defaults live under `D:\AI`. Override with `SHAFT_LOCAL_AI_ROOT`.

Or run the installer from this repo (backs up `~/.ollama`, verifies the zip digest, installs Aider):

```powershell
pwsh -NoProfile -File scripts/local-coding-agent/install.ps1
```

Manual equivalent:

1. Backup leftover `%USERPROFILE%\.ollama` to a timestamped folder. Keep existing `config.json` integrations and `server.json` (`disable_ollama_cloud` should stay true).
2. Download official `ollama-windows-amd64.zip` **v0.32.14**. Verify SHA-256 against the GitHub release asset digest before unzipping into `D:\AI\ollama`.
3. Set `OLLAMA_HOST=127.0.0.1:11434` and `OLLAMA_MODELS=D:\AI\ollama\models`. Do not install the Windows service, tray app, or startup item. Do not bind `0.0.0.0`.
4. Create an isolated Python 3.12 environment with `uv` (host Python 3.14 cannot install Aider):

   ```powershell
   uv python install 3.12
   uv venv D:\AI\aider\.venv --python 3.12
   D:\AI\aider\.venv\Scripts\python.exe -m pip install aider-chat==0.86.2
   ```

5. Start Ollama on demand, then `.\ollama.exe pull qwen2.5-coder:7b`. Keep only that model loaded.
6. Confirm `nvidia-smi` shows the process on the NVIDIA GPU and `Get-NetTCPConnection -LocalPort 11434` is `127.0.0.1`.

Windows launchers: `py -3` and `mvn.cmd`. Do not use `python3` / `mvn.exe` as the probe names.

## How the orchestrator dispatches

From PowerShell or IntelliJ Terminal (`pwsh`):

```powershell
pwsh -NoProfile -File scripts/local-coding-agent/run_agent.ps1 `
  -Worktree C:/Users/Mohab/.grok/worktrees/shaft-engine-5017 `
  -Spec C:/path/to/spec.md `
  -Allowlist @('shaft-engine/src/test/java/.../ExampleTest.java') `
  -TestCommand 'mvn.cmd -pl shaft-engine -Dtest=ExampleTest test "-Dallure.automaticallyOpen=false"'
```

`run_agent.ps1` writes `D:\AI\reports\<utc-id>\`:

- `spec.md` — copy of the orchestrator spec
- `diff.patch` — allowlisted `git diff`
- `agent.log` — Aider output
- `test-output.txt` — focused Maven output when requested
- `report.json` — machine-readable result (`ok`, `model`, `worktree`, `files_allowed`, `files_changed`, `commit`, `test_command`, `test_exit`, `elapsed_ms`, `loopback`, `blockers`)

The orchestrator must read those artifacts. Do not trust the agent's prose. Schema: `report.schema.json`.

The wrapper refuses a missing worktree, spec, or allowlist, and refuses `-Push`.

On this machine Aider may print `Unsupported git index version 4` (Git for Windows uses index v4). That warning is non-fatal: the 5017 acceptance run still applied the allowlisted edit and created a local commit.

Stop only what we started:

```powershell
pwsh -NoProfile -File scripts/local-coding-agent/stop.ps1
```

## Recreate checklist

- [ ] `D:\AI\ollama\ollama.exe --version` reports 0.32.14
- [ ] `ollama list` shows `qwen2.5-coder:7b` only (or that plus unused pulled tags you accept)
- [ ] Aider `0.86.2` runs from `D:\AI\aider\.venv`
- [ ] `run_agent.ps1` emits `report.json` the orchestrator can quote
- [ ] `stop.ps1` removes the launcher PID and 11434 is gone or no longer ours
- [ ] Dirty sibling SHAFT worktrees were never used as `-Worktree`

## Rollback / uninstall

1. `stop.ps1`
2. Delete only `D:\AI` if we created it
3. Restore the `~/.ollama` backup
4. Do not delete `%USERPROFILE%\.ollama` wholesale — it may hold unrelated client integrations

## Related

- Report/launcher tests: `tests/scripts/test_local_coding_agent.py`
- Tracker: #5017; launchers subtask: #5060
