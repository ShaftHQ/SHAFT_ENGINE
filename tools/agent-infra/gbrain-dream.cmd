@echo off
REM Nightly agent-infra maintenance (user-level task, daily).
REM Registered by install-agent-tasks.ps1; runs from the source-controlled repo copy.
REM 1) gbrain dream: sync, extract, embed, consolidate/takes, orphans, purge.
REM 2) graphify: rebuild + re-cluster the shared repository-map cache
REM    (graphify-out/) from this checkout so worktree sessions read fresh.
REM Logs stay machine-local (never in the repo).
"%USERPROFILE%\.bun\bin\gbrain.exe" dream > "%USERPROFILE%\.gbrain\autopilot\dream.log" 2>&1
cd /d "%~dp0..\.."
py -3 -m graphify . > "%USERPROFILE%\.gbrain\autopilot\graphify-refresh.log" 2>&1
py -3 -m graphify cluster-only . >> "%USERPROFILE%\.gbrain\autopilot\graphify-refresh.log" 2>&1
