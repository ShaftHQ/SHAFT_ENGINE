@echo off
REM Nightly graphify maintenance (user-level task, daily).
REM Registered by install-agent-tasks.ps1; runs from the source-controlled repo copy.
REM Rebuilds + re-clusters the shared repository-map cache (graphify-out/) from
REM this checkout so worktree sessions read fresh.
REM Logs stay machine-local (never in the repo).
cd /d "%~dp0..\.."
py -3 -m graphify . > "%USERPROFILE%\.agent-infra\logs\graphify-refresh.log" 2>&1
py -3 -m graphify cluster-only . >> "%USERPROFILE%\.agent-infra\logs\graphify-refresh.log" 2>&1
