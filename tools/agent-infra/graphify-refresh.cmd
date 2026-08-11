@echo off
REM Nightly graphify maintenance (user-level task, daily).
REM Registered by install-agent-tasks.ps1; runs from the source-controlled repo copy.
REM Rebuilds, audits, and re-clusters the shared repository-map cache from this
REM checkout so worktree sessions read fresh.
REM Logs stay machine-local (never in the repo).
cd /d "%~dp0..\.."
py -3 tools\repository-map\graphify_maintenance.py refresh --root . > "%USERPROFILE%\.agent-infra\logs\graphify-refresh.log" 2>&1
if errorlevel 1 exit /b 1
if not errorlevel 0 exit /b 1
