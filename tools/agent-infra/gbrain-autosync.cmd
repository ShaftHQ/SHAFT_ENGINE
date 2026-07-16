@echo off
REM gbrain incremental sync of all registered sources (user-level task, every 30 min).
REM Registered by install-agent-tasks.ps1; runs from the source-controlled repo copy.
REM --no-pull: never touch git state of working repos; sync what is on disk.
REM --skip-failed: acknowledge parse failures (e.g. Docusaurus SLUG_MISMATCH,
REM   ShaftHQ/SHAFT_ENGINE#3618) so one bad file never wedges the sync bookmark.
REM Exits nonzero harmlessly while a gbrain serve MCP session holds the PGLite lock.
REM Logs stay machine-local (never in the repo).
"%USERPROFILE%\.bun\bin\gbrain.exe" sync --all --no-pull --skip-failed --no-hard-deadline > "%USERPROFILE%\.gbrain\autopilot\autosync.log" 2>&1
