SUPERSEDED 2026-07-27 memory-hygiene cleanup: gbrain was fully uninstalled 2026-07-18 (see decision.gbrain-fully-uninstalled-2026-07-18-supersedes-same-day-keep-as-accelerator-decision) -- this describes operational quirks of a tool that no longer exists in this environment. Kept as historical record, not deleted. Original text preserved below.

---

Calling mcp__gbrain__put_page from within a git working directory produced a response with write_through.written=true and write_through.path pointing at a real .md file created at the repo root (not inside gbrain's own store). This is an untracked-file side effect that git status will show and that must be cleaned up before committing -- it is not merely a remote database write.