# Token tactics card

On demand; every bot and host. Measured waste first: polling, CI round trips, long reports.

- Register: Caveman and Ponytail ultra for bot-to-bot messages (agent messages,
  handoffs, delegate briefs and reports) and internal thinking or notes, keeping
  the full reasoning. Humans get short plain full sentences under Caveman content
  rules (no filler, preamble or narration; each fact once), not telegraphic style.
- Retrieve once (graphify or mempalace) before broad search; scope `rg` to hits.
  Bots: `tool.py checkpoint` refuses a work step until a retrieve is recorded since entry.
- Open bodies from the [harness map](../references/harness-map.md) only when a line matches.
- Script first; one search; read ≤ 80-line excerpts; filter logs to the verdict.
- Delegate large reads; their content stays in the delegate's context.
- Compact or summarize at task boundaries, then rerun `tool.py entry --full`.
- Stable prefix first (cards never carry timestamps) so prompt caches hit.
