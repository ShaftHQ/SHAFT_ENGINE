Issue #4458, decided in PR #4475: keep the global total-guidance ratchet retired and cap the on-demand reference surface per file instead. `file_budgets` keys may be globs, and `.agents/skills/act-as-mohab/references/**/*.md` is capped at 16384 LF-normalized bytes per file.

The rationale is incentive locality, not bytes loaded. Under a shared pool, the bearer is whoever must delete unrelated prose to fund a neighbour's growth. Under a per-file cap, the bearer is whoever grew the file. Prefer the latter; do not re-derive it from load arguments.

The per-file cap bounds the worst single file, not a session total. Growth by file count or many files growing together remains unmetered. Dynamic totals and largest-file facts belong to validator output, not this durable decision. Splitting can relocate load rather than reduce it, but it makes the author of the oversized file carry the remediation.

The per-surface caps from #3745 were never renegotiated; the retired pooled floor was repeatedly renegotiated. The `references/` relief valve remains functionally open because the cap is per file and far above a relocated skill body.