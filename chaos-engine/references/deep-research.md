# Deep research

Portable contract for sourced research. Host workflow scripts may implement
the same phases; this file owns the host-neutral rules.

## Use when

The owner asks to compare, investigate, or write a cited report that needs
independent evidence. Isolated explore still follows
[context-firewall](context-firewall.md).

## Phases

1. **Plan** — split the query into a small set of independent questions
   (default cap 4, hard cap 6). No paraphrases of the same question.
2. **Research** — collect structured claims, evidence, and sources in
   parallel. Each claim names its source.
3. **Verify** — independently cross-check every candidate claim. Missing,
   failed, or unusable verification is not a confirming vote.
4. **Report** — synthesize only verified claims into cited prose. Drop
   unverified claims or mark them as unresolved.

Do not pretend a host TUI workflow ran if the host has no workflow runner.
Bounded parallelism and citation validation remain mandatory on every host.
