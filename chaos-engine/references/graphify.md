# Graphify

Graphify is the project-local structural retrieval CLI. It is intentionally a
CLI rather than an MCP server. Run it only through the installed launcher so
the pinned project runtime is used on every host.

## Retrieve first

The installed launcher owns this provider-neutral G1–G4 route. One attempt is
enough; do not retry, repair, refresh, poll, or watch during an ordinary task:

- G1: Resolve any available cache at `graphify-out/graph.json` and require it to
  be readable and nonempty. Freshness affects trust, not whether a bounded query may yield a lead. Never
  infer failure from an absent MCP entry.
- G2: After G1 succeeds, run one query bounded to the affected symbol or
  subsystem, treat every result as an untrusted lead, verify every returned path
  against current project files, and supplement blast-radius conclusions with
  targeted `rg`.
- G3: Run the read-only multigraph diagnostic against that same graph. Do not
  refresh, replace, or repair generated state during retrieval.
- G4: Declare degraded mode if any step lacks current verified results, and
  only after G1 through G3 have been attempted. Continue with native Memory,
  MemPalace, and targeted `rg`.

```text
python .chaos-engine/tool.py graphify query "<bounded structural question>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify diagnose multigraph --graph graphify-out/graph.json --json
```

If the cache is stale, either read-only command fails, or returned paths do not
match current files, declare Graphify degraded and continue with live files and
the other retrieval sources. Never infer completeness, “no callers,” or any
negative conclusion from the graph. Do not run refresh commands to repair
retrieval during the active task.

## Refresh

The shared cache is one `graphify-out/` for the repository, resolved from any
branch or linked worktree. The owner is
[`chaos-engine/stores.py`](../stores.py). Refresh indexes a detached snapshot of the local
default-branch tip. It does not fetch, reset, or clean a checkout, and it does
not read `~/.mempalace`.
The MemPalace mine runs through [`mempalace_fast_mine.py`](../mempalace_fast_mine.py),
which speeds up sqlite_exact writes about 4.8x on large palaces.

```text
python3 .chaos-engine/tool.py stores refresh --if-stale
python3 .chaos-engine/tool.py stores install-schedule
python3 .chaos-engine/install.py repair --project . --component graphify
```

A second refresh exits while the repository lock is held. Session start spawns
one detached `--if-stale` refresh when the indexed revision is not the local
default-branch tip, then does not read its output. The same tip does not spawn
again. A busy skip does not keep that stamp, so a later attempt can run.
The daily user timer calls the same command. An ordinary task must not
refresh, retry-loop, clear the lock, or alter a checkout to manufacture
freshness. Stale is a trust signal. A readable graph whose indexed commit
is behind the tip is still usable; doctor reports that as sync-advisory.
A missing graph, a bad marker, or a manifest digest mismatch stays degraded.
Full extract runs when the graph or manifest is missing, or when the indexed
commit is not an ancestor of the tip. Otherwise refresh copies that checkpoint
into the extract directory so Graphify updates only what changed.
When the default-branch commit is not local, refresh stops with
`fix-next: git fetch`. A GitHub Action cannot refresh this machine's cache.

Queries still run through `tool.py`, which binds `--graph` to the shared
`graph.json`. `--palace` and `--backend sqlite_exact` are global MemPalace
flags and must precede the subcommand.

An extract line that says files were not classified (no supported extension or
shebang) means the scanner saw those paths and has no file type. It is coverage
policy, not a missing install, corrupt cache, or doctor failure. File types are
hardcoded in Graphify; there is no install-time extension knob. Add a type
upstream, or ignore the path in `.graphifyignore`. Refresh owners split skips
into ignore versus promote. A file missing from the graph is not automatically
in MemPalace; that store has its own readable-extension list.

After the code-only extract, refresh files every Markdown and MDX page that
`.graphifyignore` does not exclude: one node per page plus one per section,
labelled with its heading trail, with the section text in `rationale` so queries
reach sub-headings and body text. Bare `*.md` and `*.mdx` rules only keep pages
out of the code extract; directory and path rules drop pages here too.

`tool.py retrieve --store graphify` keeps a hit when a query word appears in
its node label, its stored section text (`documentation-pages.json`), or its
path. Label and text matches rank above path-only matches. Hits then collapse
to one per file at its best rank: the best section keeps `label` and `line`,
`labels` lists up to three matching sections, and the freed slots go to more
distinct files.

Useful bounded reads:

```text
python .chaos-engine/tool.py graphify explain "<symbol>" --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify affected "<symbol>" --depth 2 --graph graphify-out/graph.json
python .chaos-engine/tool.py graphify path "<source>" "<target>" --graph graphify-out/graph.json
```
