# Allure Report Internals

## Results model
`allure-results/` holds one `*-result.json` per test attempt (status:
passed/failed/broken/skipped; failed = assertion, broken = unexpected
exception), plus `*-container.json` (fixtures/grouping), attachments by UUID,
`categories.json`, `environment.properties`, `history/` (trend, keyed by
historyId = hash of fullName+params). Retries: multiple result files share a
historyId; the report shows the last, others become "retries".

## Verdict discipline (SHAFT hard rule)
An empty or unexpectedly small `allure-results` invalidates any pass/fail
verdict — count result JSONs and executed tests FIRST, and inspect failed,
broken, retried, and skipped attempts separately before declaring a run green
(repo gotcha `allure-result-population`). A suite that "passed" with 3 result
files when 40 tests exist means the run silently didn't execute.

## Single-file report + patching
`allure generate --single-file` inlines everything into one huge `index.html`
(tens of MB). Never `Files.readString` + `String.substring` to patch it —
that's 2-3 whole-file copies on the heap and OOMs on large reports (SHAFT
issue #3407). The proven pattern (PR #3433, `AllureManager`):
1. Pass 1: stream with a 64KB sliding-window byte scan, carrying a tail
   overlap so markers spanning read-chunk boundaries are found; locate
   patch-id presence, first `</head>`, last `</body>` offsets.
2. Pass 2: sequential copy through a sibling temp file, splicing patches at
   the recorded offsets; atomic move into place. Skip if already patched.
- ASCII byte search is UTF-8-safe (multibyte sequences can't contain '<').
- If a patch inserts before `</body>`, every later offset shifts by the sum
  of all earlier patch lengths — compute final offsets, don't re-scan.
- Windows: replace-moves are not atomic under file-watcher/AV contention;
  use `ATOMIC_MOVE` with fallback and never leave a half-written index.

## Ops rules
- Never `allure serve`/`allure open` in agent sessions (guard-enforced);
  generate only. History carries over by copying `history/` from the
  previous report into results before generating.
- Report JS executes at open; theme/branding patches belong in `<head>`
  (style) and before `</body>` (script) — SHAFT's patch constants must never
  themselves contain `</head>`/`</body>` literals or the marker scan breaks.

## Maven-provisioned Allure 3 CLI (#5801)
SHAFT never calls a user `PATH` `allure` binary (Allure 2 drop: #5798/#5800). Resolution order:

1. **Maven cache (preferred):** `~/.m2/repository/allure/allure-cli/<allure3Version>/node_modules/allure/cli.js`
   invoked as `node <cli.js>` (portable Node under `~/.m2/repository/nodejs/` when PATH node is missing).
2. `npx --yes allure@<allure3Version>` on PATH.
3. Portable Node + its `npx`.

**Operators / CI (offline / air-gap):**
```bash
mvn -Pprovision-allure-cli -pl shaft-engine -am initialize
# then tests / report generate can run without npm registry at generate time
```
Root POM property `allure.cli.version` must stay aligned with `Internal.allure3Version()` (currently 3.17.0; independent of `allure-bom` 3.0.0 Java adapters).

Overrides: `-Dallure.cli.cacheRoot=...`, `-Dallure.cli.skipProvision=true` (engine bootstrap only).
