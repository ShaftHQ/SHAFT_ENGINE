# Installer program executable specification

Sibling of [`installer-program.md`](installer-program.md). Required before the
first implementing commit on any child. Re-copy onto that child if a cell
changes. Portable payload must stay free of repository-identity tokens; quote
artifact ids and upstream URLs only from origin-only [`INSTALL.md`](../INSTALL.md)
and the live profile catalog.

## Resolved caller matrix

| Site | Effective cwd/path | Runtime/version/platform | Permissions/trust | Configuration precedence | Input existence |
| --- | --- | --- | --- | --- | --- |
| Empty first install | Operator cwd is the empty directory; wrappers install into cwd | POSIX `install.sh` / Windows `install.ps1`; Python bootstrapped | User-scoped uv/npm; no sudo | Official upstream URL from INSTALL.md unless `CHAOS_ENGINE_REPOSITORY` | Directory exists and is writable |
| Java first install | Operator cwd is the Maven project root that contains `pom.xml` | Same wrappers; Temurin 25 for Maven Tools | Same | `installWhen.mavenArtifactIds` selects `repository` | Root `pom.xml` readable, not a reparse point |
| Non-Java first install | Operator cwd is the existing project root | Same wrappers | Same | No matching Maven id → `portable` | Project files may already include host configs |
| Upgrade of each profile | Same cwd as the original install | Same wrappers; existing `.chaos-engine/` verified or rematerialized | Receipt-owned replace; persistent data retained | Existing bundle-options + current payload | `.chaos-engine/` present; hosts receipt may be drifted |
| Conflict merge | Same project cwd | Deterministic `hosts.py` merge, no LLM | Foreign bytes are operator-owned | Owned markers/records lose to fail-closed when ambiguous | Pre-existing `AGENTS.md` / hooks / `.mcp.json` / Codex TOML |
| #5667 verify | Adopter win32 class: hosts receipt present, core present | `py -3`; `Scripts\python.exe` layout | Doctor probes must not require origin/main for required `mcps` | Doctor `status` ⊆ `doctor` for required components | Core rematerialized at commit `887facff34…` |

## State/failure matrix

| State | Immutable ownership | Preflight | Mutation order | Mixed state | Atomicity | Concurrency | Idempotency | Recovery | Fail-closed |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Fresh empty | N/A | Writable cwd, not a link | Resolve → download → deps → core → hosts → verify | Not allowed | Existing journal/lock | Install lock | Second run is upgrade | Uninstall then retry | Link/reparse, unsigned payload |
| Fresh Java | Operator POM bytes | POM parse for ids | Same, plus Maven Tools cache publish | POM never rewritten | Cache rename-publish already atomic | User-cache lock | Re-run rediscovers JAR | Repair `--component` hosts/mcps | Missing Temurin 25 on Maven project |
| Fresh non-Java | Operator project files | Detect no matching id | Same as empty; skip required Maven Tools | Foreign files preserved | Host file writes stay staged then published | Install lock | Second run is upgrade | Handoff if merge impossible | Unknown same-name MCP |
| Upgrade | Receipt-owned vs persistent split | Drift/wiped-runtime gates (#5633/#5587/#5636) | Quarantine stale receipt if needed, rematerialize core, rebind hosts | Persistent data + new core is required | Backup/journal already in `install.py` | Install lock | No-op when already at commit | rollback then one-liner | Broken tree not used as backup |
| Mergeable foreign | Foreign bytes | Parse + marker count | Merge after core verify | Owned span new, foreign old | Per-file stage + replace | Single writer | Exact second merge is no-op | Re-run | None if mergeable |
| Impossible foreign | Entire foreign file | Detect impossible **before** write | Skip file, write handoff | Core new, that file old | Handoff write after core commit | Single writer | Re-run refreshes handoff only | Agent prompt then doctor | Do not guess-edit |
| #5667 probes | Core + receipts | Resolve managed Python on win32 | Probe after core; missing interpreter is a named component failure, not a silent dual fail | Core healthy, probes advisory or detailed recovery | Probes are read-only | Timeout 30s | Doctor repeatable | `repair --component hooks` / `mcps` with named `fix-next` | Symlink guard; never `{}` allow on missing guard |

## Acceptance-to-proof map

| Criterion or invariant | Positive proof | Negative or mutation proof | Command |
| --- | --- | --- | --- |
| Empty first install reaches healthy doctor | Smoke fixture doctor `status` healthy, distribution `portable` | Delete `hooks/guard.py` after install → doctor unhealthy `hooks` | `python3 scripts/ci/chaos_engine_empty_project_smoke.py --output /tmp/ce-smoke.json` plus focused unittest |
| Java first install selects repository | Manifest `distribution.id` is `repository`; Maven Tools component present | POM without the configured `installWhen.mavenArtifactIds` entry must not select repository | Focused installer distribution tests |
| Non-Java first install stays portable | Manifest `portable`; missing Maven Tools does not fail required health | Inject matching artifact id → flips to repository | Same tests, non-Java fixture |
| Upgrade preserves foreign bytes | Pre-seed `AGENTS.md` prose; after upgrade file contains that prose plus current owned block | Pre-seed colliding markers → no overwrite of foreign file | Hosts merge tests |
| Impossible merge is success + handoff | Collision fixture: exit 0, `merge-handoff.md` exists, stdout has styled success and one backtick prompt, no `CE-INSTALL-FAILED` | Mutation: omit handoff write → test fails | New installer UX + hosts tests |
| Foreign MCP server survives | Unknown server in `.mcp.json` remains after install | Same-name unknown `chaosengine-memory` → impossible, not overwrite | Existing MCP migration tests + new collision-handoff test |
| #5667 no bare dual fail | When managed Python is missing on nt, `hooks` and `mcps` include `code` + `detail` + `fix-next`; verify does not fail solely because Memory origin/main is desynced | Mutation: drop detail → test fails | `tests.scripts.test_chaos_engine_installer` / hosts doctor tests on win32 layout |
| Doctor status ⊆ doctor for required components | Existing contract tests | Status healthy / doctor unhealthy still forbidden | `python3 -m unittest tests.scripts.test_chaos_engine_installer -v` (focused) |
| Sibling omission | Merge AGENTS.md but skip `.mcp.json` in a fixture → MCP test fails | — | Hosts publish atomicity tests already required by #5368 |
