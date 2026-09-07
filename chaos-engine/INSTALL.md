<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/brand/symbol-dark.svg">
    <source media="(prefers-color-scheme: light)" srcset="assets/brand/symbol-light.svg">
    <img alt="ChaosEngine symbol" src="assets/brand/symbol-light.svg" width="180">
  </picture>
</p>

# Install or upgrade ChaosEngine

ChaosEngine is a portable, provider-neutral working contract for software
agents. This page is the installation reference. See [`README.md`](README.md) for purpose and operating loop, and [`skills/chaos-engine/SKILL.md`](skills/chaos-engine/SKILL.md) for the always-loaded contract.

## Golden path (first run)

1. Change into the project directory you want ChaosEngine to manage.
2. Run exactly one of these one-liners (Python is not required beforehand).
3. When it finishes, run human doctor and confirm it reports healthy.

Windows PowerShell, using [install.ps1](install.ps1):

```powershell
irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex
```

macOS or Linux, using [install.sh](install.sh):

```bash
curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh" | bash -s -- "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh"
```

Verify:

```text
python3 .chaos-engine/install.py doctor --project .
```

On Windows use py -3 instead of python3. A successful install also prints a
first-session brief and five host onboarding cards. You can stop reading here
for a normal first install.

## Advanced topics

Everything below is optional on first run: agent install instruction, payload
details, OmniRoute, identity/portability, interactive flags, Maven Tools,
uninstall/rollback, and empty-project smoke.

Origin identity masters under `assets/brand/`, the origin adoption matrix
`RESEARCH.md`, and `STANDALONE.md` stay in the source tree and are not copied
into the adopter payload. The installer also merges receipt-bound LF attributes for canonical harness paths,
so Windows Git checkouts retain the exact owned bytes while unrelated
`.gitattributes` rules remain untouched.

The canonical one-liners above use the official `ShaftHQ/SHAFT_ENGINE` upstream
URLs. The scripts parse their invocation URL and do not copy that identity into
the adopter payload. `CHAOS_ENGINE_REPOSITORY` remains a local-file override when
the invocation URL cannot be parsed (for example when you run `install.sh` from a
checked-out tree). Change into the target project or folder first; both scripts
install into the current working directory.

Python is not required before either command. The wrapper bootstraps Python as
needed. The installer then discovers the invoking account's tools, resolves
official stable channels, and chooses `reused`, `installed`, `upgraded`,
`repaired`, or `blocked` per dependency. It installs latest stable Python
through uv, uv itself through Astral, active
LTS Node 22 or newer through the platform provider, and Temurin 25 through
Adoptium-supported packages. uv tools and npm globals remain user scoped;
neither uses sudo. An unavailable stable channel blocks activation because an
older installation cannot be certified as latest. `.chaos-engine-dependencies.json` records
sanitized observed versions, absolute executables, probes, providers, actions,
and freshness without claiming ownership of global packages.

A root `pom.xml` enables Maven Tools MCP automatically. Pass `--with-maven-tools`
to force it on a non-Maven project. `--skip-tools` still skips it. Java
resolution prefers `CHAOSENGINE_JAVA`, then `JAVA_HOME`, then `PATH` and requires
Temurin Java 25. No private JDK archive is downloaded. Project uninstall retains
user-account packages, project knowledge data, and the shared Maven Tools cache;
cache purge removes only exact receipt-verified cache content.

POSIX:

```bash
url="https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh"; curl -fsSL "$url" | bash -s -- "$url" --with-maven-tools
```

PowerShell:

```powershell
$installer = irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1"; & ([scriptblock]::Create($installer)) -WithMavenTools
```

Inspect the linked installer and [bootstrap.py](bootstrap.py) first when policy
requires review before execution. The bootstrap resolves the default branch to
an immutable commit and downloads only its validated `chaos-engine/` subtree;
`portable` is already the default and need not be supplied. Restart any client
that was open during installation so it loads its verified local plugin cache.

Pass `--interactive` on macOS/Linux (PowerShell `-Interactive`) for the first-run
wizard: it detects host CLIs, explains what will be installed (including Caveman +
Ponytail companions), and prints a host-specific next-actions checklist before any
download. The non-interactive one-liner path is unchanged.

Set `CHAOS_ENGINE_BRANCH` to override the repository's configured default
branch (otherwise `main`). The bootstrap resolves that mutable branch through
the GitHub API, downloads the exact commit's declared harness files, rejects
unsafe tree entries, and records repository, immutable provenance digests and
the commit in `.chaos-engine/manifest.json`.
The public default is the neutral `portable` distribution. A bundled
repository profile is installed only when the target project's root `pom.xml`
matches that profile's declared Maven artifact ids. Pass `--distribution` to
the bootstrap if you need to override the detected choice. Re-running the
same command upgrades to the latest resolved commit; an offline or invalid
download leaves the last verified installation unchanged. A drifted, CRLF-converted,
extra-file, or otherwise broken `.chaos-engine` directory is replaced with a
fresh verified payload. Controllers from that broken tree are not executed, and
the broken tree is not kept as a rollback backup. A leftover install journal
in front of a still-broken tree is dropped so the next run can retry. Link or
reparse trees stay fail-closed. Uninstall and rollback of a still-drifted tree
also stay fail-closed. Generated `__pycache__` files are not treated as
ownership drift. The bootstrap retries
transient timeout, connection, rate-limit, and server responses with bounded
backoff, while permanent client errors fail immediately.

Installations created before distribution-bound manifests are reported as
`legacy`. To prevent repository-specific content from surviving in a backup,
convert them with an explicit uninstall followed by the portable bootstrap.
The installer refuses an in-place legacy conversion and leaves the old tree
unchanged.

ChaosEngine initializes and mines the current folder with upstream MemPalace,
then installs Graphify's project integration and runs code-only extraction when
its index is absent. Existing initialized data and indexes are preserved.
ChaosEngine-created MemPalace MCP servers use MemPalace's bundled
`sqlite_exact` backend explicitly. It keeps the complete local MCP contract but
has no native HNSW index, so separate agent sessions cannot enter Chroma's
process-crash and derived-index-corruption path. An upgrade never converts or
deletes existing generated memory state silently. After dependency provisioning,
a fresh install creates only the empty SQLite-exact schema and default collection;
it never files adopter content. Passive `status` and active `doctor` validate
that state through read-only SQLite queries. A structurally valid legacy Chroma
palace reports `migration-required` before any MCP launch; corrupt, unrecognized,
or incomplete state reports `recovery-required`. ChaosEngine does not migrate,
archive, rename, delete, or claim receipt ownership of palace state, so install
rollback and uninstall deliberately leave initialized or user-generated state unchanged.
Migration remains an explicit operator-owned MemPalace procedure and must use a
verified backup plus the upstream workflow appropriate to that MemPalace
version. `doctor` stays blocked until the operator supplies a fresh or valid
SQLite-exact palace. This containment avoids the native HNSW path for generated
clients; it does not repair the upstream Chroma/HNSW defect. Never remove a
writer lock or rename an HNSW segment while a MemPalace process is live.

The consumer folder may be a GitHub checkout, another Git checkout, or a
non-Git directory. ChaosEngine installs project-locally and does not infer its
upstream from the consumer repository.

`status` and `doctor` report every component with its `owner`, `scope`,
`lifecycle`, and `taskImpact`. Without `--json`, `doctor` (and `status`) print a
short human summary when healthy, or a scannable failure list with one
`fix-next` action per unhealthy component. Pass `--json` for the stable
schema v2 machine contract. **Contract:** for every **required** component,
`status` ⊆ `doctor` — status must never report healthier than doctor (the
plugins false-healthy case is closed by reconciling marketplace activation in
both commands). `doctor --json` also emits per-host `activationProof` and
`clients`. CE-INSTALL-FAILED / Verify installation / doctor share the same
component ids and fix-next vocabulary. Memory, MemPalace, and Graphify are
advisory to ordinary tasks but remain strict in `doctor`; an unhealthy selected
store still returns `recovery-required`. Maven Tools MCP is auto-installed when
the project has a root `pom.xml`. On non-Maven projects it stays optional and
absent does not make project health fail.

### Default-on all-in-one bundle

The unattended one-liner provisions **Memory + MemPalace + Graphify + Ponytail +
Headroom** (pin + CLI, CE `agent-90` policy, beacon off) and keeps Caveman on.
Disable only with flags (combinable):

```bash
--without-memory --without-mempalace --without-graphify \
  --without-ponytail --without-headroom --without-caveman
```

Headroom CLI is auto-provisioned via `uv tool install --python 3.13
"headroom-ai==0.37.0"` unless `--without-headroom` is set. Ponytail XOR
`HEADROOM_OUTPUT_SHAPER` remains enforced.

### Component repair (no full wipe)

```bash
python3 .chaos-engine/install.py repair --project . --component plugins
python3 .chaos-engine/install.py repair --project . --component headroom
python3 .chaos-engine/install.py repair --project . --component mempalace
```

Supported components: `plugins`, `hosts`, `core`, `headroom`, `mempalace`,
`graphify`, `memory`, `hooks`, `mcps`, `skills`, `roles`, `tools`. One-command
**update** is the same install one-liner (repair/reinstall semantics aligned with
this health truth).



## Uninstall / rollback (first-time recovery)

Use these when a first install goes wrong and you need a clean retry.

### Commands

- doctor: python3 .chaos-engine/install.py doctor --project .
- rollback: python3 .chaos-engine/install.py rollback --project .
- uninstall: python3 .chaos-engine/install.py uninstall --project .

On Windows, use py -3 instead of python3. Prefer rollback for a bad upgrade; prefer uninstall then the one-liner for a wiped or drifted tree.

### What is removed vs retained

Removed (receipt-owned): portable core tree; CE-owned host adapters; CE-owned dependency/runtime generations; CE install journals/locks.

Retained: user-account packages; project knowledge data; shared Maven Tools MCP cache; unrelated project files.

Mixed or unknown ownership fails closed. Knowledge stores are never deleted by uninstall or rollback.

### Clean reinstall after a bad first run

1. Run human doctor and follow each fix-next line.
2. If rollback is available, rollback then rerun the Install one-liner.
3. Otherwise uninstall, confirm retained knowledge data if needed, then rerun the Install one-liner.
4. Restart open hosts and re-run doctor.


## Empty-project smoke (< 5 minutes)

Reference profile: brand-new empty directory on **Ubuntu 22.04** with Python 3.13
and no prior ChaosEngine install.

1. `mkdir /tmp/ce-empty-smoke && cd /tmp/ce-empty-smoke`
2. Run the **same macOS/Linux one-liner** from the Install section above (do not
   duplicate it here — keep a single documented URL).
3. Time it (`time` / `/usr/bin/time`) and then run
   `python3 .chaos-engine/install.py doctor --project .`

Expect install + healthy human doctor within **300 seconds**. CI attaches the
fresh-account phase stopwatch from
`scripts/ci/chaos_engine_live_installer_acceptance.py` as evidence on that
reference profile. Local/CI fixture helper:

`python3 scripts/ci/chaos_engine_empty_project_smoke.py --output /tmp/ce-smoke.json`

If doctor is not healthy, follow every `fix-next` line, then open a GitHub
issue and paste the full doctor output (including fix-next lines).

## Troubleshooting

### `status` healthy but `doctor` unhealthy (false-healthy)

For **required** components this must not happen. If you still see it on an
older install, upgrade ChaosEngine, then:

```bash
python3 .chaos-engine/install.py status --project .
python3 .chaos-engine/install.py doctor --project .
python3 .chaos-engine/install.py repair --project . --component plugins
```

`doctor --json` includes `activationProof` per detected host.


### `tool.py` says primary checkout HEAD != origin/main

On the SHAFT_ENGINE monorepo (or any checkout that ships
`tools/repository-map/resolve_mempalace.py`), ChaosEngine pins the shared
**Memory** store to the primary checkout's `origin/main` revision.

- `memory` / `memory-mcp` **hard-fail** when primary `HEAD` is not exactly
  `refs/remotes/origin/main`, and print a `fix-next` line.
- `mempalace` / `mempalace-mcp` / `graphify` **soft-warn** and still run when
  their own doctor status is healthy, so advisory stores stay queryable on a
  behind-main primary checkout or worktree.

Fix the primary checkout (not a task worktree):

```bash
git fetch origin main && git merge --ff-only origin/main
```

Then rerun the tool, or confirm with
`python3 .chaos-engine/install.py doctor --project .`
(and `--fix-next-only` when scripting).

### Codex MCP collision / orphan `context7`

Re-running install self-heals CE-owned Codex MCP sections (`context7`,
`chaosengine-memory`, `chaosengine-mempalace`, `maven-tools-mcp`) that sit
outside or inside a drifted `# CHAOSENGINE:START`…`END` block. Non-owned
user MCP servers are left untouched. No need to empty `.codex/config.toml`
manually.

### Orphan `.chaos-engine-hosts.active-*` anchors

Leftover active/removing host anchors when `.chaos-engine/` is missing are
quarantined under `.chaos-engine-state/orphaned-*` on the next install
(same wiped-runtime heal as a stale hosts receipt).

### Install verify / doctor and `HEAD != origin/main`

`memory` / `memory-mcp` **tools** still hard-fail writes when primary
`HEAD` is not `origin/main`. Install verify and doctor treat that probe
exit as **compatible-legacy** for required `mcps` (advisory + fix-next),
while still probing `mempalace-mcp`. Sync when you need Memory writes:

```bash
git fetch origin main && git merge --ff-only origin/main
```

### JRE without `javac` (Maven Tools)

If only a JRE is on `PATH`, install provisions a Temurin JDK into the
ChaosEngine tools cache so Maven Tools can compile.

### Windows `WinError 2` during Provision dependencies

Install resolves `pwsh`/`powershell` and PATHEXT launchers before
CreateProcess. A missing tool names the executable and prints fix-next
instead of an opaque WinError 2; the portable core is kept so re-run can
self-heal.

### MemPalace `sqlite_exact` FTS5 malformed

Doctor/heal attempts a bounded FTS rebuild and quarantines clearly-operator
`*.bak` siblings when safe.

### Missing dependency receipt / wiped `.chaos-engine` runtime

If `.chaos-engine/` was deleted or replaced, and/or
`.chaos-engine-dependencies.json` is missing, tools fail closed:

- `python3 chaos-engine/tool.py …` / `python3 .chaos-engine/tool.py …` prints
  `dependency pointer is missing or invalid` (or a wiped-runtime controller
  error) with a **fix-next** that names install/doctor, and exits **non-zero**.
- `doctor` / `status` report `CE_CORE_MISSING` or `CE_WIPED_RUNTIME` when a
  stale `.chaos-engine-hosts.json` (and/or `.chaos-engine-hosts.active-*`
  anchor) no longer matches the installed core.

**Heal (preferred):** rerun the official install one-liner from the Install
section above. Install quarantines orphaned host receipt/anchors under
`.chaos-engine-state/`, rematerializes `.chaos-engine/` from upstream or the
local `chaos-engine/` source tree, recreates the dependency receipt, and
rebinds hosts from the current core. No manual receipt surgery.

Then confirm:

```bash
python3 .chaos-engine/install.py doctor --project .
```

Data directories (`mempalace.yaml`, `graphify-out`, `.memory`) are left in place
when possible; heal restores tooling without requiring a full data rebuild.

### Host adapter drift with deps + core still present

After a git fast-forward (or `git restore`) of receipt-owned host overlays while
`.chaos-engine/` and `.chaos-engine-dependencies.json` remain, doctor may report
`CE_HOST_ADAPTER_DRIFT` / Blocked host receipt mismatch, and install can fail
closed with `host adapter drift`. This is the post-#5631 upgrade gap: wiped-runtime
quarantine does **not** fire when the dependency receipt is present.

**Heal (preferred):** rerun the official install one-liner, or:

```bash
python3 .chaos-engine/install.py repair --project . --component hosts
```

Install/repair quarantines the drifted `.chaos-engine-hosts.json` (+ active
anchors) under `.chaos-engine-state/`, then rebinds hosts from the current core.
Foreign user MCP servers outside ChaosEngine ownership are preserved. Do not
manually delete MCP config to clear the drift.

### Kept core without hosts receipt

If a prior install kept `.chaos-engine/` after a provision failure (#5631) but
never wrote `.chaos-engine-hosts.json` (doctor/`CE_HOSTS_RECEIPT_MISSING`),
rerun the one-liner or:

```bash
python3 .chaos-engine/install.py repair --project . --component hosts
```

Install clears a stale account-rollback journal that cannot authenticate host
pairing, then binds hosts from the current core. No manual quarantine.

### Install progress phases (core vs dependencies)

Install core and Provision dependencies run **sequentially**. The TTY checklist
shows only the active phase as running. Interactive installs default to a richer
live trace (downloads, tool commands with secrets redacted); set
`CHAOS_ENGINE_QUIET=1` or run under `CI=1` for the compact trace window.



## Optional native Maven Tools MCP

Do not put `docker run -i --rm` in a default stdio MCP configuration. Each
active client owns its own stdio server process, so a Docker-backed declaration
creates one container per client and keeps Docker Desktop and its VM resident.

A root `pom.xml`, or `--with-maven-tools`, performs the upstream native JAR flow:

1. Resolve system Temurin 25 from `CHAOSENGINE_JAVA`, `JAVA_HOME`, or `PATH`.
2. Resolve the latest compatible stable GitHub release, clone its tag with
   `--depth 1`, record the tag's immutable commit, and run
   `./mvnw -B clean package -Pci`. Git and Java are required; no private archive
   fallback exists.
3. Stage `maven-tools-mcp-<resolved-version>.jar` under a fresh unique directory on the same
   filesystem as the current user's data directory, then publish that directory
   with a no-overwrite rename to
   `ChaosEngine/tools/maven-tools-mcp/<resolved-version>/`. On Windows the data directory is
   `%LOCALAPPDATA%`; elsewhere it is `$XDG_DATA_HOME`, or `~/.local/share` when
   that variable is unset. `CHAOSENGINE_MAVEN_TOOLS_MCP_JAR` may name a
   different verified JAR. Beside it, write `install-receipt.json` with exactly
   these keys: `version`, immutable `commit`, `jar` =
   the installed filename, and `sha256` = the lowercase SHA-256 of its bytes.
   Discovery recomputes the digest and rejects a missing, malformed, stale, or
   differently pinned receipt. The version directory is an immutable,
   receipt-owned shared cache: parallel projects may read the verified pair, while
   project uninstall never changes or removes it.
4. Host installation discovers both files and atomically rewrites `.mcp.json`, `.gemini/settings.json`, and
   `.codex/config.toml` with their resolved absolute paths. Upgrades repeat
   discovery, so another user's Java or data path is never inherited.
5. Start a fresh client session and prove both the MCP initialize and tools/list
   responses over the upstream default stdio transport. Native mode launches
   only `java -jar <verified-jar>` and never installs or starts Docker.

Docker remains opt-in for users who already run a healthy Docker daemon:

```text
python .chaos-engine/install.py install --project . --with-maven-tools --maven-tools-mode docker
```

This resolves the same latest stable Maven Tools release, pins
`arvindand/maven-tools-mcp:<resolved-version>`, and writes the absolute Docker
executable into each generated MCP configuration. ChaosEngine never installs,
starts, or upgrades Docker.

If an ambient Temurin 25 runtime cannot run the JAR,
installation fails closed on a Maven project and omits the server on a
non-Maven project. Maven CLI, repository files, Context7, and authoritative
Maven Central sources remain the no-Docker fallback.

Inspect or remove the exact supported cache version with:

```text
python .chaos-engine/install.py cache status --component maven-tools-mcp
python .chaos-engine/install.py cache purge --component maven-tools-mcp --version <resolved-version>
```

`cache status` returns `healthy`, `absent`, `invalid`, or `busy`. `cache purge`
takes a non-waiting user-cache lock and removes only the verified receipt-owned
JAR and receipt. It refuses modified, linked, unknown, broad, or busy targets;
an absent version is already successful.

An installing agent can use this PowerShell sequence after the source build:

```powershell
$version = '3.2.0'
$commit = '4475ff6c61f23ea9a93cb6d5665a63235ef2ef36'
$cacheRoot = Join-Path $env:LOCALAPPDATA "ChaosEngine\tools\maven-tools-mcp"
$staging = Join-Path $cacheRoot (".staging-" + [guid]::NewGuid().ToString('N'))
$jar = Join-Path $staging "maven-tools-mcp-$version.jar"
New-Item -ItemType Directory -Force -Path $cacheRoot | Out-Null
New-Item -ItemType Directory -Path $staging | Out-Null
Copy-Item -LiteralPath "target\maven-tools-mcp-$version.jar" -Destination $jar
$receipt = [ordered]@{
  version = $version
  commit = $commit
  jar = [IO.Path]::GetFileName($jar)
  sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $jar).Hash.ToLowerInvariant()
}
$receipt | ConvertTo-Json | Set-Content -Encoding utf8NoBOM (Join-Path $staging 'install-receipt.json')
py -3 -c "import runpy,sys; from pathlib import Path; api=runpy.run_path('.chaos-engine/hosts.py'); api.get('publish_maven_tools_cache')(Path(sys.argv[1]), root=Path(sys.argv[2]))" $staging $cacheRoot
```

The equivalent POSIX installation is:

```sh
version=3.2.0
commit=4475ff6c61f23ea9a93cb6d5665a63235ef2ef36
data_root=${XDG_DATA_HOME:-"$HOME/.local/share"}
cache_root="$data_root/ChaosEngine/tools/maven-tools-mcp"
mkdir -p "$cache_root"
staging=$(mktemp -d "$cache_root/.staging.XXXXXXXX")
jar="$staging/maven-tools-mcp-$version.jar"
cp "target/maven-tools-mcp-$version.jar" "$jar"
sha=$(python3 -c 'import hashlib,pathlib,sys; print(hashlib.sha256(pathlib.Path(sys.argv[1]).read_bytes()).hexdigest())' "$jar")
printf '{"version":"%s","commit":"%s","jar":"%s","sha256":"%s"}\n' \
  "$version" "$commit" "maven-tools-mcp-$version.jar" "$sha" \
  > "$staging/install-receipt.json"
python3 -c "import runpy,sys; from pathlib import Path; api=runpy.run_path('.chaos-engine/hosts.py'); api.get('publish_maven_tools_cache')(Path(sys.argv[1]), root=Path(sys.argv[2]))" "$staging" "$cache_root"
```


## Headroom (default-on max-savings companion)

ChaosEngine pins `headroom-ai==0.37.0` (Apache-2.0) and **auto-provisions** the
CLI on install (`uv tool install`). Defaults: `HEADROOM_SAVINGS_PROFILE=agent-90`,
beacon off. MemPalace and Graphify remain the memory SoT. Pass
`--without-headroom` to skip CLI provision. Repair with
`python3 .chaos-engine/install.py repair --project . --component headroom`.

```bash
eval "$(python3 .chaos-engine/headroom_policy.py export-env --token-budget ultra-lean)"
headroom doctor
HEADROOM_SAVINGS_PROFILE=agent-90 headroom wrap claude   # or proxy --port 8787
```

Ponytail XOR `HEADROOM_OUTPUT_SHAPER`: keep OUTPUT_SHAPER off while Ponytail is
active. See [references/headroom.md](references/headroom.md).

## Self-improve skill

Post-delivery Learning Session loads
[skills/self-improve/SKILL.md](skills/self-improve/SKILL.md) (CC BY 4.0
attribution for Task Observer methodology). It wraps `learning.py` privacy gates
for harness + product dual-track observations. Portable Stop / delivery-complete
hooks enforce the Learning Session after every confirmed delivery (including
`gh pr merge` and `delivery-status`) even when `chaos-engine/` was untouched.
Doctor surfaces the gate under `components.hooks.learningSession`. SessionStart
stays locator-only; the heavy protocol runs on the delivery Stop path (not an
always-on Task Observer). Harness parity: lasting policy lives in this overlay
so every supported host shares the same outcomes.
