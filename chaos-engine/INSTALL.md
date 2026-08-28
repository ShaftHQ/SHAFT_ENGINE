<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/brand/symbol-dark.svg">
    <source media="(prefers-color-scheme: light)" srcset="assets/brand/symbol-light.svg">
    <img alt="ChaosEngine symbol" src="assets/brand/symbol-light.svg" width="180">
  </picture>
</p>

# Install or upgrade ChaosEngine

This is the operational reference for installing, verifying, upgrading,
recovering, and removing ChaosEngine. For its purpose and trust model, start
with the [README](README.md). The canonical agent contract lives in
[`skills/chaos-engine/SKILL.md`](skills/chaos-engine/SKILL.md).

Give the following instruction to a coding agent while its working directory is
the project you want to manage:

> Install or upgrade ChaosEngine in this project from the latest commit of the
> official upstream. Change into the target project first. Run
> `chaos-engine/install.ps1` on Windows (`irm ... | iex`) or
> `chaos-engine/install.sh` on macOS/Linux (`curl -fsSL ... | bash -s -- <url>`).
> Those wrappers read owner/repository from the invocation URL, then download
> `bootstrap.py` and run the full install: hooks, skills,
> companions, Memory, MemPalace, Graphify CLI, and doctor. Do not stop until
> the active doctor reports the resolved 40-character commit and every
> required component healthy. Treat the installed ChaosEngine skill as the
> canonical harness and route any existing agent guidance through it without
> deleting unrelated user content.

The bootstrap installs the neutral core, routed guidance, host adapters,
lifecycle hooks, pinned companion skills, and selected local tools. It then
runs active doctor probes. Generated indexes, caches, receipts, and runtimes
remain untracked; canonical configuration and adapters remain trackable.

## Install

The Windows example below uses an `owner/repository` placeholder; replace it
with the upstream that hosts the wrapper. The macOS/Linux example constructs
the official upstream URL without embedding source identity in the portable
payload. The scripts parse their invocation URL and do not copy that identity
into the adopter payload. The installer also merges receipt-bound LF attributes
for canonical harness paths, so Windows Git checkouts retain exact owned bytes
while unrelated `.gitattributes` rules remain untouched.
`CHAOS_ENGINE_REPOSITORY` remains a local-file
override when the invocation URL cannot be parsed. Change into the target
project or folder first; both scripts install into the current working
directory.

Windows PowerShell, using [install.ps1](install.ps1):

```powershell
irm "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.ps1" | iex
```

macOS or Linux, using [install.sh](install.sh):

```bash
curl -fsSL "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh" | bash -s -- "https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/chaos-engine/install.sh"
```

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
url="https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh"; curl -fsSL "$url" | bash -s -- "$url" --with-maven-tools
```

PowerShell:

```powershell
$installer = irm "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.ps1"; & ([scriptblock]::Create($installer)) -WithMavenTools
```

Inspect the linked installer and [bootstrap.py](bootstrap.py) first when policy
requires review before execution. The bootstrap resolves the default branch to
an immutable commit and downloads only its validated `chaos-engine/` subtree;
`portable` is already the default and need not be supplied. Restart any client
that was open during installation so it loads its verified local plugin cache.

## Verify and operate

Installation is complete only when the active doctor reports the resolved
40-character commit and every required selected component healthy.

```text
python .chaos-engine/install.py status --project . --json
python .chaos-engine/install.py doctor --project . --json
python .chaos-engine/install.py explain Stop --project . --host codex --json
python .chaos-engine/install.py rollback --project .
python .chaos-engine/install.py uninstall --project .
```

- **Upgrade:** run the platform one-liner again. A failed candidate leaves the
  last verified installation active.
- **Legacy install:** if status reports `legacy`, uninstall first, then run the
  portable bootstrap. In-place legacy conversion is intentionally refused.
- **Rollback or uninstall:** only receipt-owned files are changed. Mixed,
  modified, linked, or unknown ownership fails closed.
- **Interrupted rollback:** the next install resumes an authenticated rollback
  intent under the installer lock before starting the requested update. If
  recovery cannot be authenticated, inspect `status --json`, then run
  `python3 .chaos-engine/install.py rollback --project .` (`py -3` on Windows).

## Advanced provenance, recovery, and store states

<details>
<summary><strong>Show branch resolution, repair, ownership, and knowledge-store details</strong></summary>

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
`lifecycle`, and `taskImpact`. Memory, MemPalace, and Graphify are advisory to
ordinary tasks but remain strict in `doctor`; an unhealthy selected store still
returns `recovery-required`. Maven Tools MCP is auto-installed when the project
has a root `pom.xml`. On non-Maven projects it stays optional and absent does
not make project health fail.

</details>

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

<details>
<summary><strong>Manual receipt publication after a source build</strong></summary>

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

</details>
