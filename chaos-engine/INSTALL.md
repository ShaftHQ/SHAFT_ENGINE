<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/brand/symbol-dark.svg">
    <source media="(prefers-color-scheme: light)" srcset="assets/brand/symbol-light.svg">
    <img alt="ChaosEngine symbol" src="assets/brand/symbol-light.svg" width="180">
  </picture>
</p>

# Install or upgrade ChaosEngine

ChaosEngine is a portable, provider-neutral working contract for software
agents. It routes work through research, planning, focused playbooks, empirical
verification, independent adversarial review, and a durable learning session.

This page is the direct installation reference. Start with the human-facing
[`README.md`](README.md) for the purpose, operating loop, trust boundaries, and
portable layout. The canonical operating model lives in
[`skills/chaos-engine/SKILL.md`](skills/chaos-engine/SKILL.md), and the reusable
vector masters and application rules live in the [identity guide](assets/brand/BRAND.md).

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

That agent instruction owns the complete flow: the bootstrap installs the
neutral core, pinned local tools, Memory and isolated MemPalace MCP servers,
Graphify CLI, skills, playbooks, five role adapters, ChaosEngine lifecycle
hooks, the pinned Caveman and Ponytail companion skills and hooks, the MIT license
and third-party notices, Codex and Claude plugin manifests/marketplaces for
ChaosEngine plus those companions, retrieval configuration, and runtime ignore
rules. Companion skills install with the core and load by default at runtime;
user off-switches still win. When a detected client
requires marketplace registration, the agent registers the project marketplace
and installs `chaos-engine`, `caveman`, and `ponytail` at project local scope,
then runs active `doctor` probes. Generated indexes, caches, receipts, and runtimes
remain untracked; canonical configuration and adapters remain trackable.
Origin identity masters under `assets/brand/`, the origin adoption matrix
`RESEARCH.md`, and `STANDALONE.md` stay in the source tree and are not copied
into the adopter payload. The installer also merges receipt-bound LF attributes for canonical harness paths,
so Windows Git checkouts retain the exact owned bytes while unrelated
`.gitattributes` rules remain untouched.

Replace `owner/repository` in the URL with the upstream that hosts the wrapper.
The scripts parse that URL and do not copy the source identity into the adopter
payload. `CHAOS_ENGINE_REPOSITORY` remains a local-file override when the
invocation URL cannot be parsed. Change into the target project or folder first;
both scripts install into the current working directory.

Windows PowerShell, using [install.ps1](install.ps1):

```powershell
irm "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.ps1" | iex
```

macOS or Linux, using [install.sh](install.sh):

```bash
curl -fsSL "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh" | bash -s -- "https://raw.githubusercontent.com/owner/repository/main/chaos-engine/install.sh"
```

Inspect the linked installer and [bootstrap.py](bootstrap.py) first when policy
requires review before execution. The bootstrap resolves the default branch to
an immutable commit and downloads only its validated `chaos-engine/` subtree;
`portable` is already the default and need not be supplied. Restart any client
that was open during installation so it loads its verified local plugin cache.

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
returns `recovery-required`. The optional Maven Tools MCP cache is user-owned
and does not make project health fail.

## Optional native Maven Tools MCP

Do not put `docker run -i --rm` in a default stdio MCP configuration. Each
active client owns its own stdio server process, so a Docker-backed declaration
creates one container per client and keeps Docker Desktop and its VM resident.

When Maven Tools MCP is wanted, the installing agent must use the upstream
native JAR flow instead:

1. Resolve a real Java 25 executable from `CHAOSENGINE_JAVA`, `JAVA_HOME`, or
   `PATH`; never write a copied example path.
2. Clone `https://github.com/arvindand/maven-tools-mcp.git`, check out detached
   commit `4475ff6c61f23ea9a93cb6d5665a63235ef2ef36`, and run `mvnw.cmd clean
   verify -Pfull` on Windows or `./mvnw clean verify -Pfull` elsewhere. The
   wrapper supplies Maven; Java 25 and Git are prerequisites. Upstream does not
   publish a JAR release asset, so do not invent a download URL or silently
   fall back to Docker.
3. Stage `maven-tools-mcp-3.2.0.jar` under a fresh unique directory on the same
   filesystem as the current user's data directory, then publish that directory
   with a no-overwrite rename to
   `ChaosEngine/tools/maven-tools-mcp/3.2.0/`. On Windows the data directory is
   `%LOCALAPPDATA%`; elsewhere it is `$XDG_DATA_HOME`, or `~/.local/share` when
   that variable is unset. `CHAOSENGINE_MAVEN_TOOLS_MCP_JAR` may name a
   different verified JAR. Beside it, write `install-receipt.json` with exactly
   these keys: `version` = `3.2.0`, `commit` = the pinned commit above, `jar` =
   the installed filename, and `sha256` = the lowercase SHA-256 of its bytes.
   Discovery recomputes the digest and rejects a missing, malformed, stale, or
   differently pinned receipt. The version directory is an immutable,
   user-managed cache: parallel projects may read the verified pair, while
   project uninstall never changes or removes it.
4. Run the ChaosEngine bootstrap again. Host installation discovers both files
   and atomically rewrites `.mcp.json`, `.gemini/settings.json`, and
   `.codex/config.toml` with their resolved absolute paths. Upgrades repeat
   discovery, so another user's Java or data path is never inherited.
5. Start a fresh client session and prove both the MCP initialize and tools/list
   responses. The JAR launch includes
   `--spring.profiles.active=docker,no-context7`: upstream's `docker` profile
   enables clean stdio without starting or requiring Docker, while
   `no-context7` keeps the receipt-pinned server's native tool surface independent
   of a live downstream Context7 connection.

If Java 25 or the verified JAR is absent, installation leaves this optional MCP
server out of every host configuration. Maven CLI, repository files, Context7,
and authoritative Maven Central sources remain the no-Docker fallback.

Inspect or remove the exact supported cache version with:

```text
python .chaos-engine/install.py cache status --component maven-tools-mcp
python .chaos-engine/install.py cache purge --component maven-tools-mcp --version 3.2.0
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
