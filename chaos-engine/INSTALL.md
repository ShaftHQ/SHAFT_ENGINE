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
verification, independent adversarial review, and a durable learning loop.

This page is the direct installation reference. Start with the human-facing
[`README.md`](README.md) for the purpose, operating loop, trust boundaries, and
portable layout. The canonical operating model lives in
[`skills/chaos-engine/SKILL.md`](skills/chaos-engine/SKILL.md), and the reusable
vector masters and application rules live in the [identity guide](assets/brand/BRAND.md).

Give the following single command to Codex, Claude, Grok, Gemini, or another
coding agent while its working directory is the project you want to manage:

> Install or upgrade ChaosEngine in this project from the latest commit of the
> configured upstream. Fetch and inspect that upstream's
> `chaos-engine/bootstrap.py`, run it with Python 3, `--project .`, and the
> explicit `--repository owner/repository`; then run Python 3 with
> `.chaos-engine/install.py status --project .`. Do not stop until the command reports the resolved
> 40-character commit and healthy core, host adapters, and local tools. Treat
> the installed ChaosEngine skill as the canonical harness and route any
> existing agent guidance through it without deleting unrelated user content.

The command is agent-oriented so the agent selects the available Python 3
executable on Windows, macOS, or Linux and can report a blocked network or
authentication boundary. For a direct terminal flow, save the bootstrap and
run:

```text
python bootstrap.py --project . --repository owner/repository
```

Add `--branch branch` to override the repository's configured default branch. The
bootstrap resolves that mutable branch through the GitHub API, downloads the
exact commit archive, rejects unsafe archive entries, and records repository,
branch, and commit provenance in `.chaos-engine/manifest.json`. Re-running the
same command upgrades to the latest resolved commit; an offline or invalid
download leaves the last verified installation unchanged.

The consumer folder may be a GitHub checkout, another Git checkout, or a
non-Git directory. ChaosEngine installs project-locally and does not infer its
upstream from the consumer repository.

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
3. Put `maven-tools-mcp-3.2.0.jar` under the current user's data directory at
   `ChaosEngine/tools/maven-tools-mcp/3.2.0/`. On Windows the data directory is
   `%LOCALAPPDATA%`; elsewhere it is `$XDG_DATA_HOME`, or `~/.local/share` when
   that variable is unset. `CHAOSENGINE_MAVEN_TOOLS_MCP_JAR` may name a
   different verified JAR. Beside it, write `install-receipt.json` with exactly
   these keys: `version` = `3.2.0`, `commit` = the pinned commit above, `jar` =
   the installed filename, and `sha256` = the lowercase SHA-256 of its bytes.
   Discovery recomputes the digest and rejects a missing, malformed, stale, or
   differently pinned receipt.
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

An installing agent can use this PowerShell sequence after the source build:

```powershell
$version = '3.2.0'
$commit = '4475ff6c61f23ea9a93cb6d5665a63235ef2ef36'
$toolDir = Join-Path $env:LOCALAPPDATA "ChaosEngine\tools\maven-tools-mcp\$version"
$jar = Join-Path $toolDir "maven-tools-mcp-$version.jar"
New-Item -ItemType Directory -Force -Path $toolDir | Out-Null
Copy-Item -LiteralPath "target\maven-tools-mcp-$version.jar" -Destination $jar
$receipt = [ordered]@{
  version = $version
  commit = $commit
  jar = [IO.Path]::GetFileName($jar)
  sha256 = (Get-FileHash -Algorithm SHA256 -LiteralPath $jar).Hash.ToLowerInvariant()
}
$receipt | ConvertTo-Json | Set-Content -Encoding utf8NoBOM (Join-Path $toolDir 'install-receipt.json')
```

The equivalent POSIX installation is:

```sh
version=3.2.0
commit=4475ff6c61f23ea9a93cb6d5665a63235ef2ef36
data_root=${XDG_DATA_HOME:-"$HOME/.local/share"}
tool_dir="$data_root/ChaosEngine/tools/maven-tools-mcp/$version"
jar="$tool_dir/maven-tools-mcp-$version.jar"
mkdir -p "$tool_dir"
cp "target/maven-tools-mcp-$version.jar" "$jar"
sha=$(python3 -c 'import hashlib,pathlib,sys; print(hashlib.sha256(pathlib.Path(sys.argv[1]).read_bytes()).hexdigest())' "$jar")
printf '{"version":"%s","commit":"%s","jar":"%s","sha256":"%s"}\n' \
  "$version" "$commit" "maven-tools-mcp-$version.jar" "$sha" \
  > "$tool_dir/install-receipt.json"
```
