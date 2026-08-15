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

Give the following instruction to a coding agent while its working directory is
the project you want to manage:

> Install or upgrade ChaosEngine in this project from the latest commit of the
> official upstream. Fetch and inspect that upstream's
> `chaos-engine/bootstrap.py`, run it with Python 3, `--project .`, and the
> explicit repository containing this guide. Do not stop until the bootstrap's
> active doctor reports the resolved 40-character commit and every required
> component healthy. Treat
> the installed ChaosEngine skill as the canonical harness and route any
> existing agent guidance through it without deleting unrelated user content.

That agent instruction owns the complete flow: the bootstrap installs the
neutral core, pinned local tools, Memory and isolated MemPalace MCP servers,
Graphify CLI, skills, playbooks, five role adapters, lifecycle hooks, Codex and
Claude plugin manifests/marketplaces, retrieval configuration, and runtime
ignore rules. When a detected client requires marketplace registration, the
agent registers the project marketplace and installs `chaos-engine` at project
local scope, then runs active `doctor` probes. Generated indexes, caches, receipts, and runtimes
remain untracked; canonical configuration and adapters remain trackable. The
installer also merges receipt-bound LF attributes for canonical harness paths,
so Windows Git checkouts retain the exact owned bytes while unrelated
`.gitattributes` rules remain untouched.

For a literal one-command terminal flow from the adopter project, use this on
Windows PowerShell:

```powershell
py -3 -c "import email.utils,pathlib,runpy,sys,tempfile,time,urllib.error,urllib.request; o='S'+'haftHQ'; r='S'+'HAFT_ENGINE'; repo=f'{o}/{r}'; d=tempfile.TemporaryDirectory(prefix='chaos-engine-bootstrap-'); p=pathlib.Path(d.name)/'bootstrap.py'; ns={'url':f'https://raw.githubusercontent.com/{repo}/main/chaos-engine/bootstrap.py','retry_header':'Retry-After','transient':{408,425,429,500,502,503,504}}; exec('for attempt in range(4):\n delay=2**attempt\n try:\n  with urllib.request.urlopen(url,timeout=30) as response: content=response.read()\n  break\n except urllib.error.HTTPError as error:\n  retry_after=error.headers.get(retry_header) if error.headers is not None else None\n  error.close()\n  if (error.code not in transient and not (error.code==403 and retry_after is not None)) or attempt==3: raise\n  if retry_after is not None:\n   try: delay=float(retry_after)\n   except ValueError: delay=max(0,email.utils.parsedate_to_datetime(retry_after).timestamp()-time.time())\n   if not 0<=delay<=60: raise\n  elif error.code==429: delay=60\n except (ConnectionError,TimeoutError,urllib.error.URLError):\n  if attempt==3: raise\n time.sleep(delay)',globals(),ns); p.write_bytes(ns['content']); sys.argv=[str(p),'--project','.','--repository',repo]; runpy.run_path(str(p),run_name='__main__')"
```

On macOS or Linux, replace `py -3` with `python3`. Inspect the
bootstrap source in this upstream repository first when policy requires review
before execution. The temporary bootstrap resolves the default branch to an
immutable commit and downloads only its validated `chaos-engine/` subtree;
`portable` is already the default and need not be supplied. Restart any client that was open during
installation so it loads its verified local plugin cache.

Add `--branch branch` to override the repository's configured default branch. The
bootstrap resolves that mutable branch through the GitHub API, downloads the
exact commit's declared harness files, rejects unsafe tree entries, and records repository,
immutable provenance digests and the commit in `.chaos-engine/manifest.json`.
The public default is always the neutral `portable` distribution; a source
repository's contributor profile requires an explicit non-default selection. Re-running the
same command upgrades to the latest resolved commit; an offline or invalid
download leaves the last verified installation unchanged. The bootstrap retries
transient timeout, connection, rate-limit, and server responses with bounded
backoff, while permanent client errors fail immediately.

Installations created before distribution-bound manifests are reported as
`legacy`. To prevent repository-specific content from surviving in a backup,
convert them with an explicit uninstall followed by the portable bootstrap.
The installer refuses an in-place legacy conversion and leaves the old tree
unchanged.

The consumer folder may be a GitHub checkout, another Git checkout, or a
non-Git directory. ChaosEngine installs project-locally and does not infer its
upstream from the consumer repository.

`doctor` reports each required component independently: core, projection
policy, skills, playbooks, hooks, plugins, roles, MCPs, retrieval config, tools,
Memory, MemPalace, and Graphify. Any missing selected component prevents a
healthy verdict.

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
