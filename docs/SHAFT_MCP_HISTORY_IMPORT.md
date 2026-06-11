# SHAFT MCP History Import

This note records the history-preserving import requested by
[SHAFT_ENGINE issue 2849](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/2849).
The inventory was captured on 2026-06-11 before the import.

## Immutable provenance

| Item | Value |
| --- | --- |
| Source repository | `https://github.com/ShaftHQ/SHAFT_MCP` |
| Source default branch | `main` |
| Source commit | `5fb9e809ba428a85e0eaff415b5b3cbc79aca7ff` |
| Source tree | `28fa1693ca9af64fb1e9829c2e7984abe2c7016f` |
| Destination repository | `https://github.com/ShaftHQ/SHAFT_ENGINE` |
| Destination baseline | `96692ce32cbba2bf86bfb609a64bc278d352e7d8` |
| Import prefix | `shaft-mcp/` |
| Prefix commit | `8bbf8200482395e571e367d83a1d1a150095011a` |
| Import merge | `dd5e4436171ff0e5be88cca3c9566fd8816883e1` |

The prefix commit has the exact source commit as its parent and changes only
the path of the 41 tracked source files. The import merge has the destination
baseline and prefix commit as its two parents. Its commit message also records
the `git-subtree-dir`, `git-subtree-mainline`, and `git-subtree-split`
provenance.

The prefix commit is intentional. A direct subtree merge retains source
commits but makes `git log --follow -- shaft-mcp/<file>` stop at the prefix
boundary. Moving the source tree in one dedicated commit before the
non-squashed merge lets Git follow the rename into the original source
history.

The pull request must be merged with a merge commit. Squash or rebase merging
would discard the imported ancestry.

## Source repository state

At inventory time, `ShaftHQ/SHAFT_MCP` was public, active, and unarchived.
This migration does not archive it or change any source repository setting,
release, package, deployment, issue, or pull request.

Open issues: none.

Open pull requests:

| PR | Branch | Title |
| --- | --- | --- |
| [#110](https://github.com/ShaftHQ/SHAFT_MCP/pull/110) | `dependabot/maven/io.github.shafthq-SHAFT_ENGINE-10.2.20260610` | Bump `io.github.shafthq:SHAFT_ENGINE` from `10.2.20260506` to `10.2.20260610` |
| [#113](https://github.com/ShaftHQ/SHAFT_MCP/pull/113) | `dependabot/maven/org.junit.jupiter-junit-jupiter-engine-6.1.0` | Bump `junit-jupiter-engine` from `6.0.3` to `6.1.0` |
| [#114](https://github.com/ShaftHQ/SHAFT_MCP/pull/114) | `dependabot/maven/org.apache.maven.surefire-surefire-testng-3.5.6` | Bump `surefire-testng` from `3.5.5` to `3.5.6` |

The fetched source branch inventory also contained `main`, six
`smithery/patch-*` branches, two `sync-versions-to-*` branches, and the three
Dependabot branches above.

## Tags and releases

All 18 source tags are lightweight commit tags. Each has a corresponding
published GitHub release.

| Tag and release | Commit | Published |
| --- | --- | --- |
| `9.3.20250822` | `8e97a0fbc443f51012174a19a332cf4ca24733f4` | 2025-08-22 |
| `9.3.20250823` | `ddd2da0ebd41f3c3c89ca875effa44375061b42c` | 2025-08-23 |
| `9.3.20250824` | `6180e18ddaa945aa812c218f6f3a28e9d62c04cf` | 2025-08-24 |
| `9.3.20250928` | `6396535de52540d08af65b5e1124d36fe600e798` | 2025-09-29 |
| `9.4.20251007` | `c69c7fce29c80a2f4703994d3af8ac556d0d40c6` | 2025-10-07 |
| `9.4.20251022` | `a855b931c481092da7797bb0b05e82ad5ed85fc7` | 2025-10-28 |
| `9.4.20251028` | `7609d257bc75fb1611ab07c21a591933d35906ad` | 2025-10-28 |
| `9.4.20251108` | `5c7b9ec90e73f062420cf6d3c7ffce9b08522b38` | 2025-11-08 |
| `9.4.20251116` | `9d0e5c1eb28902100d67fd953c2e16654dd8d829` | 2025-11-16 |
| `10.1.20260312` | `2db96f2bde7fc9bc3b577e6841cf3805210784a3` | 2026-03-14 |
| `10.1.20260315` | `539e77d880c505eafd8c9fded9bdcfb94ca8017f` | 2026-03-15 |
| `10.1.20260319` | `facc09956ad5481f1ebb7a51ca008c06e16cfb35` | 2026-03-19 |
| `10.1.20260324` | `5ba7e07aa152c33e4f5bcc9c1264865490f5c1e6` | 2026-03-26 |
| `10.1.20260331` | `bf62543ad896e30650b33408301bc59dcc43e985` | 2026-04-09 |
| `10.2.20260422` | `988b1beca8509d73372063f4cc6a1cc7f5ed753e` | 2026-04-22 |
| `10.2.20260424` | `17b2d9fbc7951648f958f9a5c6e6305483b0a286` | 2026-05-02 |
| `10.2.20260501` | `72d70955fac36a17936cc06c2be25ade319d11a5` | 2026-05-05 |
| `10.2.20260506` | `20f7166e511bd90c1ce656b1adeeebf92b482757` | 2026-06-10 |

The source tags were fetched into isolated local refs under
`refs/tags/shaft-mcp-source/`. No existing SHAFT_ENGINE tag was changed, and
source tags are not pushed by this pull request.

## Publication inventory

Maven Central:

- Coordinates: `io.github.shafthq:SHAFT_MCP`
- Metadata:
  `https://repo1.maven.org/maven2/io/github/shafthq/SHAFT_MCP/maven-metadata.xml`
- Published versions: `9.3.20250823`, `9.3.20250824`, `9.3.20250928`,
  `9.4.20251007`, `9.4.20251022`, `9.4.20251028`, `9.4.20251108`,
  `9.4.20251116`, `10.1.20260312`, `10.1.20260315`, `10.1.20260319`,
  `10.1.20260324`, `10.1.20260331`, `10.2.20260422`, `10.2.20260424`,
  `10.2.20260501`, and `10.2.20260506`.

GitHub Container Registry:

- Image: `ghcr.io/shafthq/shaft-mcp`
- Anonymous registry inventory: `9.4.20251116`, `10.1.20260312`,
  `10.1.20260315`, `10.1.20260319`, `10.1.20260324`, `10.1.20260331`,
  `10.2.20260422`, `10.2.20260424`, `10.2.20260501`, `10.2.20260506`, and
  `latest`.
- The GitHub Packages REST inventory could not be queried because the
  authenticated token lacked `read:packages`; the public OCI registry was
  queried directly instead.

Model Context Protocol Registry:

- Server name: `io.github.ShaftHQ/shaft-mcp`
- Registry API:
  `https://registry.modelcontextprotocol.io/v0.1/servers?search=io.github.ShaftHQ%2Fshaft-mcp`
- Active versions: `9.4.20251116`, `10.1.20260312`, `10.1.20260315`,
  `10.1.20260319`, `10.1.20260324`, `10.1.20260331`, `10.2.20260422`,
  `10.2.20260424`, `10.2.20260501`, and `10.2.20260506`.
- Latest registry version at inventory time: `10.2.20260506`.
- Imported metadata: `shaft-mcp/server.json`.

Hosted deployments:

- Smithery listing:
  `https://smithery.ai/server/@ShaftHQ/shaft-mcp`
- Smithery configuration: `shaft-mcp/smithery.yaml`,
  `shaft-mcp/Dockerfile.smithery`, and
  `shaft-mcp/Dockerfile.smithery.build`
- Render service endpoint: `https://shaft-mcp.onrender.com/mcp`
- Render configuration: `shaft-mcp/render.yaml` and
  `shaft-mcp/Dockerfile.render`
- Fly.io service endpoint: `https://shaft-mcp.fly.dev/mcp`
- Fly.io configuration: `shaft-mcp/fly.toml` and
  `shaft-mcp/Dockerfile.fly`

Documentation and repository links:

- Source README:
  `https://github.com/ShaftHQ/SHAFT_MCP/blob/main/readme.md`
- Deployment guide:
  `https://github.com/ShaftHQ/SHAFT_MCP/blob/main/SMITHERY_DEPLOYMENT.md`
- SHAFT documentation: `https://shafthq.github.io/`
- Source issues: `https://github.com/ShaftHQ/SHAFT_MCP/issues`

## Secrets and variables

Only names were inventoried. No secret value was read, printed, copied, or
committed.

The authenticated GitHub API returned no configured repository Actions,
Dependabot, Codespaces, or environment secret names. Source workflows refer
to these secret names:

- `GITHUB_TOKEN` (GitHub-provided token)
- `GPG_KEYNAME`
- `GPG_PASSPHRASE`
- `GPG_PRIVATE_KEY`
- `OSSRH_USERNAME`
- `OSSRH_PASSWORD`
- `RENDER_DEPLOY_HOOK_URL`
- `FLY_API_TOKEN`

Repository variable names returned by the API:

- `COPILOT_AGENT_FIREWALL_ALLOW_LIST_ADDITIONS`
- `COPILOT_AGENT_FIREWALL_ENABLED`

Repository environments returned by the API were `copilot` and `production`;
neither returned environment secret or variable names.

## Verification

The normalized source and imported manifests were compared with:

```powershell
$source = git ls-tree -r --full-tree 5fb9e809ba428a85e0eaff415b5b3cbc79aca7ff
$imported = git ls-tree -r --full-tree HEAD:shaft-mcp
Compare-Object $source $imported
```

Result: zero differences. Both trees resolve to
`28fa1693ca9af64fb1e9829c2e7984abe2c7016f`, so file names, modes, and blob
hashes are identical after removing only the `shaft-mcp/` prefix.

History verification:

```bash
git log --follow -- shaft-mcp/pom.xml
git show -s --format='%H%n%P%n%B' dd5e4436171ff0e5be88cca3c9566fd8816883e1
git show -s --format='%H%n%P%n%B' 8bbf8200482395e571e367d83a1d1a150095011a
```

The source contains 112 commits reachable from the imported source SHA.
Original commits and authors remain reachable through the prefix commit.

Secret scanning:

- GitHub secret scanning API: zero open alerts.
- Gitleaks `8.30.1`, redacted directory scan of `shaft-mcp/`: zero findings.

Build and repository validation:

- `mvn clean install "-DskipTests" "-Dgpg.skip"`: success for all eight
  existing SHAFT_ENGINE reactor modules.
- `mvn -f shaft-mcp/pom.xml clean package "-DskipTests" "-Dgpg.skip"`:
  success for the imported standalone project. Tests were skipped and the
  existing source emitted 23 non-fatal JavaDoc warnings.
- `scripts/ci/validate_agent_guidance.py`: success.
- `git diff --check` for this provenance document: success. The untouched
  imported source contains pre-existing trailing whitespace, which is retained
  to preserve exact tree parity.

The imported `shaft-mcp/LICENSE` remains the source MIT license. No generated
artifacts, repository caches, or local credentials are part of the imported
tree.

## Rollback

Before merge, close the pull request and delete its branch. No source
repository state needs restoration.

After a merge commit lands on `main`, revert the import merge without
rewriting history:

```bash
git revert -m 1 dd5e4436171ff0e5be88cca3c9566fd8816883e1
```

Revert the later provenance-note commit separately if the documentation should
also be removed. Do not reset or force-push `main`. The source repository and
all existing distribution endpoints remain the operational rollback source
until the final SHAFT Pilot release ticket completes.
