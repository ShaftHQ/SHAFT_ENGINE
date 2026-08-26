# ChaosEngine research and adoption matrix

Accessed: 2026-08-26. Primary specifications and first-party product guidance
were checked live. Each adoption points to its local proof owner; no third-party
code was copied by this review.

| Rank | Primary source | Pattern or tool | Decision | ChaosEngine action and proof |
| --- | --- | --- | --- | --- |
| 1 | [Agent Skills specification](https://agentskills.io/specification) | Progressive disclosure: small metadata, focused `SKILL.md`, on-demand references and scripts | Adopted | The canonical `skills/chaos-engine/SKILL.md` routes to focused `references/`; portability and reachability tests enforce the boundary. |
| 2 | [Agent Plugins specification](https://agent-plugins.org/specification) | One manifest, fixed discovery locations, root containment, symlink/reparse rejection, and narrow component failure boundaries | Adopted | `install.py` and `hosts.py` use manifest-bound ownership, contained relative paths, native adapters, and fail-closed collision tests. |
| 3 | [MCP specification](https://modelcontextprotocol.io/specification/2025-06-18/index) | Capability negotiation plus explicit consent, privacy, and tool-safety boundaries | Adopted | Project-local MCP launchers are explicit; `learning.py` requires confirmation and blocks raw/private material before network access. |
| 4 | [Claude Code skills](https://code.claude.com/docs/en/skills) | Skills load only when relevant; record stable run knowledge only after it proves useful; evaluate and iterate | Adopted | The router uses progressive disclosure, TDD, live-flow verification, and the quarantined `learning.py` queue rather than transcript-driven mutation. |
| 5 | [OpenAI skill guidance](https://learn.chatgpt.com/docs/build-skills) | Focus one skill on one job, front-load triggers, prefer instructions, and use scripts for deterministic behavior | Adopted | One canonical router owns policy; deterministic lifecycle work is implemented in `install.py`, `dependencies.py`, `hosts.py`, and `learning.py`. |
| 6 | [Gemini CLI skills](https://geminicli.com/docs/cli/skills/) | Repository skill discovery with explicit enablement and inspectable local files | Retained | `.gemini/skills/chaos-engine/SKILL.md` and `.gemini/settings.json` are thin receipt-owned adapters verified by `test_chaos_engine_hosts.py`. |
| 7 | [GitHub Copilot custom instructions](https://docs.github.com/en/copilot/how-tos/configure-custom-instructions-in-your-ide/add-repository-instructions-in-your-ide) | Keep always-on repository facts short; use nearest `AGENTS.md` and skills for detailed procedures | Adopted | `AGENTS.md`/Copilot files only redirect; detailed policy remains in the canonical installed skill and selected project profile. |
| 8 | [uv tool management](https://docs.astral.sh/uv/concepts/tools/) | Account-scoped isolated tools, explicit upgrades, and uv-managed Python | Adopted | `dependencies.py` resolves latest stable releases, uses `uv tool install`/`upgrade`, installs exact resolved Python through uv, and records absolute account executables. |
| 9 | [SLSA 1.2](https://slsa.dev/spec/v1.2/) | Resolve mutable source to immutable provenance and verify the installed artifact against it | Adopted | Latest-main installation records the resolved 40-character commit and per-file SHA-256 ownership in `.chaos-engine/manifest.json`. |
| 10 | [GitHub issue CLI](https://cli.github.com/manual/gh_issue_create) | Make reviewable, minimal upstream contributions through issues with explicit repository selection | Adopted | `learning.py` deduplicates by a privacy-safe digest, asks with an estimated token cost, queues offline/auth failures, and can create an issue; it never opens a PR. |

## DeepSeek Harness adoption — Accessed: 2026-08-15

Primary source: [DeepSeek Harness architecture at commit
`47f943859bef60e4160492346772ded9b24f765a`](https://github.com/deepseek-ai/deepseek-harness/blob/47f943859bef60e4160492346772ded9b24f765a/docs/architecture.md).

| Capability | Decision | ChaosEngine ownership |
| --- | --- | --- |
| Capability ownership | Adopted | Keep service definition, provider, and consumer responsibilities explicit in `distributions.json`, profile, and dependency component descriptors. |
| Declarative composition | Adopted | Continue composing one canonical entrypoint with profiles and thin host adapters. |
| Orthogonal outcomes | Adopted | Report task delivery, cleanup, and each knowledge-store outcome independently. |
| Bounded asynchronous behavior | Adopted | Bound optional retrieval to one attempt and keep maintenance with its existing owner. |
| Quiescent teardown | Adopted | Delivery and cleanup receipts prove owned processes/resources are stopped or narrowly degraded. |

Rejected runtime scope: the duplicate Node runtime, agent loop, session log,
goals/todos, and “everything is a plugin” overhead. Cordis offers a strong
replaceable plugin ecosystem with reversible effects and one event model, but
importing it would create a second execution and persistence stack without a
current consumer. ChaosEngine adopts the architectural invariants, not the
preview runtime.

## Token-first adoption — Accessed: 2026-08-17

Primary sources checked live: [Agent Skills specification](https://agentskills.io/specification),
[Anthropic context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents),
and DeepSeek Harness [compaction](https://github.com/deepseek-ai/deepseek-harness/blob/master/docs/subsystems/compaction.md)
plus code mode. Companion bodies stay MIT-pinned. ChaosEngine selects ultra
and loads them at task start on every host; they are not optional at runtime.

| Pattern | Decision | ChaosEngine ownership |
| --- | --- | --- |
| Progressive disclosure | Adopted | Always-on router; references load when their decision is active. |
| Just-in-time retrieval | Adopted | Query a store only when it can shorten the task; one bounded attempt. |
| Tool-result prune and spill | Adopted as guidance | [context-economy](references/context-economy.md) |
| Script over long tool chains | Adopted as guidance | [script-first](references/script-first.md) |
| Auto-load companion bodies every mutation | Rejected | SessionStart injects each vendor `SKILL.md` once. Do not re-inject on every tool. Companions stay active at ultra until off. |

## Architecture decision

### Selected approach

Use a standard-library, repository-local, latest-main core installer. Resolve
the configured upstream branch to an immutable commit, stage and verify the
core, publish thin native adapters, provision upstream tools for the invoking
account, and keep self-improvement local until a user explicitly approves a
minimal issue. Dependency activation requires successful latest-stable
resolution from each official channel; stale versions never activate merely
because they remain locally runnable.

### Marketplace-only alternative

Native marketplaces provide the smallest install UX and delegate cache/update
logic to each client. They remain useful distribution references, but they do
not provide one five-host ownership record, safe migration of mixed existing
guidance, a shared dependency doctor, or cross-client rollback. They are not
the canonical ChaosEngine delivery path.

### Private-runtime alternative

A receipt-owned private Python/uv/Node/Java tree was rejected because plugin
launchers and ordinary account commands cannot reliably discover it. Standard
upstream account installations are reused or upgraded only after version and
functional probes. Project rollback restores ChaosEngine configuration but
never removes account packages it does not own.

## Freshness policy

This matrix is evidence, not an auto-update feed. Recheck primary sources when
their relevant specification date, schema, discovery path, or installer
contract changes. Candidate code remains quarantined and license-reviewed;
patterns are reimplemented locally only when a RED test proves a gap.
