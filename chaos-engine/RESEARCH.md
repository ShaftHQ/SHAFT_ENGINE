# ChaosEngine research and adoption matrix

Accessed: 2026-08-12. Primary specifications and first-party product guidance
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
| 8 | [uv tool management](https://docs.astral.sh/uv/concepts/tools/) | Isolated tool environments, explicit upgrades, and reinstall when constraints change | Adopted | `dependencies.py` owns a project-local uv runtime, typed receipts, doctor/freshness checks, atomic repair, and rollback. |
| 9 | [SLSA 1.2](https://slsa.dev/spec/v1.2/) | Resolve mutable source to immutable provenance and verify the installed artifact against it | Adopted | Latest-main installation records the resolved 40-character commit and per-file SHA-256 ownership in `.chaos-engine/manifest.json`. |
| 10 | [GitHub issue CLI](https://cli.github.com/manual/gh_issue_create) | Make reviewable, minimal upstream contributions through issues with explicit repository selection | Adopted | `learning.py` deduplicates by a privacy-safe digest, asks with an estimated token cost, queues offline/auth failures, and can create an issue; it never opens a PR. |

## DeepSeek Harness adoption — Accessed: 2026-08-15

Primary source: [DeepSeek Harness architecture at commit
`47f943859bef60e4160492346772ded9b24f765a`](https://github.com/deepseek-ai/deepseek-harness/blob/47f943859bef60e4160492346772ded9b24f765a/docs/architecture.md).

| Capability | Decision | ChaosEngine ownership |
| --- | --- | --- |
| Capability ownership | Adopted | Keep service definition, provider, and consumer responsibilities explicit in validated component descriptors. |
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

## Architecture decision

### Selected approach

Use a standard-library, repository-local, latest-main installer. Resolve the
configured upstream branch to an immutable commit, stage and verify the core,
publish thin native adapters, provision receipt-owned local tools, and keep
self-improvement local until a user explicitly approves a minimal issue.

### Marketplace-only alternative

Native marketplaces provide the smallest install UX and delegate cache/update
logic to each client. They remain useful distribution references, but they do
not provide one five-host ownership record, safe migration of mixed existing
guidance, a shared dependency doctor, or cross-client rollback. They are not
the canonical ChaosEngine delivery path.

### Global-tool alternative

A globally installed Python or uv command would simplify bootstrap and avoid
repository copies. It was rejected because global state is not project-owned,
can select the wrong version or root, and cannot make adapter/config rollback
atomic with the installed core. The project-local runtime preserves isolation
and relocation.

## Freshness policy

This matrix is evidence, not an auto-update feed. Recheck primary sources when
their relevant specification date, schema, discovery path, or installer
contract changes. Candidate code remains quarantined and license-reviewed;
patterns are reimplemented locally only when a RED test proves a gap.
