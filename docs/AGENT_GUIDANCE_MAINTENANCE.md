# Agent Guidance Maintenance

## Goal

Keep mandatory context small, accurate, and stable. Repository policy is
canonical in `AGENTS.md`; host adapters add only host-specific behavior.
Task workflows belong in skills and Java-only rules belong in path-scoped
instructions.

## Ownership Map

| Requirement class                                                | Canonical location                                      |
|------------------------------------------------------------------|---------------------------------------------------------|
| Repository map, safety, cost policy, validation tiers            | `AGENTS.md`                                             |
| Claude-only loading and response behavior                        | `CLAUDE.md`                                             |
| Copilot-critical review instructions                             | `.github/copilot-instructions.md`                       |
| Production Java API, property, logging, Allure, capability rules | `.github/instructions/framework-source.instructions.md` |
| Test lifecycle, parallelism, Allure verdict, Java 25 test traps  | `.github/instructions/java-tests.instructions.md`       |
| CI investigation, flaky tests, release preparation               | `.github/skills/*/SKILL.md`                             |
| Architecture, CI details, and operational history                | Existing `docs/`, executable config, and Git history    |

## Rule Disposition

The consolidation applied these decisions to every prior instruction category:

| Prior content                                                                                                                 | Disposition                                                          |
|-------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------|
| Toolchain, repository map, secrets, generated files, destructive/release restrictions                                         | Retained globally                                                    |
| Minimal diffs, dirty-worktree preservation, structured parsing, bug reproduction                                              | Retained globally                                                    |
| Public JavaDoc, compatibility, SHAFT facade, properties, logging, batteries-included tools, W3C capabilities                  | Moved to source scope                                                |
| Driver lifecycle, `ThreadLocal`, static-state isolation, Allure population/root handling, TestNG Java 25 traps                | Moved to test scope                                                  |
| CI artifact parsing and provider-vs-code diagnosis                                                                            | Moved to CI skill                                                    |
| Parallel image/file races and shared server/property lessons                                                                  | Moved to flaky-test skill or test scope                              |
| Reactor versions, release metadata, Central immutability, consumer verification                                               | Moved to release skill                                               |
| Long architecture examples, feature catalogs, workflow matrices, dated incidents                                              | Left in user docs, executable files, or Git history                  |
| Repeated commands, examples, anti-pattern lists, and cross-agent copies                                                       | Deduplicated                                                         |
| Three mandatory PDCA cycles, build before each commit, checkpoint commits, routine screenshots, automatic PR/issue operations | Retired as costly ceremony; replaced by risk-based proof             |
| Per-session memory updates and append-only agent ledgers                                                                      | Retired; durable rules go directly to their canonical file           |
| Empty vendor skill stubs and stale setup-verification reports                                                                 | Removed                                                              |
| Paid planner plus second-model summary orchestration                                                                          | Retired; normal agents plan directly and escalate only when required |

## Budgets And Audit

Budgets live in `scripts/ci/agent_guidance_budget.json`. Run:

```bash
python3 scripts/ci/validate_agent_guidance.py
python3 -m unittest tests.scripts.test_validate_agent_guidance
```

The audit checks file and host token budgets, the approved 60% total reduction,
skill frontmatter, local links, path scopes, stale references, costly mandates,
and duplicated long paragraphs. It is deterministic and requires no network.

## Updating Guidance

1. Confirm a rule is recurring, repository-specific, and not already encoded
   in executable configuration.
2. Put it in the narrowest canonical surface from the ownership map.
3. Replace an existing rule instead of appending a second version.
4. Keep volatile versions, dates, incidents, and long commands outside
   automatically loaded files.
5. Run the audit and its unit tests.

The paid refresh workflow is manual. It runs only when the deterministic audit
finds a violation or a maintainer explicitly forces an AI review with a
reason.
