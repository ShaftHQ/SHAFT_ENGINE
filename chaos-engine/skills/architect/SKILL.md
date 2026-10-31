---
name: architect
description: >-
  Use for design-level judgement: requirements, quality attributes, trade-offs,
  ADR, contracts, resilience, security, observability, refactor, debug, review,
  debt.
license: MIT
permissions:
  - read
  - write
---

# Architect

Judgement cards for design-level work. Core [triage](../chaos-engine/SKILL.md#triage)
sets depth; [Ponytail](../../companions/ponytail-ultra.md) still caps scope.
Load ONE card file below, not all.

## First: retrieve

1. `python3 .chaos-engine/tool.py retrieve --store graphify "<module> callers dependencies"`:
   blast radius, seams, who breaks.
2. `python3 .chaos-engine/tool.py retrieve --store mempalace "<topic> decision ADR gotcha"`:
   prior decisions, rejected options, lessons.
3. Record `retrieve: used`. Contradicts a prior decision: cite it, supersede it
   in the ADR, never silently diverge.

## Tier → cards

| Triage tier | Use |
| --- | --- |
| One file, reversible | 18 debug, 19 review only |
| One module, reversible | add 1, 3, 9, 16, 17 |
| Public contract / hard to reverse | all relevant; 4 ADR mandatory |

## Card index

| # | Card | Rule | File |
| --- | --- | --- | --- |
| 1 | Requirements | Testable acceptance criteria + non-goals before design. | [analyze](references/analyze.md#1-requirements) |
| 2 | Quality attributes | Name top 3 (ISO 25010) as measurable scenarios. | [analyze](references/analyze.md#2-quality-attributes) |
| 3 | Trade-offs | 2+ options, criteria from 2, pick by reversibility. | [analyze](references/analyze.md#3-trade-offs) |
| 4 | ADR | Hard-to-reverse choice gets a 10-line ADR. | [analyze](references/analyze.md#4-adr) |
| 5 | C4 sketch | Multi-component change: context + container diagram. | [analyze](references/analyze.md#5-c4-sketch) |
| 6 | API contracts | Additive change; versioned; idempotent; Hyrum's law. | [design](references/design.md#6-api-contracts) |
| 7 | Boundaries | Domain inward, I/O outward; depend on stable things. | [design](references/design.md#7-boundaries) |
| 8 | Domain language | Code names = domain words; one context, one model. | [design](references/design.md#8-domain-language) |
| 9 | Principles | SOLID/DRY/KISS/YAGNI calibrated; rule of three. | [design](references/design.md#9-principles) |
| 10 | Patterns | Pattern by forces present today, never speculative. | [design](references/design.md#10-patterns) |
| 11 | Failure modes | Timeout, bounded retry+jitter, idempotency, breaker. | [operate](references/operate.md#11-failure-modes) |
| 12 | Observability | Structured logs, RED metrics, traces, SLO. | [operate](references/operate.md#12-observability) |
| 13 | Security | Trust boundaries, STRIDE-lite, least privilege. | [operate](references/operate.md#13-security) |
| 14 | Performance | Measure first; budget; fix the bottleneck only. | [operate](references/operate.md#14-performance-and-scale) |
| 15 | Data | Expand/contract migrations; one writer per fact. | [operate](references/operate.md#15-data-and-state) |
| 16 | Test strategy | Risk-based mix; contract tests at boundaries. | [change](references/change.md#16-test-strategy) |
| 17 | Refactor/legacy | Characterize, then small safe steps; strangler. | [change](references/change.md#17-refactoring-and-legacy) |
| 18 | Debugging | Root cause before fix; 3 failed fixes → rethink. | [change](references/change.md#18-debugging) |
| 19 | Review | Design > correctness > tests > clarity; severity. | [change](references/change.md#19-review) |
| 20 | Tech debt | Name it, price it, file it; pay where you touch. | [change](references/change.md#20-tech-debt) |

Sources: [sources](references/sources.md).
