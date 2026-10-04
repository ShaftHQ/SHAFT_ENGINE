# Memory provenance, trust, and quarantine

Safety floor for learned ChaosEngine memory (issue 6520; AgentPoison
2407.12784). Every learned item records **origin** and **trust**. Untrusted
origins stay **quarantined** so retrieval and skill promotion skip them until a
verifier passes.

## Fields

| Field | Values |
| --- | --- |
| `origin` | `owner`, `learning-session`, `verifier`, `doctor`, `ci`, `legacy`, `web`, `tool-output`, `other-agent`, `unknown` |
| `trust` | `trusted`, `verified`, `quarantined` |
| `verifiedBy` / `verifiedAt` | set only when `trust=verified` |

Trusted origins (`owner`, `learning-session`, `verifier`, `doctor`, `ci`,
`legacy`) stamp as `trusted`. All other origins stamp as `quarantined`.

Legacy items with no provenance fields are treated as `trusted` /
`origin=legacy` on read so existing stores keep working.

## Surfaces

| Piece | Path |
| --- | --- |
| Module | [`memory_provenance.py`](../memory_provenance.py) |
| Heuristics stamp + retrieve filter | [`heuristics.py`](../heuristics.py) |
| Learning queue stamp | [`learning.py`](../learning.py) `queue` |
| Skill-promotion skip | [`draft_skill_pr.py`](../draft_skill_pr.py) `provenance_promotion_status` |
| Doctor | `learningMetrics.provenance` / `heuristics.provenance` |
| CLI | `python3 chaos-engine/memory_provenance.py summary\|stamp\|verify` (repo-only) |
| Unittest | `python3 -m unittest tests.scripts.test_chaos_engine_memory_provenance_6520 -v` (repo-only) |
| PR Gate check | `memory-provenance-contract` (surface `memory-provenance`) |

## Rules

1. Retrieval (`heuristics retrieve` / `retrieve.py heuristics`) returns only
   `trusted` and `verified` items.
2. Skill promotion skips `quarantined` items until `verify_item(...)`.
3. Web, tool output, and other-agent content is never auto-trusted.
4. MemPalace / Memory notes CE does not author stay outside this stamp until a
   CE write path exists; treat them as untrusted when promoting into heuristics
   or skills (`origin=tool-output` or `other-agent`).

## Related

- Heuristics store: [heuristics](heuristics.md) (field guidance) · module above
- Learning Session: [self-improve](../skills/self-improve/SKILL.md)
- Harness eval suite (phase-1 peer): [harness-eval-suite](harness-eval-suite.md)
