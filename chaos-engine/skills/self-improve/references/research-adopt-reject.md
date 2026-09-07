# Research note — Task Observer adopt vs reject

Sources reviewed: Task Observer `SKILL.md` + `references/environments.md`
(rebelytics, CC BY 4.0); CE `learning.py`; MemPalace continuous-improvement
workflows; delivery-phase Learning Session.

## Adopted (adapted)

| Idea | CE shape |
| --- | --- |
| Lean core + on-demand refs | This skill + `references/*` |
| Activation reliability beyond description matching | SessionStart one-line locator + Learning Session / post-delivery gate |
| Capture vs review split | Observe during work; queue via `learning.py`; review/apply later |
| Observation taxonomy | harness vs product dual track (`references/observation-taxonomy.md`) |
| Silent same-turn capture bias | Prefer immediate queue over mental batching |
| Attribution | CC BY 4.0 credit in LICENSE / UPSTREAM / THIRD_PARTY_NOTICES |

## Rejected / deferred

| Idea | Why |
| --- | --- |
| Always-on full SessionStart protocol (frontmatter scan of hundreds of files) | Token cost; CE keeps SessionStart locator-only under `SESSION_START_MAX_BYTES` |
| Verbatim observation-log YAML frontmatter schema | CE already has privacy-gated `learning.py` queue → GitHub issues |
| Auto-stage skill patches / PENDING.md install ledger | CE requires human/CI review; skill proposes only |
| Cross-project global observation workspace defaults | CE is project-portable; observations stay project-scoped |
| Weekly-review automation that mutates live skills | Out of scope; review remains operator-driven |
| Blind clone of Task Observer prose | License + CE privacy/filing invariants demand a native rewrite |

## Dual track (must)

1. **Harness** — skills, hooks, MemPalace, Graphify, Headroom, installer/doctor UX.
2. **Product** — adopter product enhancements as privacy-safe queued learnings.
