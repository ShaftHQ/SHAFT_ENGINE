# Codacy ACTION_REQUIRED gate

Permanent owner rule from the PR #6158 coalesce babysit (#6168, epic #6161): any Codacy check in `ACTION_REQUIRED` for a ≥medium finding, in any category (security/Bandit, error-prone, unused code, style, complexity), is a hard merge blocker equal to a failing unit test. Portable across Codex, Claude, Grok CLI, Gemini, Copilot, and Grok Bot; no host-only memory exception.

## Iron rule

1. Codacy `ACTION_REQUIRED` (≥medium, any category) ≡ unit red. Do not wait for other jobs to finish before acting.
2. Never arm auto-merge while Codacy is `ACTION_REQUIRED`; if auto-merge is already armed, disarm it (`gh pr merge <n> --disable-auto`) until the finding is fixed.
3. Triage from the check-run annotations (pattern id + file:line), not from a full log: `gh api repos/<owner>/<repo>/check-runs/<id>/annotations`.
4. Fix the code. Do not weaken Codacy, delete findings, or admin-merge.
5. Status and RAG: report `red` / `Blocked` with the pattern id; never bury it under "pending checks". The `watch_pr_checks.py --digest` field `codacy_action_required` carries it ([CI status economy](ci-status-economy.md)).

## Categories

- Complexity / NPath: keep the [Complexity checklist](codacy-complexity-gate.md) (kind-family helpers, rule tables).
- Security / Bandit B607 and friends: local preflight in [tip-churn preflight](tip-churn-preflight.md) (#6165 owns B607 detection).
- Everything else ≥medium: same urgency; fix in the tip already in flight.

## Boundaries

- This gate owns babysit and merge behavior only; it does not change org-level Codacy config. `scripts/agents/watch_pr_checks.py` already classifies `ACTION_REQUIRED` as RED; agent behavior matches it.
