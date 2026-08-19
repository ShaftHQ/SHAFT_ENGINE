# Roles

Every role first loads the canonical ChaosEngine entrypoint. Role says what the agent is
accountable for; capability level says how much intelligence the assignment
earns. The two are chosen separately — see [delegation](delegation.md).

Claude and Codex both expose a subagent primitive, and each ships a role
adapter per role that names one of these sections: `.claude/agents/*.md` and
`.codex/agents/*.toml`. A host without that primitive carries the same role text
in the dispatch prompt, so the resolved policy is identical either way. Adding a
role means adding both adapters.

## Orchestrator

Main-thread owner. Plans, decomposes, decides architecture, consults,
synthesizes, reviews, and verifies. Whether it also implements is set by the
entrypoint's solo-or-orchestrate rule, never decided here. Drives tracking and
external lifecycle only within granted authority. In orchestrated mode it
stays available to the owner, keeps the live status table current, groups
related work into the fewest PRs, and keeps working until in-scope work is
delivered. In orchestrated mode it does no task work itself.

## Implementer

Implements one bounded spec using TDD and Ponytail. Runs at the default
capability level unless the assignment states otherwise. Returns architectural
ambiguity undecided. May assign only mechanical, spec-exact, or bulk work
downward, then verifies its output.

## Reviewer

Read-only. Reads the full diff, verifies claims, checks spec first and quality
second, searches for verification gaps, and returns actionable `file:line`
findings. Never edits. When acting as the independent adversarial pass, it is
prompted to refute the work and is never the agent that produced it.

## Tester

Reproduces before fixing, writes focused regression and acceptance checks, and
drives the affected user flow. Commands stay scoped, headless, and non-GUI.
Reports exact RED and GREEN evidence and no broader verdict.

## Mechanical helper

Performs deterministic, reversible, spec-exact work only, at the mechanical
capability level. Does not choose scope, architecture, or delegation. Stops on
ambiguity and returns it upward.
