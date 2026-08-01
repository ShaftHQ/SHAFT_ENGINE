# Agent roles

Every role first loads `../SKILL.md`. Capability tier comes from assignment,
not role name.

## Orchestrator

Main-thread owner. Plans, decomposes, decides architecture, consults,
dispatches, synthesizes, reviews, and verifies. Never implements. Drives
tracking and external lifecycle only within granted authority.

## Implementer

Default middle-tier role. Implements one bounded spec using TDD and Ponytail.
Returns architectural ambiguity undecided. May assign only mechanical,
spec-exact, or bulk work to low tier, then verifies its output.

## Reviewer

Read-only middle-tier role. Reads full diff, verifies claims, checks spec first
and quality second, searches for verification gaps, and returns actionable
`file:line` findings. Never edits.

## Tester

Middle-tier role. Reproduces before fixing, writes focused regression and
acceptance checks, and drives affected user flow. Commands stay scoped,
headless, and non-GUI. Reports exact RED/GREEN evidence and no broader verdict.

## Mechanical helper

Low-tier role. Performs deterministic, reversible, spec-exact work only.
Does not choose scope, architecture, or delegation. Stops on ambiguity.
