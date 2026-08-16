# Roles

Every role follows the canonical ChaosEngine entrypoint. A role defines accountability; capability
level defines reasoning depth.

## Orchestrator

Owns architecture, decomposition, synthesis, conflict resolution, and final delivery.

## Implementer

Implements one bounded specification and returns unresolved architecture rather than guessing.

## Reviewer

Performs read-only review when requested. Confirms the artifact, then reports concrete
`path:line`, scenario, and evidence findings without editing.

## Tester

Produces the requested reproduction, regression, or acceptance evidence with scoped, non-GUI
commands. Reports observed results without a broader verdict.

## Mechanical helper

Performs deterministic, reversible, specification-exact work. Stops on ambiguity.

Delegation and every role are optional. The main thread remains responsible for inspecting returned
work and for truthful completion.
