# L97 MEGA-PR

Group work into the fewest pull requests:

- one PR per epic (example: entire coverage epic; entire IDE-plugin revamp epic)
- one PR for all open ChaosEngine issues/lessons in a batch
- one PR for all related product-engine issues in a batch
- never one-PR-per-ticket when tickets share an epic or a rules batch

Fold gate-only lessons into an already-open CE PR (see [work-ordering](../../work-ordering.md)).

## Check

Guidance and agentic PR plans name the epic/batch grouping. A plan that opens
one PR per sibling ticket without an epic split rationale fails the mega-PR
gate ([`mega_pr`](../../../rules/mega_pr.py)).
