Caught by adversarial review on PR #4513 before it shipped, in tests/scripts/test_agent_harness_portability.py.

The superseded-policy scan gained POLICY_RECORD_ALLOWLIST: memory object ids permitted to name a retired policy. Two guards were written around it, each defensible alone:

1. every entry must name a live active object that really does trip the scan without it, so a stale exemption cannot sit on silently;
2. the allowlist must be non-empty, on the theory that an unexercised escape hatch is an unenforced one.

Together they deadlock. The whole point of the check is to drive the store to zero offenders. Reconcile the last exempt object -- the intended end state, and exactly what #4461 did to four objects -- and there is no legal configuration left: keeping its entry trips guard 1 as stale, removing it trips guard 2 as empty, and adding a placeholder trips guard 1 as nonexistent. Three reachable states, all red.

The only exits are to leave a memory object misstating a retired policy, keep a knowingly stale exemption, or delete the guard. The last is the cheapest an agent will see, and iron law 4 forbids it. So a guard meant to protect the check becomes the thing that gets it edited away -- the exact pressure that module warns about in four separate comments.

General rule: before adding a guard, walk the state the system is supposed to reach and confirm it is green there. An empty exemption list is the healthy state, not a symptom. If the worry is that the mechanism goes unexercised, exercise it against a synthetic fixture rather than requiring live data to stay dirty.

Same shape to watch for elsewhere: any check that requires a known-bad list, a deprecation registry, a quarantine set or a TODO inventory to be non-empty.