## Delegate continuity

Dispatch may opt into bounded continuity. Omit `continuity` for unchanged
legacy behavior. Continuity freezes capability floor, maximum attempts,
retryable exit codes, bounded backoff, authority/checkpoint hashes, completed
action hashes, tracker/PR hashes, and ordered alternate identity/session hashes.
At most four writers may participate: one initial writer plus no more than three
alternates, with no more than four total attempts. Each private alternate also
carries one validated target and bounded argument list. The supervisor keeps the
sealed launcher fixed while selecting those inputs in memory for each attempt.
Raw prompts, credentials, links, provider/model names, commands, and local paths
never enter continuity state.

Replacement starts only after prior process-group death is proven. Lower
capability alternates are skipped. Learning registration precedes launch;
registration failure creates no participant or process. One live replacement
sets `replacement_running`, making repeated resume calls idempotent. Exhausted
attempts open the breaker and block; unverifiable process death or identity
quarantines. Terminal receipts include only redacted continuity hashes,
attempt/state, and participant hashes. Root still owns final evidence import and
the sole Learning Session.

For opted-in dispatches, runner starts its private `_supervise` process instead
of one-shot `_capture`. Supervisor retains raw alternate session identifiers
only in its inherited process environment, removes them before launching any
delegate, and never writes them to disk. It observes sealed-launcher exit,
proves process-group death, applies backoff and capability selection, registers
replacement, then launches candidate-specific private inputs against the same
frozen task and authority. Final successful evidence
moves normal `status` flow to review without owner input. `_supervise` is an
internal runner command, not an operator-facing interface.
The original timezone-aware deadline bounds all attempts, backoff, and process
runtime. Expiry blocks continuity before another launch.
