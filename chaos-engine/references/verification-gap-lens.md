# Verification-gap lens

Worked method for the second-pass adversarial review (SKILL.md, Delegation)
and for reviewing a subagent's diff before building on it. Adapted from
bmad-method's `src/core-skills/bmad-review/references/lens-verification-gap.md`
(MIT-licensed; trimmed and reworded to our scale — see PR notes for the
adoption record).

**Goal:** find changed behavior that could break without verification
catching it. Ask one question: if the behavior this change is supposed to
produce broke where it's actually used, would a test fail? Don't hunt for
correctness bugs generally — that's the rest of the review; this lens is
verification-coverage only.

## The four gap shapes

1. **Regression gap.** The changed code regresses where it's used, and no
   test covering that use would fail.
2. **Missing-adoption gap.** A place that should now use the new behavior
   doesn't — it handles the same case its own way, or not at all — and no
   test flags the omission. Qualifies only when there's a real supersession
   signal (the change's intent, a replaced sibling site, a deleted duplicate)
   *and* the local site shares the same observable contract; otherwise it's a
   refactor suggestion, not a gap.
3. **Broken-verification gap.** A test appears to cover the changed
   behavior but wouldn't actually catch a regression — skipped, flaky, not
   run in the normal path, or too weak to observe the change (mock-only,
   snapshot-only, success/no-throw checks).
4. **Unbound-check gap.** The change adds or edits a check, guard, pin or
   metric that would still pass with the thing it protects removed. Three
   ways it happens: the test declares its own copy of a pattern, threshold
   or clause the shipped artifact owns, so it verifies the copy; a metric
   whose input is absent reports that absence as a value; or the fix's own
   mechanism is unguarded, so reverting it leaves the suite green.

## The Demonstration technique

For each candidate site: name the smallest realistic regression a real
consumer would observe — invert the branch, drop the default, omit the
field, return the old error code, skip the call. If you can't name one,
drop the path; untested downstream code that nothing would actually break
is not a finding. Then find the relevant test and ask: would the
Demonstration make an assertion fail? If yes, it's verified — no finding.

## Proving a check binds

Imagining the Demonstration is right for code you are reviewing. For a check
in your own diff it is not, because the mutation is free and revertible:
apply it, run it, read the failure, revert. Mutate the shipped artifact,
never a fixture.

Weakening counts as a mutation. A rule survives deletion and dies by
addition — appending "unless time is short" leaves every pinned word in
place — so mutate by qualifying as well as by removing. Make a metric move;
one that cannot report failure is reporting its own absence.

## Evidence rules (non-negotiable)

- Read a test before claiming what it covers, runs, asserts, or misses, and
  re-open it before writing the finding rather than reporting from memory of
  having glanced at it earlier in the review.
- Before claiming no test exists, search the repository by symbol and import
  reference; expected file locations alone are not enough. A successful build
  banner alone proves nothing when the runner writes structured reports; read
  the report counts before calling anything green.
- Say what you actually checked ("none of the tests I read cover this") and
  how far you looked. An ungrounded finding gets dropped, not softened — that
  is a wider rule than the refuted bucket, which is about a finding you did
  ground and disproved.
- Don't assign severity, confidence, or priority — that's the returned-work
  triage in delegation.md.

## Trimmed review sequence

1. **Screen for behavioral change.** Non-behavioral (formatting, renames,
   type-only) → zero findings, stop.
2. **Find what changed** — output, side effect, branch, error path, schema
   shape, default, contract.
3. **Trace consumers** — direct callers, registered entry points, contract
   consumers. Stop at the nearest boundary where a test would fail or the
   next hop is guesswork.
4. **Qualify each consumer with the Demonstration, then read its test.**

## Findings shape

Each finding: `location` (`file:line`), `trigger_condition` (the gap, one
line), `potential_consequence` (what ships wrong and why the checked tests
wouldn't catch it), `gap_shape` (one of the four above, or `other` for a
genuine problem noticed in passing), `evidence` (what you actually read,
with `file:line`). An empty list is a valid, complete result — say so
plainly rather than padding with low-confidence noise.
