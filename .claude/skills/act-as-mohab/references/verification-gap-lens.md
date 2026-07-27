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

## The three gap shapes

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

## The Demonstration technique

For each candidate site: name the smallest realistic regression a real
consumer would observe — invert the branch, drop the default, omit the
field, return the old error code, skip the call. If you can't name one,
drop the path; untested downstream code that nothing would actually break
is not a finding. Then find the relevant test and ask: would the
Demonstration make an assertion fail? If yes, it's verified — no finding.

## Evidence rules (non-negotiable)

- Read a test before claiming what it covers, runs, asserts, or misses.
- Before claiming no test exists, search the repo by symbol and import
  reference — expected file locations alone aren't enough (`gotcha.local-mvn-test-never-fails-the-build-read-surefire-xml-counts-for-verdicts`
  is the SHAFT-specific version of this: `BUILD SUCCESS` from `mvn test`
  proves nothing — read `surefire-reports/TEST-*.xml` counts before calling
  anything green).
- Never assert what you didn't verify; an ungrounded finding gets dropped,
  not softened.
- Say what you actually checked ("none of the tests I read cover this") and
  how far you looked. Only claim a test doesn't exist anywhere when the
  symbol/import search actually shows that.
- Don't assign severity, confidence, or priority — that's triage's job (see
  the four-bucket taxonomy in SKILL.md's Delegation section).

## Trimmed review sequence

1. **Screen for behavioral change.** Non-behavioral (formatting, renames,
   type-only) → zero findings, stop.
2. **Find what changed** — output, side effect, branch, error path, schema
   shape, default, contract.
3. **Trace consumers** — direct callers, registered entry points, contract
   consumers. Stop at the nearest boundary where a test would fail or the
   next hop is guesswork.
4. **Qualify each consumer with the Demonstration, then read its test.**
5. **Re-open the specific test or search result before writing the finding.**
   Don't report from memory of having glanced at it earlier in the review.

## Findings shape

Each finding: `location` (`file:line`), `trigger_condition` (the gap, one
line), `potential_consequence` (what ships wrong and why the checked tests
wouldn't catch it), `gap_shape` (one of the three above, or `other` for a
genuine problem noticed in passing), `evidence` (what you actually read,
with `file:line`). An empty list is a valid, complete result — say so
plainly rather than padding with low-confidence noise.
