# Field heuristics

Use only the section needed after the entrypoint's operating contract. Current
files and observations outrank this guidance.

## Investigate and debug

- Scout until every plausible caller or dependency affected by the change is
  known; use targeted search before theory.
- Answer the requested outcome. Diagnose without editing when the user asked
  only for assessment.
- Probe the real CLI, protocol, API, binary, or rendered surface before
  implementing against an assumption.
- Treat surprising output as a broken premise. Stop, verify the executing file,
  version, environment, data, and test path, then revise the model.
- Read the complete error and relevant callers. Diff timestamps, versions,
  configuration, and environment when behavior appears to change unaided.
- With multiple hypotheses, list them and run the smallest experiment whose
  outcomes distinguish them. Bisect boundaries; do not add broad logging.
- Reduce a defect to its smallest reproduction. Fix the invariant's owner and
  add the focused regression there.
- After three failed fixes for one symptom, return to reproduction and widen
  the premise search. A partial improvement may mean two defects.

## Plan and change

- Test the unknown most likely to invalidate the approach first. Estimate work
  by unknowns and blast radius, not line count.
- Fix small blockers in the direct path.
- Use a throwaway probe for high uncertainty, then discard it before TDD
  implementation.
- Match surrounding naming and idioms. Comments record constraints; they do
  not narrate code. Keep style-only cleanup separate.
- For user-facing work, verify the cheapest faithful rendering or real flow
  early; visual intent is a premise, not a final polish step.

For hard-to-reverse or cross-cutting decisions, choose one or two focused
challenges: invert the success claim, steelman the rejected design, inspect
second-order effects, run an independent adversarial review, apply five whys,
or write a pre-mortem. More techniques are ceremony unless risk warrants them.

## Verify and report

- Verify the changed behavior, nearest plausible regression, and negative path.
  Ensure the run used fresh sources and inspect real test reports rather than a
  success banner or stale artifact.
- Review delegated work per [delegation](delegation.md) and the
  [verification-gap lens](verification-gap-lens.md).
- State outcome first. Distinguish observed, inferred, assumed, skipped, and
  failed work. Cite repository-relative `path:line` evidence and exact commands.
- Report a decisive failure line, not an unrequested log dump. If safe in-scope
  work remains, finish it before the final report.

## Risk and ownership

- Reversibility sets pace; dependency count sets blast radius. Verify evidence
  for the specific state-changing action, and do not transfer authorization
  between similar actions.
- Restore any system this work breaks before continuing or handing off.
- New user input reorders explicit commitments; it does not erase them.
- Exhaustive mode adds evidence, affected coverage, and independent attempts to
  refute the result. It does not add verbose reports or repeated ceremony.
