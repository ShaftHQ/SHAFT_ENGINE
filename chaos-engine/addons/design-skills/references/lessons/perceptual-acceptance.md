---
name: perceptual-acceptance
description: Use when numeric QC or a reviewer is about to accept a visual/media package, or when metric floors are set or changed.
---

# L13 Perceptual acceptance

## Use when

A visual or media package is about to ship or be accepted, numeric gates
report pass (`gates_all_pass` or equivalent), metric floors are chosen or
tuned, or a reviewer would otherwise sign off from JSON alone.

## Rules

1. **Proxies ≠ perceived realism.** Embedding similarity, edge scores,
   Laplacian / MAR / MAD / EAR heuristics, and aperture↔RMS lag are
   **proxies**. They can be green on a still or clip a human rejects in
   one second. Never equate a green proxy with "looks real."
2. **Blind A/B vs real footage first.** Before reading any JSON, run a
   human blind side-by-side: generated clip vs real reference footage
   (same script window when available). Pass/fail that glance first.
3. **Perceptual fail overrides JSON.** Hard visual fails (uncanny mouth,
   warped glasses or props, identity drift, stiff/dead eyes, or any
   owner/reviewer "looks wrong") fail the package even when every metric
   is green. Residual prose in a self-review does not replace a hard fail.
4. **Metrics are floors only.** Numeric gates are minimums that catch
   crashes and vacuous runs. They never flip the ship bit. Acceptance
   still requires the human perceptual gate.
5. **NaN / missing fail closed.** Missing, `None`, or NaN gate inputs are
   **FAIL**, never a vacuous pass (for example empty mean coerced to
   `0.0` and compared to a floor). Unit-test None/NaN/missing paths.
6. **Never tune floors to pass.** Do not lower a floor so a weak package
   goes green. Raise a floor only with an identity/quality RCA and keep
   the old failing case as a regression. Tuning floors to clear a
   packaging date is forbidden.
7. **Reviewer must view output vs real reference.** Accepting
   `gates_all_pass` (or any all-green metric receipt) without a human-eye
   check against real reference footage is an invalid acceptance. The
   sign-off names that the reviewer watched the output beside a real
   reference (or recorded why no reference exists and who owned the
   perceptual call).

## Verify

- Handoff / acceptance note records blind A/B (or equivalent perceptual
  glance) **before** metric tables; metric-only ready claims fail review.
- At least one hard visual fail criterion is listed; a green JSON with a
  named visual defect is treated as FAIL in the review receipt.
- Gate code and tests show None/NaN/missing → FAIL; live receipts expose
  null counts or explicit null handling.
- Floor changes cite RCA + a kept failing fixture; no "lowered to pass
  this package" notes.
- Reviewer sign-off cites viewed output vs real reference (or the
  documented no-reference perceptual owner), not `gates_all_pass` alone.

## Sources

Field closeout: multi-milestone packages green on face/edge/mouth/motion
proxies while owner judged the line extremely poor; floors previously
tuned under a low bar; a reviewer once accepted `gates_all_pass` without
a human-eye check. Companion: [L08 visual-first approach](visual-first-approach.md).
