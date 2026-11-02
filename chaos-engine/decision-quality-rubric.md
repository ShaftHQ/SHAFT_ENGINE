# ChaosEngine decision-quality operational rubric

Accessed: 2026-09-01. Imports the metrics dictionary, sampling protocol, and
taxonomy classes from `chaos-engine/decision-quality-baseline.md` (#5520) by
reference. Does not redefine those contracts. Parent tracking epic: #5514.
Deliverable for #5521.

Privacy: no prompts, transcripts, secrets, private paths, provider routes,
model IDs, endpoint URLs, or runtime indexes.

---

## Research sources (Accessed: 2026-09-01)

| Source | Use in this artifact | URL |
| --- | --- | --- |
| Stanford Encyclopedia of Philosophy — Decision Theory (rev. 2025-08-20) | Normative choice framing under uncertainty | https://plato.stanford.edu/entries/decision-theory/ |
| SEP — Expected Utility | Risk-weighted choice vocabulary | https://plato.stanford.edu/entries/rationality-normative-utility/ |
| Howard, R.A. (1966) Information Value Theory | Primary value-of-information lineage | https://ieeexplore.ieee.org/document/4082278 |
| Value of information (Raiffa/Schlaifer lineage overview) | EVPI/EVSI intuition for discriminating experiments | https://en.wikipedia.org/wiki/Value_of_information |
| Satisficing (Simon) | Stop and aspiration rules when further search is waste | https://en.wikipedia.org/wiki/Satisficing |
| OODA loop (Boyd) | Competing stage-cycle model | https://en.wikipedia.org/wiki/OODA_loop |
| PDCA (Deming) | Competing plan-do-check-act model | https://en.wikipedia.org/wiki/PDCA |
| Cynefin framework | Domain routing in the rejected stage-gate model | https://en.wikipedia.org/wiki/Cynefin_framework |
| ISO 31000 overview | Risk-process vocabulary | https://en.wikipedia.org/wiki/ISO_31000 |
| NIST SP 800-37 Rev. 2 | Risk-management lifecycle posture | https://csrc.nist.gov/pubs/sp/800/37/r2/final |
| Five Whys | RCA limit note (symptom chains vs root owner) | https://en.wikipedia.org/wiki/Five_whys |

---

## Competing decision models

### Model A — Value-of-Information Discriminating Gate (chosen)

**Complete claim:** choose the next action that maximizes expected information
about the named objective and invariant per unit of cost, latency, and flake
risk. When expected information gain is zero or below that cost, stop,
escalate, defer, or schedule instead of repeating.

**Operators:**

1. Name the objective and the invariant under test.
2. State uncertainty and missing evidence explicitly (never invent values for
   `UNAVAILABLE` metrics from the baseline dictionary).
3. Estimate risk posture, blast radius, and reversibility.
4. List candidate next actions. For each, estimate information gain, cost,
   latency, and flake or external-dependency risk.
5. Select the cheapest discriminating experiment: highest expected information
   gain relative to cost + latency + flake risk.
6. Run that experiment once. Update beliefs. If repeating the same action would
   yield no new information, do not repeat it (`STALE-RETRIEVAL` stop).
7. Exit through exactly one terminal: **stop**, **escalate**, **defer**, or
   **schedule**.

**Lineage:** Howard VOI (1966), Raiffa/Schlaifer decision analysis, Simon
satisficing, NIST risk posture.

### Model B — Stage-Gate OODA/PDCA Compliance Loop (rejected)

**Complete claim:** decision quality comes from completing a mandatory
Observe→Orient→Decide→Act or Plan→Do→Check→Act cycle with phase artifacts and
Cynefin domain classification before any skip is allowed. ISO 31000-style risk
process lives inside Orient/Plan. Check is never optional.

**Operators:**

1. Observe: gather current signals without acting.
2. Orient/Plan: classify domain (clear / complicated / complex / chaotic), list
   risks, and write the intended Check.
3. Decide/Do: act only after the Orient/Plan artifact exists.
4. Check/Act: verify; on failure restart the loop; never skip Check.
5. Escalate only for chaotic domain or missing authority; defer only when Plan
   names an external owner; schedule exhaustive proof as a Plan item.

#### Steelman of Model B

Model B deserves serious weight. Mandatory Check blocks premature YAGNI and
under-testing. Cynefin domain routing blocks treating complex failures as
simple retries. Stage gates reduce escaped defects and false confidence better
than opportunistic information-gain scores that agents can game. Ritual
coverage is an asset under adversarial conditions where skipping proof is easy
to rationalize. PDCA Check/Act is the historical backbone of continuous
improvement; discarding it for a thin score risks metric theater.

#### Why Model B still loses on the reviewed corpus

Against `STALE-RETRIEVAL`, mandatory re-Observe/re-Orient has no intrinsic
staleness gate, so zero-information re-queries remain compliant. Against
`SYM-BEFORE-ROOT`, Act can still patch the latest signal inside a valid loop
without naming the root owner. Against `LATE-ARCH`, Orient can complete without
forcing an architectural-boundary hypothesis, so multiple full cycles can
accrue (`fix_iterations` before boundary named = 4 in the baseline) before
collapse. Model A’s discriminating-experiment rule and zero-information stop
attack those three measured classes directly, while still allowing escalate or
schedule when residual risk requires broader proof.

---

## Corpus evaluation (same reviewed #5520 set)

Metrics and class definitions are those in
`chaos-engine/decision-quality-baseline.md`. Missing telemetry remains
`UNAVAILABLE`. This comparison is qualitative on the reviewed public corpus; it
is not a statistical trial (#5522 owns controlled calibration).

| Taxonomy class | Model A effect on class mechanism | Model B effect on class mechanism | Winner |
| --- | --- | --- | --- |
| `SYM-BEFORE-ROOT` | Requires invariant/root-owner naming before the experiment; targets fewer `fix_iterations` | Allows symptom Acts inside otherwise valid loops | A |
| `STALE-RETRIEVAL` | Forbids zero-information re-query; targets lower `retry_count` on stable stores | Re-Observe can be zero-information and still compliant | A |
| `LATE-ARCH` | Boundary hypothesis is part of uncertainty framing before act | Boundary may stay unnamed across complete cycles | A |

Public anchors reused from the baseline (redacted): catalog-boundary collapse
after multiple transport failures; repeated stable-catalog reads with no new
information units; `fix_iterations` before boundary named = 4 and after = 1.

---

## Operational rubric (Model A)

Use this checklist before every non-trivial tool call, test, workflow, review,
retry, or retrieval. Keep it short. Do not invent numeric precision that
telemetry does not support.

### 1. Objective / invariant

- State the user-visible objective in one sentence.
- Name the single invariant that must hold (API contract, ownership boundary,
  privacy gate, proof owner, or equivalent).
- If the invariant owner is unknown, the next experiment must identify it
  (`LATE-ARCH` / `SYM-BEFORE-ROOT` pressure), not patch the latest symptom.

### 2. Uncertainty

- List what is unknown that could change the action.
- Prefer the cheapest unknown that can falsify the current plan.
- Record baseline metrics as `UNAVAILABLE` when unobserved; never substitute 0.

### 3. Risk

- Sketch probability × impact qualitatively (low / medium / high) using NIST-
  style posture, not fake precision.
- Include correctness, security, user-authority, and preservation-of-work as
  non-negotiable ceilings from epic #5514.

### 4. Reversibility

- Two-way (diff deletable, no persisted external side effect) vs one-way
  (published artifact, external system, irreversible data change).
- One-way actions require higher information gain or explicit escalate.

### 5. Information gain

- Ask: if this action succeeds or fails, which belief about the invariant
  updates?
- Expected information gain = 0 when the source is unchanged since last valid
  read, or when the action cannot discriminate competing hypotheses.

### 6. Cost / latency / flake risk

- Cost: tokens, human attention, money.
- Latency: wall time until the signal returns.
- Flake / external risk: nondeterminism, shared CI queues, third-party quotas.
- Prefer local deterministic proof when it discriminates the same invariant.

### 7. Cheapest discriminating experiment

- Among actions with non-zero expected information gain, pick the lowest
  combined cost + latency + flake risk that can falsify at least one live
  hypothesis about the invariant.
- One experiment, then reassess. Do not batch low-information repeats.

### 8. Stop / escalate / defer / schedule

| Terminal | When |
| --- | --- |
| **stop** | Invariant proven or disproven with adequate evidence; or further actions have expected information gain ≤ cost |
| **escalate** | Authority, safety, or one-way blast radius exceeds current mandate |
| **defer** | Blocked on an external dependency that this agent cannot clear |
| **schedule** | Broader or exhaustive proof has a later rightful owner (for example nightly CI) and current residual risk does not require it now |

Efficient never means skipping required evidence. It means selecting the
smallest evidence that discriminates the real risk at the right lifecycle
stage.

---

## Failure modes (both models)

| Failure mode | How it appears | Guard in this rubric |
| --- | --- | --- |
| Analysis paralysis | Endless Orient/estimation without an experiment | Cap modeling; run the cheapest discriminating experiment |
| Metric gaming | Inflated information-gain scores to justify preferred actions | Require a falsifiable belief update; zero-IG actions are forbidden |
| Under-testing | Skipping Check because local confidence feels high | Non-negotiable ceilings; schedule/escalate when residual risk remains |
| Premature YAGNI | Deleting needed proof or structure because cost looks high | Reversibility + invariant owner first; do not optimize tokens alone |
| False confidence | Treating HTTP/status symptoms as root cause | Force invariant/boundary naming before repeated Acts |

---

## Privacy constraints (normative)

Same gate as the baseline artifact. This rubric and any worked example must
never record prompt content, transcripts, model IDs, provider routes, endpoint
URLs, credentials, private paths, or runtime indexes. Redaction failure
invalidates the example.

---

## Reuse contract for #5522–#5525

Sibling issues may:

- Import this rubric and the #5520 dictionary/taxonomy by reference.
- Pressure-test Model A vs no-guidance controls (#5522).
- Add focused harness guidance only after controlled evidence of improvement.

Sibling issues must not:

- Silently redefine baseline metrics or taxonomy classes here.
- Convert one anecdote into global policy without cross-project evidence.
- Close parent #5514 from this deliverable alone.
