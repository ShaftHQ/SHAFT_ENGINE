---
name: explainer-arc
description: Use when an explainer, promo, or product overview needs a hook, a persuasion arc, audience-specific versions, and measurable goals before storyboarding.
---

# D21 Explainer arc and audience

## Use when

A video must persuade, not only teach: a product overview, a launch piece,
or "why choose this" explainer, often in more than one audience version.
Run after the brief inputs exist and before the D01 storyboard is final.

## Inputs

Personas (role, goal, fear, where they watch), the one message per version,
the proof points with sources, the call to action and how it is measured.

## Rules

- One storyboard per audience. Executive and technical versions share facts,
  never copy: executives get outcomes, risk, cost, and adoption effort;
  engineers get real code, real output, and the number instead of the
  adjective.
- Beats run hook, problem, solution, proof, cta (`beat` on every scene). A
  mid-roll CTA is allowed as an extra beat; the last beat is always the CTA.
- The hook starts at 0 s and states the promise or the pain by 5 s. No logo
  bumper or title card before it; the brand lands in the CTA.
- Proof beats carry claims with evidence (D01). Prefer numbers with a named
  source, a real run on screen, or a guarantee the docs state. No customer
  logos, download counts, or comparisons the evidence cannot back.
- Length bands: executive 60 to 120 s, technical 180 to 300 s with chapters,
  social cut under 60 s. Deliver the main message before the midpoint.
- Show one honest limit or error in the technical version; it builds more
  trust than polish.
- Social versions work muted: on-screen supers carry the argument and
  captions are burned in (D18, D20).
- Goals are written in the brief and measured after release: average
  engagement against the length benchmark, reach of the proof beat, CTA
  click-through, and one lead indicator (a tagged link). They never block
  delivery; QC and review do.

## Workflow

1. Write personas and one message per version into `brief.md`.
2. Draft the beat sheet per version; time it aloud.
3. Tag each D01 storyboard scene with `beat`; attach claims to proof beats.
4. Pick the measurement links (tagged URLs) and write the goals.

## Verify

- `design_qc.py arc storyboard.json --audience exec|technical|social`
  passes: beat order, hook by 5 s, closing CTA in the last 20%, evidence on
  every proof beat, length inside the band.
- `design_qc.py brief storyboard.json` and `describe storyboard.json` pass.
- The brief lists each goal with its benchmark and its source.

## Sources

LinkedIn Marketing Solutions B2B video research (first six seconds, 30 s to
2 min sweet spot, job relevance); Wistia State of Video 2025 (engagement by
length, in-video CTA conversion); developer-audience surveys (Hackmamba
2024, Apollo Studio) on real code over slides. Own synthesis.
