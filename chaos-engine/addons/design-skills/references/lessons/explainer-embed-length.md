---
name: explainer-embed-length
description: Use when sibling product pages share one Watch layout; keep landscape explainers in a short similar length band.
---

# L30 Shared Watch-slot length parity

## Use when

Two or more sibling product embeds share one Watch layout. Extends
[D21 explainer arc](../explainer-arc.md) and
[D20 delivery QC](../delivery-qc.md) web embed.

## Rules

1. **Short explainer band.** Prefer ~**60–100 s**; hard cap ~**120 s**
   unless the plan states another band. Reject multi-minute masters for
   the shared slot.
2. **Sibling parity.** Durations within **30 s** of each other; same
   aspect (16:9).
3. **Probe public duration.** Gate `loadedmetadata.duration` (or ffprobe)
   from the public URL before SIGN.

## Verify

- [ ] `design_qc.py cmsduration --dur D --min 60 --max 120` fails outside
      the band; passes inside.
- [ ] `design_qc.py cmsduration --dur A --peer B --peer-tol 30` fails when
      |A−B| > 30 s.

## Why

A multi-minute master next to a short explainer breaks the shared Watch
slot and hurts completion.

## Sources

Field lesson from a hosted-CMS embed redesign: a multi-minute master was
rejected for a ~60–100 s explainer pair with matched aspect.
