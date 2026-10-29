---
name: live-verify-cms-embed
description: Use after saving a hosted-CMS paste; Done means a headless matrix on the public URL at 360/390/430/1440, not the editor preview.
---

# L28 Headless live-verify after paste

## Use when

A Code-block paste was saved in a hosted builder. Extends core
[ui-delivery](../../../../references/ui-delivery.md) and
[D04 web interface audit](../web-interface-audit.md). Playbook:
[hosted-cms-embed](../site-builders/hosted-cms-embed.md).

## Rules

1. **Public URL, not editor.** computerUse / editor paste is not Done.
2. **Matrix at 360, 390, 430, and 1440.** Headless pass required:
   HTTP 200, required media playable with expected duration ±tol, identity
   tile + heading colour ([L26](hostile-cms-resets.md)), no duplicate chrome
   ([L23](hosted-cms-no-dup-chrome.md)), assets loaded ([L24](hosted-cms-assets.md)),
   single CMS `og:title` ([L25](hosted-cms-owns-meta.md)).
3. **Fail closed.** Partial “200 OK” reports without this matrix are
   forbidden. Every acceptance row must be checked.
4. **Record video durations** from `loadedmetadata` for every `<video>`.

## Verify

- [ ] Live measure (URL or `header_measure` boxes + content widths) yields per-viewport header Δ≤1px vs `/` at 360/390/430/1440 (L36); forged `header_*_delta_ok_*` bools alone fail.
- [ ] `design_qc.py livecms report.json` fails when any required row is
      missing or not pass; passes on a complete green matrix.
- [ ] Receipt includes viewport_360/390/430/1440 plus `media_ok`/`duration_ok`
      (`n/a` if no video) and `nested_containers_0`.

## Why

Desktop eye and editor toasts miss duplicate nav and blank logos. The
headless matrix on the public URL catches them before SIGN.

## Sources

Field lesson from a hosted-CMS embed redesign: nav duplication and blank
logos survived first live eye; headless 360/390/430/1440 matrix caught them.
