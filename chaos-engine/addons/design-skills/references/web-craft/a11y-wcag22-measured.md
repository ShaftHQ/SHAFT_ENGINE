---
name: a11y-wcag22-measured
description: Use when auditing or building a web UI for accessibility against WCAG 2.2: focus, targets, zoom, motion, toasts and route changes, with severity tied to a success criterion.
---

# W09 WCAG 2.2, measured

## Use when

A page or component is built or audited for accessibility. Extends
[D04 web interface audit](../web-interface-audit.md).

## Rules

1. **Focus ring:** at least 3:1 against every colour it crosses (1.4.11),
   2 px with an offset; a focused element is never fully hidden under
   sticky chrome (2.4.11): `scroll-margin-top` = header height.
2. **Tabindex** only 0 or -1; positive values are a finding. No
   `maximum-scale=1` or `user-scalable=no`.
3. **Targets** at least 24×24 CSS px (2.5.8 AA); aim 44 touch, 40 desktop.
   Every drag has a single-pointer alternative (2.5.7).
4. **Moving content** over 5 s needs pause (2.2.2); action and error
   toasts stay until dismissed.
5. **SPA route change:** update `document.title` and move focus to the
   `<h1>` or `<main>`.
6. **Severity:** HIGH only with a WCAG A/AA success criterion number or a
   blocked assistive-tech task; AAA and conventions are recommendations.

## Verify

- [ ] `design_qc.py a11yscan src/` (positive tabindex, zoom lock)
- [ ] `design_qc.py contrast --pair focus:bg:ui` for each surface crossed
- [ ] Keyboard pass with the sticky header; each finding cites its SC.

## Sources

- [jakubkrehel/skills better-accessibility](https://github.com/jakubkrehel/skills/tree/019ab7c199154f4509b55f12d84a4f5b578dc606)
  @ `019ab7c199154f4509b55f12d84a4f5b578dc606` (MIT, adapted).
- [addyosmani/web-quality-skills accessibility](https://github.com/addyosmani/web-quality-skills/tree/afa8da942115f2961fdbfa80807ea0b232ff6c00)
  @ `afa8da942115f2961fdbfa80807ea0b232ff6c00` (MIT, inspiration).
