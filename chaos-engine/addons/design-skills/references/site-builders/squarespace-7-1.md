---
name: squarespace-7-1
description: Use when styling a Squarespace 7.1 site with Custom CSS and Code blocks only (no Code Injection).
---

# Squarespace 7.1 without JavaScript

Companion to [C01 web design](../craft/web-design.md). Applies when the plan
locks Code Injection, so only Custom CSS and Code blocks are available.

## Custom CSS (LESS 1.3.3)

- The minifier breaks `clamp()`, `calc()` arithmetic and the `a / b` grid
  shorthand. Use fixed sizes per breakpoint and longhand grid properties;
  split a padding sum across two elements instead of
  `calc(24px + env(...))`.
- A local `lessc` parses differently (it rewrites `1fr` to `1 fr`; live
  keeps `1fr`). Verify by injecting the source CSS over the live page, not
  the locally compiled output.
- `@property` is rejected; put it in the Code block's `<style>`.

## Selectors and sections

- New Code blocks render as `.sqs-block-website-component`, not
  `.sqs-block-code`; selectors match both.
- Fluid Engine sections saved at "Section height: Medium" add
  viewport-scaled padding. For code-only sections zero `padding` and
  `min-height`, keyed with `:has(.snippet-class)`.
- An empty `data-section-theme` lets the default colours leak in. Force the
  palette with `section:has(.snippet)`, not the theme attribute or section ID.

## Navigation, menu and footer

- Fix nav order in the editor (Pages > Main Navigation), never with CSS
  `order`, so keyboard and screen-reader order stay correct. An RTL desktop
  nav needs `margin-inline-start`; the built-in `margin-left` lands on the
  wrong side.
- Mobile menu overlay (`.header-menu`, `.header-menu-cta`): CTA inset 24 px,
  full width, at least 44 px tall; brand-weight links; list top-aligned.
- Mobile footer: at least 32 px padding between it and a dark CTA band,
  icons and lines centred, bottom padding plus
  `env(safe-area-inset-bottom)`.

## Media and preview

- Video without JS: an iframe `srcdoc` facade (`youtube-nocookie`, autoplay
  only after the click, `loading=lazy`, 0 requests before the click). No
  muted loop next to an animated hero: one motion focus per viewport.
- The live CSP blocks `data:` images. In previews serve local assets through
  an intercepted same-origin URL.
- Motion rules: [D08](../motion-principles.md). RTL text:
  [D05](../typography-layout.md).
