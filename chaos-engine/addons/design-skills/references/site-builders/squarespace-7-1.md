---
name: squarespace-7-1
description: Use when styling a Squarespace 7.1 site with Custom CSS and Code blocks only (no Code Injection).
---

# Squarespace 7.1 without JavaScript

Companion to [C01 web design](../craft/web-design.md). Applies when the plan
locks Code Injection, so only Custom CSS and Code blocks are available.

## Custom CSS (LESS 1.3.3)

- Lesson gate: [L33](../lessons/less-css-grid-calc.md) (`lessgrid`). The
  minifier breaks `clamp()`, `calc()` arithmetic and the `a / b` grid
  shorthand. Use fixed sizes per breakpoint and longhand grid properties;
  escape calc as `~"calc(...)"` or split a padding sum across two elements.
- A local `lessc` parses differently (it rewrites `1fr` to `1 fr`; live
  keeps `1fr`). Verify by injecting the source CSS over the live page, not
  the locally compiled output.
- `@property` is rejected; put it in the Code block's `<style>`.
- Grid tracks in %: 3 across 32% with 2% gaps, 2 across 49%, 1 column
  100%, so no compiler can mangle `1fr`. A collapse seen only in a locally
  compiled test is not a live bug: re-check with the source CSS injected
  and the live CSS removed (NOINJ) before fixing it.
- Patch-on-top build: ship `custom-vN.css` = previous full CSS plus
  `vN-patch.css`, keep a patch-only file for append, compile the full file
  with LESS 1.3.3 and confirm no `__TOKEN__` placeholders remain.

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
- Facade box: `aspect-ratio: 16/9` plus a `padding-top: 56.25%` fallback
  wrapper; verify a 1.778 ratio at every width and no horizontal scroll.
- The editor's "iframe embeds are a premium feature" warning can still
  reach logged-out visitors. Verify with `curl` of the public page (srcdoc
  markup present, no upgrade notice), not the editor view.
- Headless Chrome does not prove autoplay. Report it unverified and ask for
  one real-device tap.
- Probe the live origin `Content-Security-Policy` `img-src` before inlining.
  If `data:` is allowed, small brand marks may be `data:` URIs; otherwise
  absolute `https://` only. In local previews, when `data:` is blocked,
  serve assets through an intercepted same-origin URL. See
  [hosted-cms-embed](hosted-cms-embed.md) Assets and [L24](../lessons/hosted-cms-assets.md).
- Motion rules: [D08](../motion-principles.md). RTL text:
  [D05](../typography-layout.md).

## Source vs rendered, plan limits

- Paste source, never rendered DOM. Copies from the live page carry
  `.sqs-code-container` and `<style class="transform-vars">`; re-pasting
  nests one wrapper per pass. Keep paste source apart from rendered
  copies (e.g. `.rendered.html`). After apply,
  `.sqs-code-container .sqs-code-container` count must be 0.
- After every GUI paste from the **build artifact**, run the headless
  livecms matrix on the public URL at 390 and 1440
  ([L28](../lessons/live-verify-cms-embed.md),
  [L31](../lessons/paste-then-reverify.md)). Editor toasts are untrusted.
- Product/landing embeds under existing site chrome: follow
  [hosted-cms-embed](hosted-cms-embed.md). Sticky header offset for the
  first paste section is [L23](../lessons/hosted-cms-no-dup-chrome.md).
- Read the plan and the site-language list before planning SEO work. The
  Personal plan has no Code Injection (needs Core+); JSON-LD still works via a
  Code block (verified live). The http `og:image`/`twitter:image` emitted for
  static1 URLs cannot be overridden.
  A missing site language keeps `<html lang>` fixed; set language per block
  ([D05](../typography-layout.md)). Drop impossible items early.

## Recovery from live

Rebuild lost source from the served Custom CSS URL (in the page HTML) and
each Code block's HTML (`.sqs-block-content` holding your snippet classes),
plus a README of what is live. Diff it against the last build to catch
drift. Page merges and redirects: [site merge](site-merge-redirects.md).
