---
name: cms-paste-no-html-comments
description: Use when authoring hosted-CMS Code-block paste; never include HTML comments — ops notes leak live and read as hidden AI text.
---

# L37 CMS paste: no HTML comments

## Use when

Authoring paste HTML for a hosted builder Code block that publishes to a
public URL. Pairs with [L36](cms-paste-fragment-only.md) and
[L31](paste-then-reverify.md).

## Rules

1. **No `<!--` in paste.** Zero HTML comments in the Code-block artifact.
2. **Ops notes stay out of paste.** Build notes, TODOs, and reviewer asides
   belong in the PR or ticket — not in the published fragment.
3. **Escaped teaching text is fine.** `&lt;!--` in visible copy is not a
   comment opener; real `<!--` always fails the gate.

## Verify

- [ ] `design_qc.py cmsmeta <paste.html>` fails when any `<!--` remains;
      passes on a comment-free fragment (including `&lt;!--` in body text).

## Why

HTML comments in a Code-block paste shipped to the live page. Operators'
notes leaked publicly and read as hidden AI/assistant text in view-source
and some crawlers.

## Sources

Field lesson from a hosted-CMS embed: comment-wrapped ops notes appeared on
the public product URL until the paste was stripped and re-verified.
