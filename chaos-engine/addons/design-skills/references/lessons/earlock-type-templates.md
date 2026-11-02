---
name: earlock-type-templates
description: Use when writing Shorts title/description; ear-lock the title to kept speech and pick viral vs qa G16 line-1 templates.
---

# L22 Ear-lock and type templates

## Use when

Writing METADATA for a Short, choosing `type=viral` vs `type=qa`, or
checking that the description line-1 prefix matches the type. Extends
[L04](signed-output-release.md) G16 and [title polarity](signed-output-release.md)
G21/G22.

## Rules

1. **Ear-lock the title.** Title words must be spoken inside the kept
   window. Gate with `design_qc.py titlepolarity` (G22 with `--words` +
   `--splice` when cuts exist).
2. **Type drives the G16 line-1 prefix.**
   - `type=viral` → `مقطع: ` + Title; hashtag line includes `#viral`.
   - `type=qa` → `مقطع من الـ Q&A: ` + Title; hashtag line includes `#QA`.
3. **Choose type from the title shape.** Statement titles → viral;
   question titles (Arabic `؟`) → qa. Document `## Type` in METADATA.
4. **Desc line 1 carries the same Title string.** Mismatch fails G16.
5. **Project fill-ins stay on flags.** Site URL and full hashtag string
   are `--site` / `--tags`; never hard-coded in the gate.

## Verify

- [ ] `design_qc.py metadatadesc METADATA.md --type qa --site … --tags …`
      passes the qa prefix; `--type viral` requires `مقطع: ` + Title.
- [ ] `design_qc.py titlepolarity --title … --srt … [--words … --splice …]`
      passes on the kept window.
- [ ] METADATA `## Type` matches the prefix used.

## Why

One qa-only prefix rejected clean viral Shorts. Type-aware templates keep
ear-locked titles and let G16 pass both shapes without a waiver.

## Sources

Field lesson from a content QA recut batch: viral units needed an
explicit G16 viral branch; qa kept the Q&A prefix; ear-lock stayed
mandatory for both.
