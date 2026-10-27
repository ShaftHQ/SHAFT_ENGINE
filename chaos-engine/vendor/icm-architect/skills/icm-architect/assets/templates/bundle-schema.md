# Schema - the rules of this bundle

The closed set of note types, how quotes are cited, and what may never appear. When practice and this file disagree, reconcile the same day.

types: concept, plan, source, claim

Add `ask`, `story` or `reading` to this list only when someone will query them.

## Note types

| `type:` | Lives at | Carries |
|---|---|---|
| source | `sources/<slug>.md` | how to cite, errors and fixes, gaps, what stays out |
| concept | `concepts/<slug>.md` | one idea, the speaker's words, where it stops |
| plan | `plans/<slug>.md` | one artifact's route through the concepts |
| claim | `claims/<slug>.md` | claims to verify, lines to rework |

Every note's first heading is its title. A planned note that is not written yet is a stub: its frontmatter (`status: stub`, a one-line description) and its title. Links to it resolve from the first batch, and `_index.md` shows it as a stub until it is filled.

## Citations

`"verbatim line" (key position)`. The key is the raw file's name without its extension, or a short name declared in `_meta/sources.txt` as `key = filename`. Position for a transcript is `mm:ss` or `h:mm:ss` (or a range, `12:30-13:05`); cite the timestamp that opens the paragraph. Position for a document is a page (`page 2`, `pp 4-5`); a document with `Page 1`, `Page 2` lines is checked by page, and any other document is matched anywhere. Paraphrase takes no quote marks. To keep a name out of a quote, cut it with `...`; never swap a word inside the marks. `_tools/quotes.py` checks every quote.

## Privacy

People are described by role, never named, in every note, with neutral pronouns unless the source states them. Phrases that must never appear go in `_meta/keep-out.txt`, one per line; `_tools/quotes.py` fails on a whole-word hit anywhere outside `sources/raw/` and `_meta/`, so list each form of a word. Keep that file out of anything that is published.

## Naming

Slugs are kebab-case. One idea per file, and the filename is the idea. Each shelf has a `CONTEXT.md` (its contract) and an `_index.md` written by `_tools/index.py`.
