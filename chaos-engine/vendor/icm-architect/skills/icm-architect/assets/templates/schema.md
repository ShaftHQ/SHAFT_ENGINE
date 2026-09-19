# Schema — the rules of this workspace

The closed set of note types, the labels they carry, and the naming they follow. When practice and this file disagree, reconcile the same day — schema drift is how structures rot.

## Node types

| `type:` | Lives at | Carries |
|---|---|---|
| team | `teams/<slug>/{Team Name}.md` | In / Movement / Out / Edges |
| process | `teams/<slug>/processes/<slug>.md` | full scoring frontmatter (see node template) |
| job | `teams/<slug>/jobs/<slug>.md` | the outcome, who owns it |
| data-asset | `teams/<slug>/data/data-<thing>.md` | source of truth, shape, sensitivity |
| governance | `teams/<slug>/governance.md` | what may not be automated, and why |
| pattern | `patterns/<slug>.md` | only after 3+ independent occurrences |

## Labels that make it queryable

`type`, `team`, `owner`, `ai-level` (L0–L3), `value` (1–5), `pain` (1–5), `governance` (internal / sensitive / external). `consumes:` and `produces:` are wikilinks to data assets — the links draw the map on their own. value + pain ≥ 8 flags a pilot candidate.

## Naming

- Slugs: kebab-case. Data assets always `data-<thing>.md`.
- Human-browsed cards may use Title Case filenames — but declare that choice here and hold it everywhere.
- `_meta/` holds the rules (this file). Generated indexes are rebuilt by script, never hand-edited.
