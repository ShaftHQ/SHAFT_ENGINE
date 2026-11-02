# Knowledge bundle from sources — ideas and their evidence, shaped to build from

Read this when a bundle will feed artifacts (a lecture, course, script, article, doc set) and the wording must stay the speaker's. Sources go in: transcripts, interviews, notes, papers. The bundle holds the ideas, the speaker's words behind each, where each one stops, and which artifact uses which. The artifacts are built later, by a pipeline. Do not restate the invariants or the other forms; they live in `SKILL.md` and `forms.md`.

Two roles. The **speaker** is whoever's words are in the sources. The **author** builds the artifact and answers for it. Often they are the same person. When they are not, "in their words" means the speaker's, and every list of open questions splits in two: what only the speaker can supply, and what only the author can decide.

## Size it to the corpus

| Corpus | Build |
|---|---|
| one short source, one artifact | an outline; stop |
| a short corpus feeding several artifacts, or whose wording must stay the speaker's | a light bundle: `sources/`, `concepts/`, `plans/`; `claims/` only when a claim leans on an outside fact. One root `CONTEXT.md` can hold the shelf contracts |
| long sources (tens of thousands of words), or several artifacts sharing ideas | the full tree below |

## Target tree

```
<subject>/
├─ AGENTS.md                 catalog: what artifacts exist, where to start
├─ _meta/
│  ├─ schema.md              note types, citation format, privacy (from bundle-schema.md)
│  ├─ sources.txt            optional: short citation key = raw file name
│  └─ keep-out.txt           phrases that must never appear outside sources/raw/
├─ _templates/               concept.md, source.md, plan.md, claim.md
├─ _tools/                   quotes.py, links.py, index.py
├─ sources/                  CONTEXT.md, _index.md, raw/ (verbatim, never edited), one note per source
├─ concepts/                 CONTEXT.md, _index.md, one idea per file
├─ plans/                    CONTEXT.md, _index.md, one artifact per file, plus map.md
└─ claims/                   CONTEXT.md, claims to verify, lines to rework (only if there is something to put in it)
```

Each shelf's `CONTEXT.md` (from `stage-CONTEXT.md`) says what a note there carries, what it reads, and how it is checked. Each `_index.md` is generated: `python -I _tools/index.py sources concepts plans claims`. Do not create shelves for types you will not use; add `asks/`, `stories/` or `reading/` only when someone will query them.

The entry file routes: building an artifact → its plan, then only the concepts it names; the exact words → the concept's "In their words" (the raw copy is in `sources/raw/`); about to say a number or a general claim → `claims/`; something missing → the plan's Open list (ask, do not fill); adding an idea → copy `_templates/concept.md`, then run the checks; status → the `status:` line in each plan.

## Notes

Citations follow `_meta/schema.md`, the one home for the format: `"verbatim line" (key position)`.

- **Concept** (`concept.md`): the idea stated once, in two or three sentences, saying only what the quoted lines say. **In their words**: verbatim lines, each cited, trimmed to the clean part and never tidied. **Where it stops**: the speaker's own limits and hedges; the loud version of an idea is rarely all of it. **Note**: your reading, marked as a proposal; delete it if you have none. **Related**: other concepts and the plans that use it.
- **Source** (`source.md`): how to cite it, the words the transcription gets wrong with their fixes, **Gaps** (where it stops short or holds only one voice), and what stays out. Say what and where; the phrases themselves go in `_meta/keep-out.txt`.
- **Claim** (`claim.md`): one block per outside fact: the speaker's quote with its citation, what it leans on, how and when it was checked, a status, and the safe line. The speaker's words never share a line with your verdict.
- **Plan** (`plan.md`): who it is for and what they arrive with, the one idea, what they leave able to do, an opening that orients before it hooks, the spine (concepts in order, each with why it is there and whether it gets the full treatment or a line), the speaker's best lines copied from the concepts, the real thing to show, the claims that apply, and **Open**, split between the speaker and the author. A plan holds a route, not the artifact; if the script is in the plan, move it out. One artifact per plan. If there are several, `plans/map.md` covers the set: the few ideas everything rests on, the artifacts in a suggested order with status, and the decisions only the author can make.

One home per fact, so a gap is written once. A verifiable fact lives in `claims/`. A gap in a source lives in that source's note. A question only the speaker can answer lives in the plan's Open list. Concepts link to these; they do not repeat them.

## Build pipeline (checked slices)

Stop after each slice. A person reads the output before the next slice starts. This pipeline replaces Build mode steps 1 and 3 to 5.

**0 — Frame.** Ask what will be built and for whom. Ask who the speaker is, who the author is, and who answers each check. Ask whose words must survive, what must not be said (names, embargoes, client confidences), and what a citation points at (a time, a page). List the sources and the artifacts. Propose the note types and get approval.

**1 — Set up and bring the sources in.** Copy each source into `sources/raw/` untouched, and write one source note each, leaving its Parts table for slice 2. From this skill, copy `assets/templates/bundle-schema.md` to `_meta/schema.md`, `assets/templates/keep-out.txt` to `_meta/keep-out.txt` (names and anything the speaker asked to keep out go in now; matching is whole-word, so list each form: `budget`, `budgets`), `assets/templates/entry.md` to `AGENTS.md` (plus the one-line pointer for each agent that reads another name, per `references/core.md`, Naming), the cards you need to `_templates/`, and `scripts/quotes.py`, `scripts/links.py` and `scripts/index.py` to `_tools/`. `quotes.py` should pass before any concept exists.

**2 — Read all of it.** Read every source whole before writing a note; sampling gives a map of the opening. List the distinct ideas each source makes, once each, and cluster them by how the artifact will ask for them, not by where they appeared. Write a stub for every planned note, concepts, plans and claims alike (frontmatter with `status: stub` and a one-line description, and the title as the first heading), so links resolve from the first batch. Fill each source note's Parts table. Present the cluster list. The speaker or author adds, merges, drops.

**3 — Concepts.** Fill the stubs one batch at a time, and write each shelf's `CONTEXT.md` when you first fill that shelf. After every batch run `python -I _tools/quotes.py` and `python -I _tools/links.py`, and fix the note, never the raw source. Read the warnings, and read the cuts with `--show-cuts`: a cut can change a meaning, and `quotes.py` fails one that drops a negation. The check: a reviewer reads three notes. Does it sound like the speaker? Does it say what they meant?

**4 — Claims.** For every outside fact a concept leans on: if you can search, verify it, write the safe version of the line, and record how and when it was checked. A search summary is a lead, not a source; say so. If you cannot search, write the claim, what would settle it and where to look, as a to-look-up list for the person. Either way, a claim that is not verified is marked "not checked," dated, and kept out of the plan's best lines. List the lines to rework (tone, transcription errors) and what stays out. Do this before any plan.

**5 — Plans.** One per artifact, then `plans/map.md` if there are several. If the sources are the speaker's and not the audience's, write the audience's question in "Arrives with" as an assumption and list the audience's own words under Open. The author reads the plans before any artifact is written. The bundle stops here; building the artifact is a Pipeline's job, and its first stage reads one plan and the concepts that plan names, never the whole bundle.

**6 — Walk it.** Regenerate the indexes and run the walk test below. No stub should be left.

## What the tools enforce

`quotes.py` fails on a quote not found near its cited position or on its cited page; a citation with no timestamp or page; an unknown source key; a bullet under "In their words" with no cited quote; a cut that drops a negation or leaves a piece too short to verify; and a keep-out phrase anywhere outside `sources/raw/` and `_meta/` (frontmatter, indexes and entry files included). It warns on a quote found in a neighboring paragraph and on eight or more quoted words with no citation. `links.py` fails on any link that does not resolve.

A script can prove a quote is in the source. It cannot prove a paraphrase is fair, or that a synonym of a kept-out phrase is absent; that is what the reviewer's three notes are for.

## Defining moves

- **Quotes are word for word, cited, and checked by script.** A quote `quotes.py` cannot find is a typo or an invention; either way it does not ship.
- **The speaker's words and yours never share a line.** Verbatim sits under "In their words". Your reading sits under "Note" and is a proposal.
- **One idea, one note.** Plans and other notes link; they do not copy.
- **Gaps are named, not filled.** Open lists name what only someone can supply: a story the speaker has not told, a number they have not given. Do not write it for them. Where the sources disagree with each other, say so in the concept's "Where it stops" and ask; do not reconcile them.
- **People are described by role, never named** (the privacy rule in `_meta/schema.md`). Names live only in the raw copies, and later agents read the notes, never the raw copies, so a name cannot travel.
- **Facts are checked before the plan, not after the artifact.**

## Walk test (this variant)

A cold agent, no memory of the sources:

1. Open `AGENTS.md`. Can it say what artifacts exist and where to start?
2. Open one plan. Can it list the spine and the speaker's best lines without opening a raw source?
3. Do `quotes.py` and `links.py` pass, with no stub left in any `_index.md`?
4. Open one concept. Does the unmarked paragraph say only what the quoted lines say? Anything beyond them sits under Note.
5. Does any plan answer an Open item itself?
6. Does one plan plus the concepts it names stay under about 8,000 tokens?

If a step fails, split or move files. Do not add a "how to read this bundle" note.

## Failure modes

- Summarizing the source instead of splitting it into ideas: the bundle becomes a second transcript.
- Quotes tidied "for readability," so they stop matching.
- The same idea restated per artifact. One home.
- A plan that contains the script.
- Only the loud version of an idea: no "Where it stops."
- Smoothing over a disagreement between sources instead of asking.
- Filling a spine with stories the sources do not contain.
- Names, client details, or embargoed lines in notes.
- Claims checked after the artifact.
- Trusting the scripts for what they cannot see: whether a paraphrase is fair.
- Note types multiplying past what anyone queries.
- A hand-edited `_index.md`.
