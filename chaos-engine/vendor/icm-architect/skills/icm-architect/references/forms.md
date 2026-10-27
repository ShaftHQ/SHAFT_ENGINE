# The forms

Each form is a recipe over the three primitives in SKILL.md: sequence, shelf, catalog. What changes is the repeating unit and what the structure optimizes for. All of them are drawn from production workspaces, not theory. The table that picks one lives in SKILL.md (Build mode, step 2).

Contents: 1 Pipeline · 2 Record library · 3 Knowledge bundle · 4 Context map · 5 System map

## 1. Pipeline — the production line

The paper's canonical shape. The same sequence runs weekly or daily with different input, every stage ends at a check, and a deliverable leaves at the end.

```
workspace/
├─ AGENTS.md               identity + routing table
├─ CONTEXT.md              the pipeline in one screen
├─ stages/
│  ├─ 01_research/   {CONTEXT.md, references/, output/}
│  ├─ 02_script/     {CONTEXT.md, references/, output/}
│  └─ 03_production/ {CONTEXT.md, references/, output/}
├─ _shared/                factory: voice.md, design-system.md
├─ _tools/                 scripts the stages call
└─ setup/questionnaire.md  configures the factory once
```

**Defining moves:**
- Handoff = one stage's `output/` is the next stage's input. A person (or the stage's named check) reads the file in between; the next stage reads whatever is there.
- Each contract carries a "load this / do NOT load that" inputs list.
- `status` is answered by scanning `stages/*/output/` for files and any `check.md` verdicts.
- Stage boundaries sit where the person naturally pauses to check. Surfacing the judgment call (an outline, a structural plan) as an editable file *before* the expensive downstream work is the whole trick: correction is cheapest at the earliest check.
- Several pipelines that share one brand or voice (an Umbrella) each keep their own entry file and share no state except the factory at the root. The root entry file routes by task ("a talking-head video: 03; animating a diagram: 05") and holds nothing else.

**Expect a U-curve of human editing:** heavy at the first stage (direction-setting), light in the middle (constrained by both anchors), heavy at the last (aligning output with earlier decisions). Design the first and last outputs to be especially easy to edit.

**Watch for:** stages that do two jobs (split them); contracts that restate reference material (point instead); pipelines built before the process has actually repeated (don't); shared reference copied into each pipeline of an Umbrella (link up instead).

## 2. Record library — the unit is a record

Nothing "runs" to completion; records (people, clients, sessions, deals) get created, accumulate, and are looked up. The structure optimizes retrieval and uniform shape.

```
workspace/
├─ AGENTS.md               identity + routing
├─ CONTEXT.md              the record shape and its lifecycle
├─ _shared/                factory: the method, rules, shared knowledge
├─ _templates/
│  └─ record-template/     the stamp: every record starts as a copy of this
├─ _tools/index.py
└─ records/
   ├─ _index.md            generated: one line per record, with its status
   ├─ acme-corp/           each record the same internal shape
   └─ jane-doe/
```

**Defining moves:**
- **A new record is a copy, not a blank page.** The template *is* the schema.
- Each record states its own status in one line (`status:` in the frontmatter of the record's `CONTEXT.md`), and the statuses form a small lifecycle (`briefed → active → archived`). `records/_index.md` is generated from them: `python -I _tools/index.py records`.
- Naming convention doubles as an id scheme (`ht10-second-brain`: type + counter + slug).
- Records can recurse: each record may internally be a mini knowledge bundle or pipeline (a person-record holding its own layered brain; a session-record holding its own six-stage line). Uniformity across records is what makes the library queryable.

**Watch for:** records drifting from the template shape (re-stamp them); status kept anywhere but the record itself; half-created records with only one file (finish the stamp or archive it).

## 3. Knowledge bundle — the product is the knowledge

The deliverable is a navigable body of knowledge: a brain, a domain wiki, a model of a person or subject, or the ideas an artifact will be built from. Often filled by a pipeline from sources (factory and product as two separate top-level trees).

```
workspace/
├─ AGENTS.md
├─ corpus/                 raw sources; a source with no note in the bundle is unprocessed
├─ extraction/             the factory: an ICM Pipeline whose output is the bundle
└─ bundle/                 the product:
   ├─ index.md             what's in here, layer by layer
   ├─ voice/  (layer A)    always-load essentials
   ├─ dispositions/ (B)    load-by-task
   └─ episodes/ (C)        evidence, loaded last, access-tiered
```

**Defining moves:**
- Every note carries typed YAML frontmatter (`type:`, `layer:`, `access_tier:`, `strength:`): labels make it queryable, links make it a graph.
- Notes cross-link by relative path; navigation is link-following, not folder-crawling. A note that is planned but not written yet is a stub (frontmatter only, `status: stub`), so links to it resolve from the start.
- Layered loading is the reading protocol: always-load layer first, task-relevant notes second, evidence only when needed. Never slurp the bundle.
- `access_tier` gates what may leave the machine: patterns abstracted from private sources are fine; raw quotes are not.
- Regenerating the bundle is a factory run; every change appends to a log.

**From sources.** When the bundle feeds artifacts (a lecture, course, scripts, docs) and the wording must stay the speaker's, fill it from `sources/raw/` (verbatim, never edited) into `concepts/` (one idea each, quotes cited `(key position)`), `claims/` (outside facts to verify) and `plans/` (one artifact's route through the concepts). `_tools/quotes.py` fails any quote it cannot find in its source. Method: [bundle-from-sources.md](bundle-from-sources.md).

**Watch for:** the bundle read as a search index instead of a model (it answers "how does this think," not "find me the file"); frontmatter fields nobody queries (cut them); extraction runs that edit the product by hand (fix the factory); quotes tidied until they no longer match their source.

## 4. Context map — the organization as a graph

The subject is a company or team: who does what, what data moves where, what's ripe for automation. Nodes + labels + links rather than stages.

```
workspace/
├─ AGENTS.md               entry
├─ CONTEXT.md              how to walk the map
├─ _meta/                  the rules: schema.md, maturity-levels.md, ritual docs
├─ teams/
│  └─ marketing/
│     ├─ Marketing.md      node card: In / Movement / Out / Edges
│     ├─ governance.md
│     ├─ jobs/             outcome nodes
│     ├─ processes/        workflow nodes (the workhorses)
│     └─ data/             data-<thing>.md asset nodes
├─ patterns/               cross-team patterns, written bottom-up only
└─ dashboards/             00-tracker.md … live queries over frontmatter (Obsidian Dataview, if the map is a vault)
```

**Defining moves:**
- A closed set of node types (team, job, process, data-asset, governance, pattern) defined once in `_meta/schema.md` (from `context-map-schema.md`); every node declares its `type:` in frontmatter.
- Process nodes (`context-map-node.md`) carry the scoring frontmatter: owner, ai-level (L0 manual → L3 integrated), frequency, value 1–5, pain 1–5, `consumes:`/`produces:` as paths to data assets. The links draw the org graph on their own (`_tools/links.py --graph`, then `viz.py`); high value + high pain = pilot candidate.
- Each folder's `_index.md` is generated by `_tools/index.py`: agents jump there, never crawl.
- **The workshop is the data event.** Map live with the team; every session ends in clean node files, not slides. "You don't point an agent at a legacy mess; you clean the shelf first."
- The librarian ritual per team: inventory → single source of truth → give it shape → catalogue → shelve by sensitivity → connect the agent. The person stays the approval check; the agent drafts and proposes.
- Patterns require three independent occurrences: one team complaining is a gripe, three teams landing on the same workflow and the same pain is structure.

**Watch for:** schema mandating names the files stopped using (reconcile immediately); instance data tangled into the reusable method (extract the blank starter kit early); node types multiplying past what anyone queries.

## 5. System map — a body of work as an edit graph

The subject is a tree someone will change: a repository, a markdown vault, or both. Nouns become object cards, verbs become process cards, and `effects/` is an index into them, so a later agent can answer "what is this" and "what else moves" without slurping the tree. Its audit pipeline, card sections and walk test: [system-map.md](system-map.md).
