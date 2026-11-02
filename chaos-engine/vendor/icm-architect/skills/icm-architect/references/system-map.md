# System map — audit a body of work so later agents can edit it

Read this when the repeating unit is **a folder someone will change**: a repo, a markdown vault, or a mix. The deliverable is not a run and not a company chart. It is a walkable graph: what the nouns are, how they move, and what else moves if you touch one.

Do not restate the invariants or the other forms. Those live in `SKILL.md` and `forms.md`.

## When to use it

The user wants to work on a tree they do not hold in their head: "map this repo," "audit this folder," "what would a change hit," "make this editable for the next agent." The tree may be code, markdown, or both.

Do **not** use this form when they want a repeating production line (Pipeline), a company or team graph (Context map), or a model of how someone thinks or the ideas behind the code laid out to teach from (Knowledge bundle).

If the tree is small enough that one `CONTEXT.md` plus an index answers "what is X" and "what else moves," stop there. Do not scaffold `objects/` / `processes/` / `effects/` for a dozen files.

## What the map is

A **record library of nouns** plus a short shelf of **verbs**, plus a **change-impact index**. The subject tree stays the source of truth. The map cites it. The map never becomes a second spec.

| Universe | Meaning |
|---|---|
| **live** | In force. Implement and cite against these. |
| **leftover** | Still present, no longer the main path. Touch only if that path is in scope. |
| **ghost** | Named or filed, not wired (stubs, dead types, docs of functions that do not exist). Do not implement against these. |

Product language and file/type names often disagree. State both once in the catalog ("Chat = `Incubator`").

## Where it lives

Propose before writing. Prefer a `map/` shelf next to existing orientation (`developer-docs/`, `docs/`, vault root) so the subject's entry file (`AGENTS.md`, `CLAUDE.md` or whatever it already uses) can add one routing row. Do not drop a map inside `src/` or scatter cards through the tree you are mapping. The map's entry file is `map/AGENTS.md`.

## Target tree (smallest that carries the work)

```
map/
├─ AGENTS.md              L0 catalog
├─ CONTEXT.md             how to walk; the universes; name collisions
├─ _meta/schema.md        closed node types
├─ _templates/            object.md, process.md
├─ _tools/                index.py, links.py
├─ objects/
│  ├─ CONTEXT.md
│  └─ <cluster>/          cards, plus a generated _index.md (one line per card, with its status)
├─ processes/             only after nouns exist; only real movements
└─ effects/CONTEXT.md     which cards to open before a change
```

Do not create `processes/` or `effects/` empty. Three verified noun clusters beat seven imagined shelves.

## Audit pipeline (checked slices)

This pipeline replaces Build mode steps 1 and 3 to 5; it never moves the subject's files. Stop after each slice. A person or a cold walk reads the output before the next slice starts.

### 0 — Inventory, do not write cards

List the tree. Classify each area: catalog / contract / factory / product / dead (Restructure mode). Infer nouns (types, durable files, note kinds) and verbs (movements that repeat). Mark universe. Present the proposed tree and cluster list. Get approval.

On a code-heavy tree, an AST graph tool (e.g. `graphify extract --code-only`: local, no model) can draft the inventory. Its EXTRACTED edges carry `file L<line>` and can seed citations and Hits; INFERRED edges are proposals that pass a check. Never paste its report into a card, or install its assistant hooks in the workspace. On private material add `--backend ollama` and set `GRAPHIFY_QUERY_LOG_DISABLE=1`.

### 1 — Catalog

Write `AGENTS.md`, `CONTEXT.md`, the schema, templates, and a stub card (frontmatter only, `status: stub`) for every noun you will not fill yet. Run `python -I _tools/index.py objects/<cluster>` for each cluster. Wire one routing row from the subject's existing entry file. Walk: "where do I go to understand X?" lands in two hops.

### 2 — Nouns (objects)

One card per type, copied from `_templates/object.md`. Cluster by how an editor asks, not by folder layout.

**Source of truth**

- Code: the type / table / module. Cite `path:line`. If a comment and the code disagree, the code wins and the card says so.
- Markdown / mixed: the file that owns the fact. Cite path. If two notes disagree, pick one home and link.

`status: verified` only with a date, a branch or commit (or vault revision), and citations. `stale` is allowed. A confident wrong date is not. Regenerate the cluster's `_index.md` after a batch.

Fill **If you change this** as **Hits** / **Does not hit**. First-order only. "Does not hit" names the obvious next noun that is the *wrong* one.

Re-verify load-bearing claims against source before calling the slice done. Do not paste an audit report into a card.

### 3 — Verbs (processes)

Only movements that actually run or are actually followed. Typical code set: auth, publish/push, hydrate/load, run/turn, promote/export. Typical vault set: ingest, revise, publish.

Each process card: Input → Movement → Output; numbered steps with citations; `consumes` / `produces` as links to object cards; Hits / Does not hit.

### 4 — Change-impact index

`effects/CONTEXT.md` is a catalog: "if you are changing X, open these cards." It does not copy waterfalls. If the index and a card disagree, fix the card.

Then walk it backwards. The index answers "I am changing X, what inside the tree moves." It does not answer "what outside the tree points in." Ask the owner (configs, issue trackers, agents, scheduled jobs) for absolute paths into the subject, and record each one on the card it lands on. These consumers break silently: nothing in the tree references them, so no card names them until you go looking.

### 5 — Re-verify

After the first fill, check the load-bearing claims again (especially Hits / Does not hit). Wrong waterfalls are more expensive than missing cards.

## Object card (required sections)

1. One sentence: product name and code/file name if they differ
2. Why this shape: the load-bearing why, not a field tour
3. Shape: keys, constraints, or owning files, with citations
4. Connected to: owns / owned-by / joins / looks-like-but-is-not
5. If you change this: Hits / Does not hit
6. Surfaces: who reads/writes (apps, agents, people, none)
7. See: the source file; at most one as-built page

## Walk test (this form)

A cold agent, no memory of the subject:

1. Open the subject's entry file. Is the map one hop away?
2. Open `map/AGENTS.md`. Can it say what the colliding names mean without opening a card?
3. Open one object card. Does it cite source, state the why, and give a first-order waterfall?
4. From `effects/CONTEXT.md`, can it name what a stated change hits and what it does not?
5. Follow one `See` link. Does it land on source, not another essay?
6. Does the entry file, a hub and one card stay under about 8,000 tokens?

If a step fails, split or move files. Do not add a "how to read this map" novel.

## Failure modes

- Mapping FeatureRequests / aspiration as live. Ghost it.
- Copying as-built behaviour into cards. Point at the file that owns it.
- Empty process/effects folders.
- Cards with no citations marked `verified`.
- Slurping the whole `objects/` folder in a later session: the catalog exists so you do not.
- An `effects/` index that only walks outward. What points INTO the tree from outside is invisible from inside it.
