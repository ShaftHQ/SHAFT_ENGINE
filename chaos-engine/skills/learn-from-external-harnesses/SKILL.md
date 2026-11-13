---
name: learn-from-external-harnesses
description: >-
  Use when asked to scan, analyze, or learn from an external repository, skill set, or agent harness.
---
# Learn from external harnesses

## 1. Intake

1. Name the source (URL or path), the question being asked, and the local harness that would change. Verify the source is what it claims (real owner, not a fork or impostor).
2. Clone or fetch a **pinned revision**. Record the exact commit SHA (40-hex), the date, the default branch, and the release tag if any. Do not analyze a floating `HEAD` and then forget what you read.
3. Scope what you will read (README, skills, rules, commands, templates, hooks, docs, licence). For large trees, bound the inventory (docs, extension seams, deferred candidates) and record what you did not read, and why.
4. Maturity and safety check beside the licence: pre-release or preview status, breaking-change warnings, SAFETY or security notes, and whether it was ever audited.
5. Find prior studies of the same source first: research notes, `RESEARCH.md`-style logs, third-party notices, and **open and closed** issues or PRs. Treat this pass as a delta against the last pin, not a first look.

## 2. Inventory the local harness first

Before judging the source, inventory **what is already local**:

- The skill registry or index (every registered skill: id, path, trigger / "when to use", owner or family, links from call sites).
- Adjacent rules, references, commands, templates, and hooks that already own the same job.
- Anti-bloat and learning paths already in the harness (retirement, compression, eval gates, "ticket first" rules). Call those paths; do not restate them.

A local inventory that is skipped is a failed pass: you cannot claim "new" without evidence of "already have."

## 3. Inventory the source

List, with paths:

- Skills, rules, commands, templates, hooks, workflows, and notable invariants.
- How the source itself registers, versions, and retires them (if it does).
- Size and token cost of always-on surfaces (MCP schemas, always-loaded prompts, SessionStart dumps).

Pin quotes to file path + SHA. Do not invent features from a star count or a marketing README alone.

## 4. License, attribution, and provenance

Record, from primary files (`LICENSE`, SPDX on the host API, `package` / `pyproject` / `CITATION`):

- Licence name and SPDX (or `NOASSERTION` / missing). A missing or incompatible licence **blocks vendoring and blocks copying source**. Ideas may still be described, but only as original local prose.
- Copyright holder. Compatible licence is not permission to vendor a second stack.
- Provenance line: `source URL + commit SHA + date studied`. Carry this line into the output ticket.

Do not copy prompts, templates, adapters, or constants. Reimplement any adopted idea in original language that fits the local harness.

## 5. Mapping table (required)

For every candidate idea, one row, with evidence (local path, source path + SHA):

| Idea | Verdict | Evidence | Proposed local change | Effort / priority |
| --- | --- | --- | --- | --- |
| … | already-have / improve-existing / new / reject | … | … | … |

Verdict rules:

- **already-have**: the local harness already does this. Cite the file. Do not add a parallel skill.
- **improve-existing**: the idea is real and a named local file already owns the job. Propose the smallest edit to *that* file. Prefer this over **new**.
- **new**: no local file owns the job, the idea is worth a skill or module, and every anti-bloat gate in §6 passes. Default is *not* new.
- **reject**: licence, duplication, bloat, a measured failure the source itself walked back, a second runtime, or a surface the local harness already forbids. Give a reason.

An explicit **do-not-adopt** list is required, even if short.

## 6. Anti-bloat gates

All of these must hold. A failed gate turns a **new** row into **reject** or **improve-existing**.

1. **Prefer merge.** Adding a skill is the last option, not the first.
2. **Dedupe by name and by intent.** Two skills that fire on the same job are one skill, even if the titles differ.
3. **Every new skill is first-class:** registered in the local index or registry; linked from the call site that should load it; given a clear trigger ("use when…"); given an owner or family; given a removal or usage check (see §8). An untracked, unlinked, or unused skill must not be added.
4. **No orphans.** A file on disk that is missing from the index is a defect, including the one you are proposing.
5. **Size cap.** Keep a new or edited skill tight (aim under ~150 lines; do not exceed the harness's published skill-body budget if it has one). Put deep reference material in a linked reference, not in the always-loaded body.
6. **Always-on cost.** Do not add always-loaded routes, MCP tool schemas, or SessionStart bodies to pay for a study. On-demand only, unless the harness already requires that exact surface.
7. **Token budget.** Prefer a compact mapping table and a pointer over pasted upstream docs. Progressive disclosure: load source files as needed, do not dump trees into the transcript.

## 7. Output is a tracked proposal

The output of this skill is a **tracked ticket or draft-PR proposal**, not a silent edit to skills, rules, or the registry.

Dedupe the ticket: if an **open** adoption ticket for the same source exists, comment there. If the earlier ticket is closed or implemented, open a new delta ticket that links it rather than reopening it.

The ticket follows the harness's work-item or research-issue format if one exists. It includes: pin (URL + SHA), local revision studied, the previous pin it is a delta against, the mapping table, the do-not-adopt list, HOW and acceptance criteria for each adopt/improve row, licence, maturity and attribution, what was not read, and rollout phases. It does **not** authorize a merge by itself.

If the harness has an eval-gated or opt-in path for skill PRs, any later apply uses that path. Never auto-merge skill mutations. Never write a skill body "while you are here."

## 8. Periodic prune

On the harness's existing review cadence (or monthly, if none is written):

- Usage or trigger counts for skills and rules added from external studies.
- Zero-use items: retire, or write one sentence justifying keep.
- Dedupe again by intent. Compress or merge rather than accumulate.
- Feed back gaps found while running this skill into this skill, through the same ticket-first path.

This prune is part of the skill, not a second skill.

## Worked example (github/spec-kit)

Pin: `https://github.com/github/spec-kit` @ `2ee2b1e8005905db05f5717300a469b3068b908f` (release v1.4.0; MIT).

Local inventory first: a work-item skill already requires the same Spec Kit *headings*; an identity/principles file already plays the constitution role; plan-before-edit and a pre-done review already exist; a shallow earlier study note already said "do not vendor." Those are **already-have** or prior **reject** rows, not a reason to skip a deeper pass.

Sample mapping after a deep read:

- Ticket headings (FR, SC, stories): **already-have**.
- specify, plan, tasks, implement order: **already-have**.
- Converge (inventory FR/SC against the current codebase; checkboxes are not evidence): **improve-existing** (extend the pre-done review, no new tree).
- Cross-artifact analyze (duplication, coverage gaps, inconsistency): **improve-existing**, read-only.
- Bounded clarify with answers written back into the work item: **improve-existing**.
- Constitution Check table on the plan, citing the live principles file: **improve-existing**; reject a second constitution document.
- `specify` CLI, `.specify/`, MCP server, host-integration pack, workflow engine, copied long command prompts: **reject**.
- A reusable learn-from-external-harnesses procedure: **new**, only if §6 gates pass.

The deliverable of that pass is the tracked adoption issue, not a vendored clone.
