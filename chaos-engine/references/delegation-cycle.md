# Delegation cycle

Peer agents share one instruction file. Chat carries a path and "read and execute", not the essay. This is the file-pointer cycle. In-process subagent dispatch stays in the existing delegation notes.

Enforced by [`lessons_2026_11_02.py`](../rules/lessons_2026_11_02.py) (`evaluate_status_board`, `evaluate_run_state`) and [`lessons_2026_11_06.py`](../rules/lessons_2026_11_06.py) (`evaluate_delegation_cycle`).

## Shared file

The router writes one shared instruction file from the human's words plus refinements. Every peer reads that file. Peer messages name the path. Two peers reading it see the same phases, templates, and pause rules.

## Status

An explicit status request always triggers a fresh poll of every registered agent. Cached reports do not build the board. Missing agents are `unknown`, not omitted. The board uses four tables: in progress (including token-interrupted work), planned (including frozen items), blocked (only after a this-turn live check), and done. Every row has an owner, a link or attachment path, and a priority. Numbered ids are opt-in: when the board sets them, every row has a stable incrementing numeric id. Rows are grouped by status and ordered by priority.

A row marked done with no link and no file is not Done.

## Proceed with N

"Proceed with N" tells only the owner of that id to resume that item. Frozen peers stay frozen. Nothing else unfreezes.

## Radio silence

Do not message the human except when they asked for a report or named ids to proceed. Publish and media gates still apply.

## Token restore is not a resume

Token restoration authorizes a reply, not a resume. Reply, then pause, unless the shared file names the next phase or the human named an id.

## Run-state before dispatch

Stop and freeze are recorded per agent as `frozen`, `active`, or `halted`. Check that record before every dispatch. Do not dispatch while the run-state is not active unless an unfreeze order names the id.

## Lessons, one PR, install, cleanup, Done

After the status cycle, each agent writes abstracted lessons (pattern, root cause, enforceable rule, acceptance, destination). Destinations are ChaosEngine, brain, mempalace, or graphify. The engineer reads them, writes an analysis, and opens one ChaosEngine pull request for the CE-bound lessons plus the open ChaosEngine bugs that belong in that cycle. Related tickets group into that mega PR. One writer per file; claim the file before editing it.

After that PR merges, every agent installs the new ChaosEngine. Cleanup of rebuildable caches starts only on the router's signal. Unique unpushed work stays. Deletes on a human's machine still need the human's own yes. The router sends one Done only after every cleanup report is in.

## Ledger

The 2026-11-06 transcripts live in [the lessons ledger](details/ce-lessons-2026-11-06/ledger.md). Upstream contribution close rules: [external contribution](external-contribution.md).

## One writer

No two workers edit the same file. Mega-PR grouping does not waive the claim.

## Who writes the code

Coach, Carl, and Chaos write the code themselves. Do not route implementation to a Cursor cloud agent or the Cursor bot. This owner's product checkout and ChaosEngine work is written on the owner's machine with Grok Build (`grok` CLI, `--prompt-file` or `-p`, `--always-approve`, `--cwd` the checkout). After a PR is opened, the public branch is `ChaosEngine/<short-slug>` and the body is a ChaosEngine body. Stripping provider chrome is cleanup after the fact. The default path does not create it.

Token-opt notes for 2026-11-06: [index](details/token-opt-retro-2026-11-06/README.md).
