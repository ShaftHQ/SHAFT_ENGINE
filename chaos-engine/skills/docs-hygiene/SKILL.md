---
name: docs-hygiene
description: "Use when editing source-of-truth docs or status tables: three sources, Done / In progress / Not started, and live hour formulas. On demand only."
license: MIT
permissions:
  - read
---

# Docs hygiene

Load this skill when the deliverable edits the three source-of-truth documents, a status table, a journey map, or hour totals. On demand only. It does not publish product docs.

Run `python3 .chaos-engine/tool.py retrieve --store graphify "docs hygiene status journey"` before a broad search and record `retrieve: used|skipped(<reason>)`.

## Three source-of-truth files

Name the three source-of-truth files at the start. Edit those files. A summary, a slide, or a chat note is not a fourth source of truth.

## Status vocabulary

Status cells use exactly one of: **Done**, **In progress**, **Not started**. Do not invent synonyms in the table.

## Hours

For every Done row, completed hours equal planned hours. In progress and Not started rows are not forced to that equality. The cells are live formulas, not pasted numbers. A hand-typed total that can drift from the plan is a failure.

## Journey maps

Journey maps agree with each other and with the source-of-truth status. Do not merge a partial journey as the full journey. A partial map stays labeled partial until the missing legs exist.

## Delete-guard upstream

If a delete-guard requires an upstream, push a start commit first, then delete in a follow-up. That is an allowed exception to a single push. Never use `--no-verify`, and never edit allowlists to bypass the guard.

## SHA-bound review

A review claim cites the head SHA it read. A review of an older SHA is not a review of the current head. Re-read after every new commit.

Enforced by [`lessons_2026_11_06.py`](../../rules/lessons_2026_11_06.py) (`evaluate_docs_hygiene`). The closed ticket this skill completes stays closed; do not refile it.
