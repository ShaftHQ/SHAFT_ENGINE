---
description: Use when a change alters a command, flag, default, or other reader-facing contract, including work already done in the session.
---

# Docs follow the change

A reader-facing contract is a command, flag, default, inference rule, or
behavior a person or agent is told to rely on. When that contract changes,
every surface that states it is updated before the delivery is done. The rule
covers changes already made in the session, not only the edit in front of you.

A refactor that leaves the stated contract unchanged does not get a
documentation edit.

## Surfaces

1. Same repository: the markdown that already shows the contract. For the
   ChaosEngine installer, that is the section directly under the one-liner in
   [README.md](../README.md#clients-this-command-can-complete) and
   [INSTALL.md](../INSTALL.md#clients-this-command-can-complete). Those pages
   ship in the same pull request as the code.
2. Companion documentation repository: the external-services inventory row in
   the origin [README.md](../README.md). Discover that checkout. Do not assume
   a sibling path, and do not edit a stale local branch. Branch from that
   repository's default branch. The installer page is
   `docs/agentic/chaos-engine.mdx`, with the same section under the one-liner.
   That repository gets its own pull request in the same delivery. The portable
   tree does not spell the product name; the origin README does.
3. A second page that repeats the command gets a pointer to the canonical
   section, or the same facts. It does not keep an older contract.

## Proof

The installer pages name every client id, every inference variable, and the
explicit `--hosts` / `-Hosts` / `CHAOS_ENGINE_HOST` forms under the one-liner.
`tests/scripts/test_chaos_engine_host_completion_7813.py` fails when that
section is missing or sits above the command.
