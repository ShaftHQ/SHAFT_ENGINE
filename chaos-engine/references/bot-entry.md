# Bot entry

Entry point for agents that auto-load nothing from the checkout and run no
ChaosEngine hooks: Grok Bot, custom GPTs (OpenAI GPTs, Assistants), Claude
Projects, and any other independent bot. `AGENTS.md` is not read for you.

## Every task, before discovery

1. With a shell: run `python3 .chaos-engine/tool.py entry` and follow what it
   prints. It bundles `identity.md`, the core card
   (`skills/chaos-engine/SKILL.md`), and both companion cards
   (`companions/caveman-ultra.md`, `companions/ponytail-ultra.md`).
   Without a shell: read those four files directly.
2. Run one `python3 .chaos-engine/tool.py retrieve --store graphify|mempalace "<q>"`
   before the first broad search or unnamed read
   ([retrieve-first](retrieve-first.md)).
3. Record `retrieve: used|skipped(<reason>)|exempt(harness)` in the
   [research receipt](research-receipt.md).

Re-runs print one line while the harness is unchanged; `--full` reprints the
cards. Keep one delivery per thread and write a handoff note (state, open PRs,
next step) at each delivery boundary instead of carrying a long transcript.

## Bots without repository access (GPTs)

Save `python3 .chaos-engine/tool.py entry > chaos-engine-entry.md` and load
that file as the bot's instructions or knowledge. Regenerate it after every
ChaosEngine update; a stale copy is a stale harness.

## Chat form

Hosts whose chat demands full sentences keep them; caveman's content rules
still apply: no filler, preamble, or narration, and each fact stated once.
