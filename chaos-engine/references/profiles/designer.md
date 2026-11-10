# Designer profile

On-demand profile for the design lane (#7896). Not an always-loaded router route, and not a second policy authority. Core ChaosEngine policy stays the entrypoint. Load this file only when the task is design work. Installation without the design add-on stays on core ui-delivery.

## Lane

UI/UX, brand and image, typography and colour, motion graphics, video edit and grade, audio and voice, reels, and training-material visuals. Route one design card for the current step. Do not load the archive into every prompt.

This profile does not authorize media generation, publication, a cloud backend, or deletion. Orchestrator acceptance is not external-action authority. Perceptual checks (speaker identity, likeness, onset, dialect, full-frame readability) stay human gates on the add-on cards. They are not re-encoded here.

The 23 D-cards live in the design add-on. Of the four named workflow skills, training-deck design lives in that add-on; hookless entry reuses core bot-entry; the other two are private and deferred. The private will permalink is not published.

Landed lesson cards already under the add-on lessons directory are the lesson register. The 140-lesson synthesis is not pasted as new rules.

## Matrix

Every row is reuse of a path in this repo, or deferred (private source, not ported).

| Item | Disposition | Where |
| --- | --- | --- |
| 23 D-cards (D01–D23) | reuse | `chaos-engine/addons/design-skills/SKILL.md` and `chaos-engine/addons/design-skills/references/` |
| Training-deck workflow skill | reuse | `chaos-engine/addons/design-skills/references/craft/training-decks.md` |
| Hookless bot-entry workflow | reuse | `chaos-engine/references/bot-entry.md` |
| delegate-via-grok-cli | deferred | Private local handoff, not ported |
| automatest-voice | deferred | Private local handoff, not ported |
| Objective QC | reuse | `chaos-engine/addons/design-skills/scripts/design_qc.py` |
| Add-on install contract | reuse | `chaos-engine/references/addons.md` |
| Landed design lesson cards | reuse | `chaos-engine/addons/design-skills/references/lessons/` (not restated) |
| Private raw knowledge dump | deferred | Private source, not ported |
| Private will | deferred | Private source, not ported. The private will permalink is not published. |
