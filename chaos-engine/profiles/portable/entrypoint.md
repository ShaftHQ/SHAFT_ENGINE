# Portable project profile

Load the canonical ChaosEngine entrypoint first. This neutral profile derives
repository identity, branches, permissions, build commands, and companion
projects from the adopter's live files and explicit instructions. It never
assumes a product, organization, hosting provider, or repository layout.

Route project-specific work through the portable
[routing table](references/routing.md).
An optional [local coding delegate](../../skills/local-coding-delegate/SKILL.md)
may run a user-configured local loop when the decider model and a hardware
probe both say it adds value.
Keep canonical non-secret harness configuration tracked; keep generated
runtime state, indexes, reports, and caches untracked.
