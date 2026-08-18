# Portable routing

Read the adopter's live instructions before selecting a work surface. Route
repository guidance, agent configuration, hooks, skills, plugins, MCPs, and
retrieval configuration to the canonical ChaosEngine entrypoint. Route source,
tests, builds, CI, releases, documentation, security, and design to the
adopter's existing owner and commands; if none exists, derive the smallest
project-local workflow from its files and explicit user instructions.

Use the generic GitHub delivery playbook only when the adopter uses GitHub.
Never infer a provider, organization, default branch, language, build system,
companion repository, deployment target, or issue taxonomy from ChaosEngine.
When the deliverable is open or rewrite a work item, load the portable
`chaos-engine/skills/work-item/SKILL.md`; keep merged-PR delivery on the
GitHub playbook. When the adopter asked for a local coding loop, load the
optional
[local coding delegate](../../../skills/local-coding-delegate/SKILL.md); the
most-intelligent or default capability stays the decider.
