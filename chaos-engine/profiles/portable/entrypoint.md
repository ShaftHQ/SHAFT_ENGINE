# Portable project profile

Load the canonical ChaosEngine entrypoint first. This neutral profile derives
repository identity, branches, permissions, build commands, and companion
projects from the adopter's live files and explicit instructions. It never
assumes a product, organization, hosting provider, or repository layout.

Route project-specific work through the portable playbooks in `references/`.
Keep canonical non-secret harness configuration tracked; keep generated
runtime state, indexes, reports, and caches untracked.
