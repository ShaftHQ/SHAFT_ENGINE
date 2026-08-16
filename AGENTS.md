# AGENTS.md

Before every task, load [.agents/skills/chaos-engine/SKILL.md](.agents/skills/chaos-engine/SKILL.md).

That adapter routes to the canonical portable ChaosEngine skill and the selected SHAFT profile.
The canonical skill is the single source of behavioral guidance. The profile is the single source
of repository-specific facts. References add detail only when routed; they cannot add gates or
override the canonical skill.

Local hooks are disabled and advisory code cannot block tools, mutation, delivery, or completion.
Host-native permissions, repository rulesets, and explicit user authority are the enforcement
boundary.

Read current files, preserve unrelated work, use relative operational paths in tracked guidance,
and report only observed delivery and validation state. Do not duplicate workflow policy in host
adapters or repository instruction files.
