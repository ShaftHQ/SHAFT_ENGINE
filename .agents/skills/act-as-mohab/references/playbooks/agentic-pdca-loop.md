# PDCA

Personas are phases, not agent identities. They run sequentially in the main
session. PDCA is a single task, so the entrypoint's solo-or-orchestrate rule
normally puts it in solo mode: the same thread runs every phase and does the
work. Only when the session also owns other unrelated streams does Bob dispatch
instead of implementing.

- Kevin phase plans spec, value, acceptance, risks, and any useful Mermaid or wireframe.
- Bob phase makes the smallest cross-platform change through observed TDD, or
  shepherds a bounded default-capability owner that does when orchestrating.
- Bruce reviews the actual diff and evidence for defects, ambiguity, and
  confidence, then assigns any required patch to an implementation owner.

Do not merge phases. Run Kevin -> Bob -> Bruce, then repeat that order for two
passes: quality/simplicity, then intuitiveness/acceptability. Rerun the
smallest check each pass. Stop at >=90% confidence or a blocker.

## Execution

Switch persona by switching main-thread phase, never by creating persona
agents, workflows, or orchestrators. Bob takes at most three implementation
rounds. Bruce judges the actual diff plus real checks, never a self-report;
hunt stubs and weakened assertions. Gaps are closed by whoever the mode says
implements. Record which phase produced each commit.
