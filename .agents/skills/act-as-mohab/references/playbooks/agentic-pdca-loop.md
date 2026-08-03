# PDCA

Personas are phases, not agent identities. They run sequentially in the main
session, whose orchestrator never edits:

- Kevin phase plans spec, value, acceptance, risks, and any useful Mermaid or wireframe.
- Bob phase dispatches and shepherds a bounded default-capability implementation
  owner, which makes the smallest cross-platform change through observed TDD.
- Bruce reviews the actual diff and evidence for defects, ambiguity, and
  confidence, then assigns any required patch to an implementation owner.

Do not merge phases. Run Kevin -> Bob -> Bruce, then repeat that order for two
passes: quality/simplicity, then intuitiveness/acceptability. Rerun the
smallest check each pass. Stop at >=90% confidence or a blocker.

## Execution

Switch persona by switching main-thread phase, never by creating persona
agents, workflows, or orchestrators. Bob shepherds at most three implementation
rounds. Bruce judges the actual diff plus real checks, never an owner's
self-report; hunt stubs and weakened assertions, and assign gaps rather than
closing them on the main thread. Record which phase produced each commit.
