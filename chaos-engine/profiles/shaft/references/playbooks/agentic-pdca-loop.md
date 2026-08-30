# PDCA

Personas are phases, not agent identities. They run sequentially in the main
session. PDCA does not select an execution mode. Select one through
[execution workflows](../../../../references/execution-workflows.md), then keep
these phases sequential inside it.

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

Focused RED-GREEN-REFACTOR runs occur during PDCA Do; consolidated Check begins
only after the implementation batch and final scope commit.
