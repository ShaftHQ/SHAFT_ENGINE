Commit and push each logically complete, independently-compiling and independently-testing
increment of work as it is finished, rather than batching everything into one commit at
session end. The branch and PR shape those increments land in is not decided here -- see
`.agents/skills/act-as-mohab/references/work-github-playbook.md` Sec. 3b. This object is
only about commit and push cadence inside whatever shape that section selects.

In the originating session (#3409, PR #3411) that meant a separate commit and push for:
(1) verbose-detail parser enrichment plus the toggle-safety fix together (they touched the
same two files and were verified together), (2) the new LocalAgentApprovalBridge class and
its own tests, (3) wiring the bridge into the runner, (4) wiring it into the panel -- each
pushed immediately after its own compile and test pass, before moving to the next piece.
