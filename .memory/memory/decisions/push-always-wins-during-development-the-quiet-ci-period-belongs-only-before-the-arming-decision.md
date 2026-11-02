An unpushed commit is unrecoverable if the session dies. A cancelled CI run is re-runnable at no cost but time. Those two costs are not comparable, and treating them as symmetric produces the wrong rule.

The incident: pushing every few minutes meant every PR Gate run on a branch read `cancelled`, because each push superseded the one before it. That made one question -- whether an intermittent Windows timeout was real -- unanswerable across four iterations, because the evidence was destroyed each time. The conclusion drawn from that was to hold pushes so a run could complete, and it was recorded on the tracker as guidance.

Two commits later R18, the unpushed-work Stop gate built earlier in that same session, blocked the turn: a branch carrying commits that exist on no remote is work only one machine can see. It was right and the recorded guidance was wrong.

The corrected rule:

- **During development, push always wins.** Cancelled runs are an acceptable price. #4538 had already settled this from the other direction: three of eight issues done and visible beats seven of eight done and stranded.
- **At the end, take one quiet period before the decision.** The run that decides whether a pull request is mergeable must be uninterrupted. That is one deliberate pause before the adversarial review and arming, not a standing practice.

The trap is that both halves are real. The inability to read CI through repeated cancellation is a genuine cost, which is what made the wrong rule plausible. The error was concluding the fix was to stop pushing generally, rather than to stop pushing at the single point where the answer matters. Watch for this shape elsewhere: a real cost observed in one place, generalised into a standing rule that trades away something strictly more valuable.

A second-order note worth keeping: `cancelled` reads as `pending` in every summary view. `gh pr checks` and the PR rollup show state without cause, so a run killed by your own push looks identical to one still going. Check `gh run list --json conclusion` before concluding CI has told you anything.