# Delegation

Delegation distributes work, never responsibility. Select capability from
uncertainty, blast radius, and reversibility. Never bind policy to a vendor,
product name, or runtime setting.

## Capability levels

Every host exposes three levels. Refer to them only by these names.

### Most intelligent model

Architecture, cross-cutting or hard-to-reverse decisions, high-blast-radius
consultation, and independent adversarial review. It owns tradeoffs and returns
a decision or a spec; it does not become default labor.

### Default model

Bounded implementation, debugging, review, testing, docs, and normal research.
This is the standard choice. A default-model owner may assign only
fully-specified mechanical slices downward, and must inspect the returned work
before using it.

### Mechanical model

Spec-exact repetitive edits, inventory, formatting, deterministic
transformation, and log or result triage. The `helper` adapters carry the
mechanical-helper role from [roles](roles.md) to both subagent hosts.

## Main-thread duties

Orchestrator retains decomposition, architecture, consultation, assignment,
synthesis, integration, and final verification. Stay available to the owner
for realignment and delegate questions. The entrypoint's solo-or-orchestrate rule
decides whether it also implements; this file governs the orchestrated mode.

Default is one writer at a time, ordered by dependency then priority. On owner
request, parallelize only independent file scopes. Each writer owns an isolated
worktree. Hard cap four concurrent writing agents; the owner may set a cap of
1–4. Refuse a requested cap above 4. File-overlapping writers never run in
parallel even when parallel is requested. A read-only reviewer does not
consume a slot. Check real progress for any agent or
command unexamined for about twenty minutes, and supply a decision, a solved
subproblem, or a re-spec — never a heartbeat.

Treat agent lifetime as part of the assignment. Collect the writer's bounded
failures, findings, and durable-learning candidates for the root session; a
delegate never runs the terminal Learning Session. Then close every no-longer-needed agent and its descendants before
moving to the next phase. Never use `followup_task` to reactivate a completed
or finished assignment; a new review round gets a new reviewer instance.
Preserve the closed relationship as history instead of deleting it. A final
answer with an unneeded live agent is incomplete.

## Orchestrator mode

Same class of standing duties: stay available, keep the live status table
current after every dispatch and every finished agent, group related work into
the fewest PRs that still keep one problem per issue (`Closes #N` per completed
subtask), and keep working until every in-scope ticket is delivered. Do not
treat planning, a status table, or one PR as session complete.

After grouping, the done condition is: every in-scope ticket is merged or
explicitly out of scope. After each chunk merges, destroy that writer, start
the next from a fresh `ChaosEngine/*` branch off the fetched configured
default branch, update the live status table, and continue. Stop only when
the in-scope set is empty, the owner narrows scope, or a real blocker needs
the owner.

Same-subsystem leftovers share one PR. Split only when writers would collide on
the same files and cannot be sequenced, or when the work truly belongs to
different categories. If a delegate omitted an actionable leftover, file the
missing issue or Memory as an owner-command; do not implement the leftover work.

After every dispatch and after every subagent finishes, post or update a status
table the owner can read without reconstructing the session. Include completed,
in progress, planned, and out of scope rows so out of scope is not silently
dropped. Enhance with blocker, PR/issue links, HEAD SHA, and root-learning
handoff (`findings` / `nothing durable`). Do not put secrets in the
table.

| ID / work item | Mode stream | Status | Owner / agent | Dependency | Last update | Details / evidence | Next action |
| --- | --- | --- | --- | --- | --- | --- | --- |

Status values at least: `planned`, `in progress`, `blocked`, `review`,
`completed`, `out of scope`.

## Independent adversarial review

Review is optional and selected explicitly during planning; recommend it and
default it on. If approved, review complete pull-request implementation only
after final scope commit plus automated CI/comment fixes. Never review
individual implementation steps. Independence remains mandatory when enabled:

- The reviewer is a **separate agent instance, never the author** of the work.
- Choose a disposable review mechanism. When the host provides reliable terminal
  close, use a native reviewer subagent and close it after the verdict. Without
  reliable terminal close, use an artifact-bounded ephemeral reviewer and pass
  the exact revision or diff plus its directly required guidance as immutable
  input. It must not depend on repository shell access to discover the artifact.
  When review is enabled, such a review records a valid receipt only when active host
  adapter records its successful receipt, bound to that artifact. Until an
  adapter supports that receipt, obtain an independent pull-request review too;
  prose claiming the one-shot review happened is not evidence.
- The reviewer is prompted to **refute** the work — find where it is wrong,
  unverified, or over-claimed — not to approve it.
- The reviewer is handed **the exact revision under review** and a way to read
  it that does not touch the shared working tree: `gh pr diff <n>`,
  `git show <ref>:<path>`, or its own `git worktree add`. A read-only reviewer
  owns no worktree, so left unsaid it inherits whatever branch the shared tree
  happens to hold, which is routinely not the one under review.
- The reviewer's **first action confirms the revision is what it thinks it is**,
  before any finding is written. If it cannot confirm, it says so and stops. A
  search that misses because the tree is wrong returns a clean no-match, which
  is indistinguishable from a real absence and is the most confident wrong
  answer a review can produce.
- Depth scales with the pull request, matching consult triage. Use one reviewer
  with correctness, reproduction, and blast-radius lenses.
- Escalate to the most intelligent model for a new subsystem, a migration, a
  dependency swap, or any decision that is expensive to unwind.

A self-review is not a review. Neither is a delegate's own report on its own
work.

### Recording a finding

Each finding is one block, in this form. A finding that cannot fill `Scenario`
and `Evidence` is not confirmed and is not written.

```
F<n>      <one-line claim>
Verdict:  confirmed | refuted | unproven     (refuted is dropped, not softened)
Blocking: yes | no                           (yes = wrong behavior ships; docstrings,
                                              commit messages and PR prose are never yes)
Where:    <path>:<line>                      repository-relative
Scenario: <the concrete sequence that produces the wrong result>
Evidence: <what was run or read, and what it returned>
Class:    <recurrence class, or "new: <name>">
Fix:      patch | ticket | decision_needed | dismiss
```

`Class` is the load-bearing field, and it is why the block exists: when a class
reaches three entries it earns a mechanical check, or an explicit "cannot be
mechanised, and here is why". Extend the list below; never invent a synonym for
a row that already exists.

| Class | Shape | What catches it now |
| --- | --- | --- |
| `vacuous-check` | a test, pin or self-test that cannot fail | mutation checks and the `setUp`-patch pin in `test_agent_router_contract.py` |
| `unspecified-predicate` | the rule re-guesses a decision the ticket never made | nothing -- it needs a ruling on the issue |
| `credit-not-in-diff` | prose credits work the diff does not contain | the credit scan in `validate_pr_closing_keywords.py` |
| `sibling-left` | the instance was fixed and its twin was not | the docstring duplicate scan in `validate_agent_guidance.py` |
| `fires-on-correct-work` | the gate refuses work that satisfied it | nothing yet, and it is past three: R19 refusing an out-of-repo write, R13 refusing an already-merged branch (#4569), and the three in #4559 item 6 -- so it has earned that item's check, which runs every new rule against the branch adding it |
| `live-state-in-tests` | the test's answer depends on the machine | `StopTestsAreIndependentOfLiveStateTest` |
| `wrong-width-check` | the command measured is not the command that matters | nothing -- see #4559 item 3 |

### Review disposition

Run no more than two rounds. Batch-fix first-round confirmed findings at the
end, then use a second round only to assess those fixes. Never exceed two or
restart full review. A finding that changes architecture returns work to
planning. Extra local tests run only after this terminal review stage.

Post the findings as a pull-request review, not only in chat. Findings that live
only in a transcript die with the session, which is how the same class gets
re-found by a reviewer instead of accumulating into a check.

## Delegate covenant

[Roles](roles.md) states how each host carries this. Either way, every dispatch
carries it:

> Load the canonical ChaosEngine entrypoint before all other work. Evidence over
> inference: read or run before claiming. Stay inside assigned scope; report
> adjacent findings. Cite repository-relative `file:line` evidence. Behavior
> complete the bounded implementation before consolidated validation. Never
> claim an unrun check. Escalate architecture or ambiguity instead of deciding it.
> Report failures plainly. Before waiting or after a material finding, send a
> substantive handoff: done evidence, current step, blockers, and whether a
> decision is needed.

A default-model delegate may add: mechanical, spec-exact, or bulk slices may go
to the mechanical model with this covenant; inspect their actual output before
using it. The mechanical model omits that clause because it may not delegate.

## Returned-work review

Read diff and tests in the two passes [roles](roles.md) gives the reviewer.
Route every finding to `patch`, `ticket`, `decision_needed`, or `dismiss`.
Orchestrator owns final severity because it has full context. Use the
[verification-gap lens](verification-gap-lens.md) for behavior that could break
without a failing check.
