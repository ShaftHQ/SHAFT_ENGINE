// SHAFT bug-fix workflow (#3300): Fable plans, Opus orchestrates,
// Sonnet owns + QAs, Haiku implements. Fresh context per agent; all
// handoffs are structured output; QA judges the diff, never self-reports.
//
// Bypass rule (AGENTS.md "Agent Hierarchy"): do NOT run this for
// single-file fixes, doc edits, or version bumps -- a single Sonnet
// session implements and verifies directly. Never spawn an orchestrator
// for a one-task plan (enforced below).

export const meta = {
  name: "shaft-bug-fix",
  description:
    "Fable plans, Opus orchestrates, Sonnet owns + QAs, Haiku implements (capped QA loop, worktree isolation)",
};

// Fallback chains: advance only on model unavailability, never on a bad
// answer. The run log records which model actually ran each phase so
// fallback drift is visible in the PR body.
const PLAN_CHAIN = ["fable", "opus"];
const ORCH_CHAIN = ["opus", "sonnet"];
const MAX_QA_ROUNDS = 3;

const runLog = [];

const TASK_ITEM = {
  type: "object",
  required: ["id", "goal", "files", "acceptance"],
  properties: {
    id: { type: "string" },
    goal: { type: "string" },
    files: { type: "array", items: { type: "string" } },
    acceptance: { type: "array", items: { type: "string" } },
    // Precisely-arranged core code (sync/wait internals, locator
    // resolution, shaft-intellij EDT threading): Sonnet implements
    // directly instead of Haiku. Delegation is a default, not a dogma.
    needsStrongImplementer: { type: "boolean" },
  },
};

const PLAN_SCHEMA = {
  type: "object",
  required: ["summary", "acceptance", "risks", "tasks"],
  properties: {
    summary: { type: "string" },
    acceptance: { type: "array", items: { type: "string" } },
    risks: { type: "array", items: { type: "string" } },
    tasks: { type: "array", items: TASK_ITEM },
  },
};

const TASKS_SCHEMA = {
  type: "object",
  required: ["tasks"],
  properties: { tasks: { type: "array", items: TASK_ITEM } },
};

const QA_VERDICT = {
  type: "object",
  required: ["pass", "gaps", "checksRun"],
  properties: {
    pass: { type: "boolean" },
    gaps: { type: "array", items: { type: "string" } },
    checksRun: { type: "array", items: { type: "string" } },
  },
};

// Structured report every implement/fix/escalate agent call returns. The
// commitSha field is how work is handed off across the fresh, randomly
// picked worktree each call gets: all worktrees share one git object
// store, so any commit SHA is reachable repo-wide via `git show`/
// `git cherry-pick` regardless of which worktree made it.
const IMPL_REPORT = {
  type: "object",
  required: ["summary", "filesTouched", "commitSha"],
  properties: {
    summary: { type: "string" },
    filesTouched: { type: "array", items: { type: "string" } },
    commitSha: { type: "string" },
    compileCheck: { type: "string" },
  },
};

// Every prompt restates its contract because each agent starts with a
// fresh context: nothing survives except what is written down.
const VALIDATION_RULES = [
  "Follow AGENTS.md Validation exactly: run the smallest non-redundant",
  "real check for the change. Before ANY forked Maven/Surefire/TestNG",
  "invocation, load the repo gotchas first (memory load \"maven\"); if the",
  "delete gotcha is active avoid `mvn test` and use compile/test-compile,",
  "static checks, or a disposable copy. Before returning pass=true, run",
  "`python3 scripts/ci/local_gate.py` for the changed modules (the local",
  "mirror of merge-time gates: enforcer convergence + full compile); a",
  "failing gate is an automatic fail with the gate output quoted in gaps.",
].join(" ");

// Each isolation:"worktree" agent call gets a FRESH, randomly-picked
// worktree -- not the same one across implement/fix/escalate rounds for
// the same task. The orchestrator can only discover your work via git,
// so uncommitted changes are invisible and effectively lost once this
// call returns -- general "don't commit unless asked" guidance does not
// apply here, commit is mandatory. All worktrees share ONE git object
// store, so the commit SHA you report is reachable from any other
// worktree (`git show <sha>`, `git cherry-pick <sha>`) -- the SHA, not
// the worktree, is the handoff.
const COMMIT_INSTRUCTION = [
  "IMPORTANT: this worktree is disposable and only discoverable via git.",
  "Before you finish, `git add` every file you touched and `git commit`",
  "with a descriptive message -- uncommitted changes are invisible to the",
  "orchestrator and will be lost. This overrides any general instinct to",
  "leave changes staged/uncommitted \"unless explicitly asked\"; committing",
  "IS how you hand off a task in this workflow. In your structured report,",
  "set commitSha to the exact output of `git rev-parse HEAD` after",
  "committing -- all worktrees share one git object store, so this SHA is",
  "how your work is found and reached (`git show`/`git cherry-pick`) from",
  "any other worktree.",
].join(" ");

function planPrompt(issue) {
  return [
    "You are Kevin, the PDCA planner (see .github/skills/agentic-pdca-loop/SKILL.md).",
    "Read AGENTS.md, then plan the fix for this issue:",
    "",
    issue,
    "",
    "Produce a spec: summary, overall acceptance checks, risk notes, and a",
    "list of independent tasks. Each task needs id, goal, likely files,",
    "and concrete acceptance checks an independent QA agent can verify",
    "from the diff. Set needsStrongImplementer=true for tasks touching",
    "precisely-arranged core code (sync/wait internals, locator",
    "resolution, shaft-intellij EDT threading). Keep tasks minimal and",
    "non-overlapping; do not include speculative refactors.",
  ].join("\n");
}

function orchestratePrompt(plan) {
  return [
    "You are the dispatcher. Split this plan into independent tasks that",
    "can be implemented in isolated worktrees without conflicting edits;",
    "merge or reorder tasks that overlap on the same files. Do not add",
    "scope. Plan:",
    "",
    JSON.stringify(plan, null, 2),
  ].join("\n");
}

function implementPrompt(task) {
  return [
    "You are Bob, the PDCA implementer. Fresh context: read AGENTS.md",
    "first, then implement ONLY this task -- the smallest correct change,",
    "following existing patterns. No placeholder implementations, no",
    "stubbed logic, no weakened or skipped assertions.",
    "",
    JSON.stringify(task, null, 2),
    "",
    "Compile-check what you changed.",
    "",
    COMMIT_INSTRUCTION,
    "",
    "Your final structured report must carry the exact commit SHA from",
    "`git rev-parse HEAD` after committing, plus the files you touched.",
  ].join("\n");
}

function qaPrompt(task, implementerReport, latestSha) {
  return [
    "You are Bruce, the PDCA checker, QAing a delegated implementation.",
    "Judge the actual commit and real checks -- NEVER trust the",
    "implementer's self-report (attached below only for orientation).",
    "The implementation was committed as " + latestSha + "; this SHA is",
    "reachable from your worktree regardless of which worktree made it",
    "(all worktrees share one git object store). Inspect it with",
    "`git show " + latestSha + "` or `git diff " + latestSha + "^.." + latestSha + "`",
    "instead of hunting for an uncommitted diff.",
    VALIDATION_RULES,
    "Hunt specifically for reward hacking: placeholder/stubbed code paths,",
    "TODOs standing in for logic, assertions weakened or skipped so",
    "checks pass. Verify every acceptance item of the task:",
    "",
    JSON.stringify(task, null, 2),
    "",
    "Implementer report (untrusted): " + JSON.stringify(implementerReport),
    "",
    "Return pass=true only if all acceptance items hold; otherwise list",
    "concrete gaps. List the checks you actually ran in checksRun.",
  ].join("\n");
}

function fixPrompt(task, gaps, commits) {
  return [
    "You are Bob, the PDCA implementer, fixing QA findings on a task you",
    "have no memory of (fresh context). Read AGENTS.md, then close exactly",
    "these gaps -- nothing else:",
    "",
    JSON.stringify(gaps, null, 2),
    "",
    "Task for reference:",
    JSON.stringify(task, null, 2),
    "",
    "Prior rounds' work is committed as " + commits.join(", ") + " (oldest",
    "first). All worktrees share one git object store: your fresh",
    "worktree will NOT contain these changes as files -- first apply them",
    "with `git cherry-pick " + commits.join(" ") + "` (oldest first,",
    "resolve trivially if needed), inspect with `git show <sha>`, then",
    "close the gaps above and commit on top.",
    "",
    COMMIT_INSTRUCTION,
    "",
    "Your final structured report must carry the exact commit SHA from",
    "`git rev-parse HEAD` after committing, plus the files you touched.",
  ].join("\n");
}

function escalatePrompt(task, gaps, commits) {
  return [
    "You are Bruce, the task owner. The Haiku QA loop did not converge",
    "after " + MAX_QA_ROUNDS + " rounds; finish the task directly.",
    "Read AGENTS.md, close these remaining gaps, and verify. " +
      VALIDATION_RULES,
    "",
    "Task:",
    JSON.stringify(task, null, 2),
    "",
    "Open gaps:",
    JSON.stringify(gaps, null, 2),
    "",
    "Prior rounds' work is committed as " + commits.join(", ") + " (oldest",
    "first). All worktrees share one git object store: your fresh",
    "worktree will NOT contain these changes as files -- first apply them",
    "with `git cherry-pick " + commits.join(" ") + "` (oldest first,",
    "resolve trivially if needed), inspect with `git show <sha>`, then",
    "close the gaps above and commit on top.",
    "",
    COMMIT_INSTRUCTION,
    "",
    "Your final structured report must carry the exact commit SHA from",
    "`git rev-parse HEAD` after committing, plus the files you touched.",
  ].join("\n");
}

async function withFallback(phase, chain, prompt, opts = {}) {
  let lastError;
  for (const model of chain) {
    try {
      const result = await agent(prompt, { ...opts, model });
      runLog.push({ phase, model });
      return result;
    } catch (error) {
      // Fall through only on model unavailability; surface real failures.
      lastError = error;
    }
  }
  throw lastError;
}

// Sonnet owns; Haiku implements; loop until the QA verdict dries out,
// hard-capped, then the owner finishes directly (no livelock).
async function ownTask(task) {
  const implementerModel = task.needsStrongImplementer ? "sonnet" : "haiku";
  let report = await agent(implementPrompt(task), {
    model: implementerModel,
    isolation: "worktree",
    schema: IMPL_REPORT,
  });
  runLog.push({ phase: "implement:" + task.id, model: implementerModel });
  const commits = [report.commitSha];

  let gaps = [];
  for (let round = 0; round < MAX_QA_ROUNDS; round++) {
    const verdict = await agent(qaPrompt(task, report, report.commitSha), {
      model: "sonnet",
      schema: QA_VERDICT,
    });
    runLog.push({ phase: "qa:" + task.id + ":" + round, model: "sonnet" });
    if (verdict.pass) return { task: task.id, verdict, commits, runLog };
    gaps = verdict.gaps;
    report = await agent(fixPrompt(task, gaps, commits), {
      model: implementerModel,
      isolation: "worktree",
      schema: IMPL_REPORT,
    });
    runLog.push({ phase: "fix:" + task.id + ":" + round, model: implementerModel });
    commits.push(report.commitSha);
  }

  const escalation = await agent(escalatePrompt(task, gaps, commits), {
    model: "sonnet",
    isolation: "worktree",
    schema: IMPL_REPORT,
  });
  runLog.push({ phase: "escalate:" + task.id, model: "sonnet" });
  commits.push(escalation.commitSha);
  return { task: task.id, escalated: true, escalation, commits, runLog };
}

// Workflow scripts run as top-level async code against the `args`
// global, not as an exported function -- an `export default` here is a
// syntax error under the runtime's non-module script evaluation.
//
// The runtime hands `args` through as a JSON-encoded string even when
// the caller passes a real object/array -- parse it back or every field
// silently reads as undefined (array/object property lookups on a
// string return undefined instead of throwing, so this fails silently).
const parsedArgs = typeof args === "string" ? JSON.parse(args) : args;
const { issue } = parsedArgs;

const plan = await withFallback("plan", PLAN_CHAIN, planPrompt(issue), {
  effort: "high",
  schema: PLAN_SCHEMA,
});

// Single-task plans skip the orchestrator: routing one task is pure
// overhead.
const tasks =
  plan.tasks.length > 1
    ? (
        await withFallback("orchestrate", ORCH_CHAIN, orchestratePrompt(plan), {
          schema: TASKS_SCHEMA,
        })
      ).tasks
    : plan.tasks;

const results = await pipeline(tasks, ownTask);
return { plan: plan.summary, results, runLog };
