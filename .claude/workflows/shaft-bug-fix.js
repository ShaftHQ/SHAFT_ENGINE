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

// Every prompt restates its contract because each agent starts with a
// fresh context: nothing survives except what is written down.
const VALIDATION_RULES = [
  "Follow AGENTS.md Validation exactly: run the smallest non-redundant",
  "real check for the change. Before ANY forked Maven/Surefire/TestNG",
  "invocation, load the repo gotchas first (memory load \"maven\"); if the",
  "delete gotcha is active avoid `mvn test` and use compile/test-compile,",
  "static checks, or a disposable copy.",
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
    "Compile-check what you changed. Report the files you touched.",
  ].join("\n");
}

function qaPrompt(task, implementerReport) {
  return [
    "You are Bruce, the PDCA checker, QAing a delegated implementation.",
    "Judge the actual git diff and real checks -- NEVER trust the",
    "implementer's self-report (attached below only for orientation).",
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

function fixPrompt(task, gaps) {
  return [
    "You are Bob, the PDCA implementer, fixing QA findings on a task you",
    "have no memory of (fresh context). Read AGENTS.md, inspect the",
    "current diff, then close exactly these gaps -- nothing else:",
    "",
    JSON.stringify(gaps, null, 2),
    "",
    "Task for reference:",
    JSON.stringify(task, null, 2),
  ].join("\n");
}

function escalatePrompt(task, gaps) {
  return [
    "You are Bruce, the task owner. The Haiku QA loop did not converge",
    "after " + MAX_QA_ROUNDS + " rounds; finish the task directly.",
    "Read AGENTS.md, review the current diff, close these remaining gaps,",
    "and verify. " + VALIDATION_RULES,
    "",
    "Task:",
    JSON.stringify(task, null, 2),
    "",
    "Open gaps:",
    JSON.stringify(gaps, null, 2),
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
  });
  runLog.push({ phase: "implement:" + task.id, model: implementerModel });

  let gaps = [];
  for (let round = 0; round < MAX_QA_ROUNDS; round++) {
    const verdict = await agent(qaPrompt(task, report), {
      model: "sonnet",
      schema: QA_VERDICT,
    });
    runLog.push({ phase: "qa:" + task.id + ":" + round, model: "sonnet" });
    if (verdict.pass) return { task: task.id, verdict, runLog };
    gaps = verdict.gaps;
    report = await agent(fixPrompt(task, gaps), {
      model: implementerModel,
      isolation: "worktree",
    });
    runLog.push({ phase: "fix:" + task.id + ":" + round, model: implementerModel });
  }

  const escalation = await agent(escalatePrompt(task, gaps), {
    model: "sonnet",
    isolation: "worktree",
  });
  runLog.push({ phase: "escalate:" + task.id, model: "sonnet" });
  return { task: task.id, escalated: true, escalation, runLog };
}

export default async function run({ issue }) {
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
}
