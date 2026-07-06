// SHAFT release/CI incident workflow (#3300): parallel read-only Haiku
// triage of failing jobs -> Fable/Opus root-cause synthesis -> Sonnet
// owns each fix, delegating implementation to Haiku with a capped QA
// loop. Encodes the release/CI-fix shape this repo runs manually, with
// the ci-failure-investigator bridge rules loaded into triage prompts.
//
// Bypass rule (AGENTS.md "Agent Hierarchy"): a single failing job with
// an obvious cause is a single-task fix -- use one Sonnet session, not
// this workflow. Watching a rerun afterwards is recurring observation:
// that belongs to /loop or Monitor, deliberately NOT encoded here so
// the workflow terminates.

export const meta = {
  name: "shaft-release-ci-fix",
  description:
    "Parallel Haiku triage of failing CI jobs, Fable/Opus root-cause synthesis, Sonnet-owned fixes with capped Haiku QA loops",
};

const SYNTH_CHAIN = ["fable", "opus"];
const MAX_QA_ROUNDS = 3;

const runLog = [];

const TRIAGE_VERDICT = {
  type: "object",
  required: ["job", "failureKind", "evidence", "suspectedCause"],
  properties: {
    job: { type: "string" },
    failureKind: {
      type: "string",
      enum: ["code", "test", "flaky", "infra", "dependency", "config", "unknown"],
    },
    evidence: { type: "array", items: { type: "string" } },
    suspectedCause: { type: "string" },
    firstBadCommit: { type: "string" },
  },
};

const FIX_PLAN_SCHEMA = {
  type: "object",
  required: ["rootCauses", "fixes"],
  properties: {
    rootCauses: { type: "array", items: { type: "string" } },
    fixes: {
      type: "array",
      items: {
        type: "object",
        required: ["id", "goal", "files", "acceptance"],
        properties: {
          id: { type: "string" },
          goal: { type: "string" },
          files: { type: "array", items: { type: "string" } },
          acceptance: { type: "array", items: { type: "string" } },
          needsStrongImplementer: { type: "boolean" },
        },
      },
    },
  },
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

const VALIDATION_RULES = [
  "Follow AGENTS.md Validation exactly: run the smallest non-redundant",
  "real check for the change. Before ANY forked Maven/Surefire/TestNG",
  "invocation, load the repo gotchas first (memory load \"maven\"); if the",
  "delete gotcha is active avoid `mvn test` and use compile/test-compile,",
  "static checks, or a disposable copy.",
].join(" ");

function triagePrompt(job) {
  return [
    "You are a READ-ONLY CI triage agent. Do not edit files or rerun",
    "jobs. Load the ci-failure-investigator bridge rules",
    "(.agents/skills/ci-failure-investigator/SKILL.md) first, then",
    "inspect the logs of this failing job with gh:",
    "",
    JSON.stringify(job),
    "",
    "Classify the failure, quote the decisive log lines as evidence, and",
    "name the most likely cause (and first bad commit if determinable).",
  ].join("\n");
}

function synthesisPrompt(verdicts) {
  return [
    "You are Kevin, the PDCA planner, synthesizing root causes for a",
    "release/CI incident. Read AGENTS.md, then correlate these per-job",
    "triage verdicts (untrusted; spot-check surprising claims):",
    "",
    JSON.stringify(verdicts, null, 2),
    "",
    "Group symptoms sharing one root cause into a single fix. Emit",
    "independent fixes with id, goal, files, and acceptance checks a QA",
    "agent can verify from the diff. Mark needsStrongImplementer=true for",
    "precisely-arranged core code. Flaky-only failures route to the",
    "flaky-test-stabilizer bridge instead of a code fix.",
  ].join("\n");
}

function implementPrompt(fix) {
  return [
    "You are Bob, the PDCA implementer. Fresh context: read AGENTS.md",
    "first, then implement ONLY this CI fix -- smallest correct change,",
    "no placeholders, no weakened assertions, no disabled tests without",
    "an explicit instruction in the fix goal.",
    "",
    JSON.stringify(fix, null, 2),
  ].join("\n");
}

function qaPrompt(fix, implementerReport) {
  return [
    "You are Bruce, the PDCA checker. Judge the actual git diff and real",
    "checks -- never the implementer's self-report (attached only for",
    "orientation). " + VALIDATION_RULES,
    "Hunt for reward hacking: stubs, TODOs standing in for logic,",
    "assertions weakened or tests skipped to go green. Verify every",
    "acceptance item:",
    "",
    JSON.stringify(fix, null, 2),
    "",
    "Implementer report (untrusted): " + JSON.stringify(implementerReport),
    "",
    "Return pass=true only if all acceptance items hold; otherwise list",
    "concrete gaps. List the checks you actually ran in checksRun.",
  ].join("\n");
}

function fixGapsPrompt(fix, gaps) {
  return [
    "You are Bob, the PDCA implementer (fresh context). Read AGENTS.md,",
    "inspect the current diff, then close exactly these QA gaps on the",
    "fix below -- nothing else:",
    "",
    JSON.stringify(gaps, null, 2),
    "",
    "Fix for reference:",
    JSON.stringify(fix, null, 2),
  ].join("\n");
}

function escalatePrompt(fix, gaps) {
  return [
    "You are Bruce, the task owner. The QA loop did not converge after",
    MAX_QA_ROUNDS + " rounds; finish this CI fix directly. Read AGENTS.md,",
    "review the diff, close the remaining gaps, verify. " + VALIDATION_RULES,
    "",
    "Fix:",
    JSON.stringify(fix, null, 2),
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
      lastError = error; // fall through only on model unavailability
    }
  }
  throw lastError;
}

async function ownFix(fix) {
  const implementerModel = fix.needsStrongImplementer ? "sonnet" : "haiku";
  let report = await agent(implementPrompt(fix), {
    model: implementerModel,
    isolation: "worktree",
  });
  runLog.push({ phase: "implement:" + fix.id, model: implementerModel });

  let gaps = [];
  for (let round = 0; round < MAX_QA_ROUNDS; round++) {
    const verdict = await agent(qaPrompt(fix, report), {
      model: "sonnet",
      schema: QA_VERDICT,
    });
    runLog.push({ phase: "qa:" + fix.id + ":" + round, model: "sonnet" });
    if (verdict.pass) return { fix: fix.id, verdict, runLog };
    gaps = verdict.gaps;
    report = await agent(fixGapsPrompt(fix, gaps), {
      model: implementerModel,
      isolation: "worktree",
    });
    runLog.push({ phase: "fix:" + fix.id + ":" + round, model: implementerModel });
  }

  const escalation = await agent(escalatePrompt(fix, gaps), {
    model: "sonnet",
    isolation: "worktree",
  });
  runLog.push({ phase: "escalate:" + fix.id, model: "sonnet" });
  return { fix: fix.id, escalated: true, escalation, runLog };
}

// Workflow scripts run as top-level async code against the `args`
// global, not as an exported function -- an `export default` here is a
// syntax error under the runtime's non-module script evaluation.
//
// args.failingJobs: [{ name, runUrl }] -- e.g. from
// `gh run view <id> --json jobs`.
const { failingJobs } = args;

const verdicts = await parallel(
  failingJobs.map((job) => () =>
    agent(triagePrompt(job), { model: "haiku", schema: TRIAGE_VERDICT })
  )
);
runLog.push({ phase: "triage", model: "haiku", jobs: failingJobs.length });

const plan = await withFallback("synthesize", SYNTH_CHAIN, synthesisPrompt(verdicts), {
  effort: "high",
  schema: FIX_PLAN_SCHEMA,
});

const results = await pipeline(plan.fixes, ownFix);

// Terminate here. Rerun-watching is /loop / Monitor territory.
return { rootCauses: plan.rootCauses, results, runLog };
