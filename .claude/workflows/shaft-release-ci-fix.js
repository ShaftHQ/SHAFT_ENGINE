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
// the same fix. The orchestrator can only discover your work via git, so
// uncommitted changes are invisible and effectively lost once this call
// returns -- general "don't commit unless asked" guidance does not apply
// here, commit is mandatory. All worktrees share ONE git object store,
// so the commit SHA you report is reachable from any other worktree
// (`git show <sha>`, `git cherry-pick <sha>`) -- the SHA, not the
// worktree, is the handoff.
const COMMIT_INSTRUCTION = [
  "IMPORTANT: this worktree is disposable and only discoverable via git.",
  "Before you finish, `git add` every file you touched and `git commit`",
  "with a descriptive message -- uncommitted changes are invisible to the",
  "orchestrator and will be lost. This overrides any general instinct to",
  "leave changes staged/uncommitted \"unless explicitly asked\"; committing",
  "IS how you hand off a fix in this workflow. In your structured report,",
  "set commitSha to the exact output of `git rev-parse HEAD` after",
  "committing -- all worktrees share one git object store, so this SHA is",
  "how your work is found and reached (`git show`/`git cherry-pick`) from",
  "any other worktree.",
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
    "",
    COMMIT_INSTRUCTION,
    "",
    "Your final structured report must carry the exact commit SHA from",
    "`git rev-parse HEAD` after committing, plus the files you touched.",
  ].join("\n");
}

function qaPrompt(fix, implementerReport, latestSha) {
  return [
    "You are Bruce, the PDCA checker. Judge the actual commit and real",
    "checks -- never the implementer's self-report (attached only for",
    "orientation). The fix was committed as " + latestSha + "; this SHA is",
    "reachable from your worktree regardless of which worktree made it",
    "(all worktrees share one git object store). Inspect it with",
    "`git show " + latestSha + "` or `git diff " + latestSha + "^.." + latestSha + "`",
    "instead of hunting for an uncommitted diff. " + VALIDATION_RULES,
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

function fixGapsPrompt(fix, gaps, commits) {
  return [
    "You are Bob, the PDCA implementer (fresh context). Read AGENTS.md,",
    "then close exactly these QA gaps on the fix below -- nothing else:",
    "",
    JSON.stringify(gaps, null, 2),
    "",
    "Fix for reference:",
    JSON.stringify(fix, null, 2),
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

function escalatePrompt(fix, gaps, commits) {
  return [
    "You are Bruce, the task owner. The QA loop did not converge after",
    MAX_QA_ROUNDS + " rounds; finish this CI fix directly. Read AGENTS.md,",
    "close the remaining gaps, verify. " + VALIDATION_RULES,
    "",
    "Fix:",
    JSON.stringify(fix, null, 2),
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
    schema: IMPL_REPORT,
  });
  runLog.push({ phase: "implement:" + fix.id, model: implementerModel });
  const commits = [report.commitSha];

  let gaps = [];
  for (let round = 0; round < MAX_QA_ROUNDS; round++) {
    const verdict = await agent(qaPrompt(fix, report, report.commitSha), {
      model: "sonnet",
      schema: QA_VERDICT,
    });
    runLog.push({ phase: "qa:" + fix.id + ":" + round, model: "sonnet" });
    if (verdict.pass) return { fix: fix.id, verdict, commits, runLog };
    gaps = verdict.gaps;
    report = await agent(fixGapsPrompt(fix, gaps, commits), {
      model: implementerModel,
      isolation: "worktree",
      schema: IMPL_REPORT,
    });
    runLog.push({ phase: "fix:" + fix.id + ":" + round, model: implementerModel });
    commits.push(report.commitSha);
  }

  const escalation = await agent(escalatePrompt(fix, gaps, commits), {
    model: "sonnet",
    isolation: "worktree",
    schema: IMPL_REPORT,
  });
  runLog.push({ phase: "escalate:" + fix.id, model: "sonnet" });
  commits.push(escalation.commitSha);
  return { fix: fix.id, escalated: true, escalation, commits, runLog };
}

// Workflow scripts run as top-level async code against the `args`
// global, not as an exported function -- an `export default` here is a
// syntax error under the runtime's non-module script evaluation.
//
// The runtime hands `args` through as a JSON-encoded string even when
// the caller passes a real object/array -- parse it back or every field
// silently reads as undefined (array/object property lookups on a
// string return undefined instead of throwing, so this fails silently).
//
// args.failingJobs: [{ name, runUrl }] -- e.g. from
// `gh run view <id> --json jobs`.
const parsedArgs = typeof args === "string" ? JSON.parse(args) : args;
const { failingJobs } = parsedArgs;

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
