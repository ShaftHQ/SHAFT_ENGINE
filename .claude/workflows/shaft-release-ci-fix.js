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

// Risk tier drives both who implements and how many QA rounds are
// allowed before escalation. "high" additionally turns on the adversarial
// Haiku refuter after a passing QA verdict. Absent riskTier means "med".
const RISK_TIERS = {
  low: { implementer: "haiku", qaRounds: 1, refuter: false },
  med: { implementer: "haiku", qaRounds: 2, refuter: false },
  high: { implementer: "sonnet", qaRounds: 3, refuter: true },
};

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
          // Precisely-arranged core code (sync/wait internals, locator
          // resolution, shaft-intellij EDT threading), public API surface,
          // or release/build plumbing: riskTier="high" routes to Sonnet
          // plus the adversarial refuter instead of the default Haiku
          // path. Absent riskTier means "med".
          riskTier: { type: "string", enum: ["low", "med", "high"] },
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

// Adversarial second opinion run only on high-tier fixes after a passing
// QA verdict. Defaults toward suspicion: refuted=false requires a
// deliberate finding of no concrete gap, not mere absence of complaint.
const REFUTER_VERDICT = {
  type: "object",
  required: ["refuted", "reasons"],
  properties: {
    refuted: { type: "boolean" },
    reasons: { type: "array", items: { type: "string" } },
  },
};

// Structured report every implement/fix/escalate agent call returns. The
// commitSha field is how work is handed off across the fresh, randomly
// picked worktree each call gets: all worktrees share one git object
// store, so any commit SHA is reachable repo-wide via `git show`/
// `git cherry-pick` regardless of which worktree made it. restatedGoal
// and plannedFiles are the task-contract echo: the implementer must
// commit to a goal/file-scope reading BEFORE writing code, so a
// materially wrong reading surfaces as a bounce (commitSha:"BOUNCED")
// instead of silently drifting into out-of-scope work.
const IMPL_REPORT = {
  type: "object",
  required: [
    "summary",
    "filesTouched",
    "commitSha",
    "restatedGoal",
    "plannedFiles",
  ],
  properties: {
    summary: { type: "string" },
    filesTouched: { type: "array", items: { type: "string" } },
    commitSha: { type: "string" },
    compileCheck: { type: "string" },
    restatedGoal: { type: "string" },
    plannedFiles: { type: "array", items: { type: "string" } },
  },
};

// Only strings that look like a real git SHA are ever pushed into a
// commits[] handoff list; "BOUNCED" (or anything else non-SHA-shaped)
// must never reach a cherry-pick instruction in a later prompt.
const SHA_RE = /^[0-9a-f]{7,40}$/i;

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
    "agent can verify from the diff. Assign riskTier per fix: \"high\" is",
    "REQUIRED for precisely-arranged core code (sync/wait internals,",
    "locator resolution -- shaft-engine element/browser sync + locator",
    "building --, shaft-intellij EDT/Swing threading); \"high\" also for",
    "changes to public API surface or release/build plumbing. \"low\" only",
    "for mechanical single-file changes with a trivially checkable",
    "acceptance. Default to \"med\" otherwise. Flaky-only failures route to",
    "the flaky-test-stabilizer bridge instead of a code fix.",
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
    "BEFORE writing any code: restate the task goal in one line",
    "(restatedGoal) and list the files you plan to touch (plannedFiles).",
    "If during work you find the fix is materially different from its",
    "stated goal, or requires touching files far outside fix.files, STOP",
    "without committing: set commitSha to the literal string \"BOUNCED\"",
    "and explain why in summary -- do not force a fix into a task that",
    "does not match reality.",
    "",
    COMMIT_INSTRUCTION,
    "",
    "Your final structured report must carry the exact commit SHA from",
    "`git rev-parse HEAD` after committing, plus the files you touched,",
    "unless you bounced (see above).",
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

function escalatePrompt(fix, gaps, commits, qaRounds) {
  return [
    "You are Bruce, the task owner. The QA loop did not converge after",
    qaRounds + " round(s) (this fix's riskTier caps it at " + qaRounds +
      "); finish this CI fix directly. Read AGENTS.md,",
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

function refutePrompt(fix, latestSha) {
  return [
    "You are an adversarial reviewer. A QA verdict just passed this fix,",
    "but this is a high-risk fix -- precisely-arranged core code, public",
    "API surface, or release/build plumbing -- so it gets a second,",
    "skeptical pass before it is trusted. Your job is to try to REFUTE the",
    "pass verdict, not to rubber-stamp it.",
    "",
    "Inspect the actual commit with `git show " + latestSha + "` (this SHA",
    "is reachable from your worktree regardless of which worktree made",
    "it -- all worktrees share one git object store). Hunt for stubs,",
    "TODO-standing-in-for-logic, weakened or skipped assertions, and any",
    "acceptance item of the fix below that is not actually met:",
    "",
    JSON.stringify(fix, null, 2),
    "",
    "Set refuted=true only for CONCRETE gaps: a specific file+line, or a",
    "specific acceptance item with no evidence it was met. Style nits,",
    "taste preferences, or \"could be better\" observations do NOT refute",
    "the verdict. Default to refuted=false only after you have actively",
    "looked and found nothing concrete -- do not default to false merely",
    "because the QA verdict already said pass.",
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

// Only ever hand a real SHA to a later cherry-pick instruction; a
// bounced "BOUNCED" sentinel must never be pushed onto the commits chain.
function pushIfRealSha(commits, sha) {
  if (SHA_RE.test(sha)) commits.push(sha);
}

async function ownFix(fix) {
  const tier = RISK_TIERS[fix.riskTier] || RISK_TIERS.med;
  const implementerModel = tier.implementer;
  let report = await agent(implementPrompt(fix), {
    model: implementerModel,
    isolation: "worktree",
    schema: IMPL_REPORT,
  });
  runLog.push({ phase: "implement:" + fix.id, model: implementerModel });

  if (report.commitSha === "BOUNCED") {
    runLog.push({ phase: "bounce:" + fix.id, model: implementerModel });
    return { fix: fix.id, bounced: true, report, runLog };
  }

  const commits = [];
  pushIfRealSha(commits, report.commitSha);

  let gaps = [];
  for (let round = 0; round < tier.qaRounds; round++) {
    let verdict = await agent(qaPrompt(fix, report, report.commitSha), {
      model: "sonnet",
      schema: QA_VERDICT,
    });
    runLog.push({ phase: "qa:" + fix.id + ":" + round, model: "sonnet" });

    if (verdict.pass && tier.refuter) {
      const refutation = await agent(refutePrompt(fix, report.commitSha), {
        model: "haiku",
        schema: REFUTER_VERDICT,
      });
      runLog.push({ phase: "refute:" + fix.id + ":" + round, model: "haiku" });
      if (refutation.refuted) {
        verdict = { pass: false, gaps: refutation.reasons, checksRun: verdict.checksRun };
      }
    }

    if (verdict.pass) return { fix: fix.id, verdict, commits, runLog };
    gaps = verdict.gaps;
    report = await agent(fixGapsPrompt(fix, gaps, commits), {
      model: implementerModel,
      isolation: "worktree",
      schema: IMPL_REPORT,
    });
    runLog.push({ phase: "fix:" + fix.id + ":" + round, model: implementerModel });
    pushIfRealSha(commits, report.commitSha);
  }

  const escalation = await agent(
    escalatePrompt(fix, gaps, commits, tier.qaRounds),
    { model: "sonnet", isolation: "worktree", schema: IMPL_REPORT }
  );
  runLog.push({ phase: "escalate:" + fix.id, model: "sonnet" });
  pushIfRealSha(commits, escalation.commitSha);
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

// Epilogue: persist the run log to the PR body and save a durable retro
// to .memory/ so the run survives after this session ends. Plain-code
// formatting first (no agent needed to build a markdown table); a single
// Haiku agent handles the two side effects (PR update via gh, memory
// retro via the project CLI) because both require live shell commands
// this script cannot run directly.
const runLogTable = [
  "## Workflow run log",
  "",
  "| Phase | Model |",
  "|---|---|",
  ...runLog.map((row) => "| " + row.phase + " | " + row.model + " |"),
].join("\n");

// filter null results -- skipped/dead agents never produced a result.
const fixOutcomes = results
  .filter((result) => result != null)
  .map((result) => {
    const id = result.fix;
    if (result.bounced) return "- " + id + ": bounced (no commit)";
    if (result.escalated) {
      return (
        "- " + id + ": escalated -- commits: " + result.commits.join(", ")
      );
    }
    return "- " + id + ": pass -- commits: " + result.commits.join(", ");
  });

const epilogueMarkdown = [runLogTable, "", "### Fix outcomes", ...fixOutcomes].join(
  "\n"
);

runLog.push({ phase: "epilogue", model: "haiku" });

const EPILOGUE_REPORT = {
  type: "object",
  required: ["prUpdated", "memorySaved"],
  properties: {
    prUpdated: { type: "boolean" },
    memorySaved: { type: "boolean" },
    notes: { type: "string" },
  },
};

function epiloguePrompt(markdown) {
  return [
    "You are closing out a shaft-release-ci-fix workflow run. Do exactly",
    "two things.",
    "",
    "1. PR body: run `gh pr view --json number,body` for the CURRENT",
    "branch. If a PR exists, take its body and either append the markdown",
    "below as a new \"## Workflow run log\" section, or -- if a",
    "\"## Workflow run log\" section already exists -- REPLACE that",
    "existing section in place (so reruns stay idempotent and never",
    "duplicate the section). Write the updated full body to a temp file",
    "in the scratchpad dir (NEVER the repo working tree), then run",
    "`gh pr edit <number> --body-file <tempfile>`. Set prUpdated=true only",
    "if the edit succeeded. If no PR exists for the current branch, set",
    "prUpdated=false and put the markdown verbatim into notes so the",
    "outer session can use it instead.",
    "",
    "2. Memory retro: read",
    "`.memory/memory/gotchas/memory-saves-use-intent-first-json-on-stdin.md`",
    "first and follow it exactly -- routine writes use",
    "`memory remember --stdin` with intent-first JSON on stdin, never a",
    "stale `memory save` variant. First run `memory search` for this run's",
    "topic; if a prior retro memory for this recurring workflow topic",
    "exists, reuse its id instead of creating a duplicate. Save a durable",
    "retro capturing: the issue/job ids involved, each fix's outcome",
    "(rounds used, refuted?, bounced?, escalated?), which model ran each",
    "phase, and one lesson learned if any fix needed more than one QA",
    "round. No diary entries, no duplicates. Set memorySaved=true only if",
    "the write actually succeeded.",
    "",
    "Workflow run log markdown to use for the PR section:",
    "",
    markdown,
  ].join("\n");
}

let epilogueReport;
try {
  epilogueReport = await agent(epiloguePrompt(epilogueMarkdown), {
    model: "haiku",
    schema: EPILOGUE_REPORT,
  });
} catch (error) {
  // An epilogue failure must never fail the workflow: hand the built
  // markdown back so nothing is lost.
  return {
    rootCauses: plan.rootCauses,
    results,
    runLog,
    epilogue: { prUpdated: false, memorySaved: false, notes: String(error) },
    runLogMarkdown: epilogueMarkdown,
  };
}

const finalReturn = {
  rootCauses: plan.rootCauses,
  results,
  runLog,
  epilogue: epilogueReport,
};
if (!epilogueReport.prUpdated) finalReturn.runLogMarkdown = epilogueMarkdown;

// Terminate here. Rerun-watching is /loop / Monitor territory.
return finalReturn;
