package com.shaft.ai.agentic;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Deterministic governed agentic test workflow: plan → generate → run → diagnose → propose.
 *
 * <p>Scripts and fixtures drive phases. Model output is never test or cleanup authority (FR-4).
 * Human/policy review remains required. Model/provider choice is out of scope.</p>
 */
public final class GovernedAgenticWorkflow {
    private static final Set<String> ALLOWLISTED_MAVEN_GOALS = Set.of(
            "test", "test-compile", "surefire:test");

    private final Map<AgenticPhase, PhaseBudget> budgetOverrides;
    private final Set<String> unavailableTools;

    /**
     * Creates a workflow with default fixture budgets and all catalog tools available.
     */
    public GovernedAgenticWorkflow() {
        this(Map.of(), Set.of());
    }

    /**
     * Creates a workflow with optional budget overrides and unavailable tools (for fail-closed tests).
     *
     * @param budgetOverrides per-phase budget overrides
     * @param unavailableTools tools treated as unavailable
     */
    public GovernedAgenticWorkflow(Map<AgenticPhase, PhaseBudget> budgetOverrides, Set<String> unavailableTools) {
        this.budgetOverrides = budgetOverrides == null ? Map.of() : Map.copyOf(budgetOverrides);
        this.unavailableTools = unavailableTools == null
                ? Set.of()
                : Set.copyOf(unavailableTools.stream()
                        .filter(Objects::nonNull)
                        .map(value -> value.trim().toLowerCase(Locale.ROOT))
                        .filter(value -> !value.isBlank())
                        .toList());
    }

    /**
     * Runs the end-to-end fixture workflow and returns a reproducible proposal with provenance (SC-1).
     *
     * @param seed deterministic seed
     * @param advice untrusted model advice (may be {@link UntrustedModelAdvice#none()})
     * @param simulatedRunnerExitCode fixture runner exit code ({@code 0} = pass)
     * @return proposal requiring human review
     */
    public AgenticProposal runFixture(AgenticSeed seed, UntrustedModelAdvice advice, int simulatedRunnerExitCode) {
        Objects.requireNonNull(seed, "seed");
        UntrustedModelAdvice modelAdvice = advice == null ? UntrustedModelAdvice.none() : advice;
        List<ProvenanceRecord> provenance = new ArrayList<>();
        List<String> warnings = new ArrayList<>();

        SeedAdmission admission = admitSeed(seed, provenance, warnings);
        if (admission.stop() != null) {
            return admission.stop();
        }
        return executePhases(seed, modelAdvice, simulatedRunnerExitCode, admission.sanitizedPage(), provenance, warnings);
    }

    private SeedAdmission admitSeed(
            AgenticSeed seed,
            List<ProvenanceRecord> provenance,
            List<String> warnings) {
        if (!seed.isConsistent()) {
            return SeedAdmission.stopped(stopped(seed, AgenticProposal.Status.STOPPED_INCONSISTENT_SEED, provenance,
                    warnings, "Inconsistent seed hash"));
        }

        TrustBoundary.SanitizedContent sanitized = TrustBoundary.sanitizePage(seed.pageContent());
        if (sanitized.credentialsDetected()) {
            warnings.add("Credential-like material redacted from untrusted page content");
        }
        if (sanitized.promptInjectionDetected()) {
            provenance.add(record(
                    AgenticPhase.PLANNER,
                    seed,
                    "fail-closed: prompt injection in untrusted page content",
                    List.of(),
                    List.of("sanitized-page"),
                    List.of(AgenticPhase.PLANNER + ": prompt injection denied")));
            return SeedAdmission.stopped(stopped(seed, AgenticProposal.Status.STOPPED_PROMPT_INJECTION, provenance,
                    warnings, "Prompt injection in untrusted page content"));
        }
        if (TrustBoundary.looksLikePromptInjection(seed.journeyText())) {
            provenance.add(record(
                    AgenticPhase.PLANNER,
                    seed,
                    "fail-closed: prompt injection in journey text",
                    List.of(),
                    List.of(),
                    List.of(AgenticPhase.PLANNER + ": prompt injection denied")));
            return SeedAdmission.stopped(stopped(seed, AgenticProposal.Status.STOPPED_PROMPT_INJECTION, provenance,
                    warnings, "Prompt injection in journey text"));
        }
        return SeedAdmission.ok(sanitized.text());
    }

    private AgenticProposal executePhases(
            AgenticSeed seed,
            UntrustedModelAdvice modelAdvice,
            int simulatedRunnerExitCode,
            String sanitizedPage,
            List<ProvenanceRecord> provenance,
            List<String> warnings) {
        PhaseOutcome planner = runPlanner(seed, sanitizedPage, provenance);
        if (planner.stopStatus() != null) {
            return stopped(seed, planner.stopStatus(), provenance, warnings, planner.stopReason());
        }

        PhaseOutcome generator = runGenerator(seed, planner.draft(), provenance, warnings, modelAdvice);
        if (generator.stopStatus() != null) {
            return stopped(seed, generator.stopStatus(), provenance, warnings, generator.stopReason());
        }

        PhaseOutcome runner = runRunner(seed, simulatedRunnerExitCode, provenance, warnings);
        if (runner.stopStatus() != null) {
            return stopped(seed, runner.stopStatus(), provenance, warnings, runner.stopReason());
        }

        PhaseOutcome diagnoser = runDiagnoser(seed, runner.draft(), modelAdvice, provenance, warnings);
        if (diagnoser.stopStatus() != null) {
            return stopped(seed, diagnoser.stopStatus(), provenance, warnings, diagnoser.stopReason());
        }

        return runProposal(seed, generator.draft(), diagnoser.draft(), provenance, warnings);
    }

    private PhaseOutcome runPlanner(
            AgenticSeed seed,
            String sanitizedPage,
            List<ProvenanceRecord> provenance) {
        PhaseBinding binding = binding(AgenticPhase.PLANNER, seed);
        List<String> denials = new ArrayList<>();
        List<String> commands = new ArrayList<>();
        List<String> artifacts = new ArrayList<>();

        PhaseOutcome toolGate = gateTools(binding, List.of("read_journey", "sanitize_page", "list_flows"), denials);
        if (toolGate != null) {
            provenance.add(record(AgenticPhase.PLANNER, seed, toolGate.stopReason(), commands, artifacts, denials));
            return toolGate;
        }
        if (binding.budget().maxSteps() < 1 || binding.budget().maxToolCalls() < 3) {
            String reason = "Planner budget exhausted before producing a plan";
            provenance.add(record(AgenticPhase.PLANNER, seed, reason, commands, artifacts, denials));
            return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED, reason);
        }

        commands.add("read_journey:" + seed.journeyId());
        commands.add("sanitize_page");
        commands.add("list_flows");
        artifacts.add("plan/" + seed.journeyId() + ".md");
        String plan = """
                # Plan for %s
                1. Open target from journey
                2. Exercise primary happy path
                3. Assert expected outcome
                Page evidence (sanitized): %s
                """.formatted(seed.journeyId(), summarize(sanitizedPage));
        provenance.add(record(AgenticPhase.PLANNER, seed, "planned journey", commands, artifacts, denials));
        return PhaseOutcome.ok(plan);
    }

    private PhaseOutcome runGenerator(
            AgenticSeed seed,
            String plan,
            List<ProvenanceRecord> provenance,
            List<String> warnings,
            UntrustedModelAdvice advice) {
        PhaseBinding binding = binding(AgenticPhase.GENERATOR, seed);
        List<String> denials = new ArrayList<>(TrustBoundary.auditModelAdvice(
                AgenticPhase.GENERATOR, binding.permissions(), advice));
        List<String> commands = new ArrayList<>();
        List<String> artifacts = new ArrayList<>();

        if (!denials.isEmpty()) {
            warnings.addAll(denials);
            provenance.add(record(
                    AgenticPhase.GENERATOR,
                    seed,
                    "denied trust-boundary mutations from model advice",
                    commands,
                    artifacts,
                    denials));
            // SC-2: denials are audited; workflow continues only with draft-only generation.
        }

        PhaseOutcome toolGate = gateTools(binding, List.of("draft_test", "render_diff"), denials);
        if (toolGate != null) {
            provenance.add(record(AgenticPhase.GENERATOR, seed, toolGate.stopReason(), commands, artifacts, denials));
            return toolGate;
        }
        if (binding.budget().maxSteps() < 1) {
            String reason = "Generator budget exhausted; approved tests unchanged";
            provenance.add(record(AgenticPhase.GENERATOR, seed, reason, commands, artifacts, denials));
            return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED, reason);
        }

        commands.add("draft_test");
        commands.add("render_diff");
        artifacts.add("draft/" + seed.journeyId() + "Test.java.diff");
        String className = toClassName(seed.journeyId());
        String draft = """
                package fixtures;

                import com.shaft.driver.SHAFT;
                import org.testng.annotations.Test;

                public class %s {
                    @Test
                    public void %s() {
                        // Generated from plan (review required; not applied):
                        // %s
                        SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
                        driver.browser().navigateToURL("https://example.invalid/");
                        driver.quit();
                    }
                }
                """.formatted(className, toMethodName(seed.journeyId()), firstLine(plan));
        provenance.add(record(
                AgenticPhase.GENERATOR,
                seed,
                "drafted review-only test; approved sources untouched",
                commands,
                artifacts,
                denials));
        return PhaseOutcome.ok(draft);
    }

    private PhaseOutcome runRunner(
            AgenticSeed seed,
            int exitCode,
            List<ProvenanceRecord> provenance,
            List<String> warnings) {
        PhaseBinding binding = binding(AgenticPhase.RUNNER, seed);
        List<String> denials = new ArrayList<>();
        List<String> commands = new ArrayList<>();
        List<String> artifacts = new ArrayList<>();

        PhaseOutcome toolGate = gateTools(binding, List.of("run_allowlisted_maven", "record_artifact"), denials);
        if (toolGate != null) {
            provenance.add(record(AgenticPhase.RUNNER, seed, toolGate.stopReason(), commands, artifacts, denials));
            return toolGate;
        }
        if (binding.budget().maxRetries() < 0
                || (binding.budget().maxToolCalls() < 1 && binding.budget().maxSteps() < 1)) {
            String reason = "Runner budget exhausted; approved tests unchanged";
            provenance.add(record(AgenticPhase.RUNNER, seed, reason, commands, artifacts, denials));
            return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED, reason);
        }

        String goal = "test";
        if (!ALLOWLISTED_MAVEN_GOALS.contains(goal)) {
            String reason = "Maven goal not allowlisted";
            provenance.add(record(AgenticPhase.RUNNER, seed, reason, commands, artifacts, denials));
            return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_FAIL_CLOSED, reason);
        }

        String command = "mvn -q -DheadlessExecution=true -Dallure.automaticallyOpen=false " + goal
                + " -Dtest=" + toClassName(seed.journeyId());
        commands.add(command);
        artifacts.add("target/agentic-fixture/" + seed.journeyId() + "/surefire-summary.txt");
        String summary = exitCode == 0 ? "PASSED" : "FAILED exitCode=" + exitCode;
        provenance.add(record(AgenticPhase.RUNNER, seed, "recorded fixture run: " + summary, commands, artifacts, denials));
        if (binding.budget().maxRetries() == 0 && exitCode != 0) {
            warnings.add("Runner retries exhausted; stopping without altering approved tests");
            return PhaseOutcome.stop(
                    AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED,
                    "Runner retries exhausted; approved tests unchanged");
        }
        return PhaseOutcome.ok(summary);
    }

    private PhaseOutcome runDiagnoser(
            AgenticSeed seed,
            String runSummary,
            UntrustedModelAdvice advice,
            List<ProvenanceRecord> provenance,
            List<String> warnings) {
        PhaseBinding binding = binding(AgenticPhase.DIAGNOSER, seed);
        List<String> denials = new ArrayList<>(TrustBoundary.auditModelAdvice(
                AgenticPhase.DIAGNOSER, binding.permissions(), advice));
        List<String> commands = new ArrayList<>();
        List<String> artifacts = new ArrayList<>();

        if (!denials.isEmpty()) {
            warnings.addAll(denials);
            // SC-2: deny + audit; do not treat advice as authority. Continue diagnosis without applying mutations.
            provenance.add(record(
                    AgenticPhase.DIAGNOSER,
                    seed,
                    "trust-boundary mutations denied and audited",
                    List.of(),
                    List.of(),
                    denials));
        }

        PhaseOutcome toolGate = gateTools(
                binding, List.of("read_artifact", "classify_failure", "consider_model_advice"), denials);
        if (toolGate != null) {
            provenance.add(record(AgenticPhase.DIAGNOSER, seed, toolGate.stopReason(), commands, artifacts, denials));
            return toolGate;
        }
        if (binding.budget().maxSteps() < 1) {
            String reason = "Diagnoser budget exhausted; approved tests unchanged";
            provenance.add(record(AgenticPhase.DIAGNOSER, seed, reason, commands, artifacts, denials));
            return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED, reason);
        }

        commands.add("read_artifact");
        commands.add("classify_failure");
        commands.add("consider_model_advice");
        String diagnosis = runSummary.startsWith("PASSED") ? "stable" : "locator_or_assertion";
        if (advice.disagreesWith(diagnosis)) {
            warnings.add("Model disagreement: primary=" + diagnosis + " alternate=" + advice.alternateDiagnosis());
            provenance.add(record(
                    AgenticPhase.DIAGNOSER,
                    seed,
                    "model disagreement fail-closed",
                    commands,
                    artifacts,
                    denials));
            return PhaseOutcome.stop(
                    AgenticProposal.Status.STOPPED_MODEL_DISAGREEMENT,
                    "Model disagreement requires human review before any change");
        }
        artifacts.add("diagnosis/" + seed.journeyId() + ".txt");
        provenance.add(record(AgenticPhase.DIAGNOSER, seed, "diagnosis=" + diagnosis, commands, artifacts, denials));
        return PhaseOutcome.ok(diagnosis);
    }

    private AgenticProposal runProposal(
            AgenticSeed seed,
            String draft,
            String diagnosis,
            List<ProvenanceRecord> provenance,
            List<String> warnings) {
        PhaseBinding binding = binding(AgenticPhase.PROPOSAL, seed);
        List<String> denials = new ArrayList<>();
        List<String> commands = new ArrayList<>();
        List<String> artifacts = new ArrayList<>();

        PhaseOutcome toolGate = gateTools(binding, List.of("write_proposal", "write_provenance"), denials);
        if (toolGate != null) {
            provenance.add(record(AgenticPhase.PROPOSAL, seed, toolGate.stopReason(), commands, artifacts, denials));
            return stopped(seed, toolGate.stopStatus(), provenance, warnings, toolGate.stopReason());
        }
        if (binding.budget().maxSteps() < 1) {
            String reason = "Proposal budget exhausted; approved tests unchanged";
            provenance.add(record(AgenticPhase.PROPOSAL, seed, reason, commands, artifacts, denials));
            return stopped(seed, AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED, provenance, warnings, reason);
        }

        String target = "src/test/java/fixtures/" + toClassName(seed.journeyId()) + ".java";
        if (TrustBoundary.isApprovedTestPath(target) && binding.permissions().mayMutateApprovedTests()) {
            // Unreachable with default permissions; keep fail-closed guard.
            denials.add(AgenticPhase.PROPOSAL + ": refusing to apply approved test mutation");
            provenance.add(record(AgenticPhase.PROPOSAL, seed, "blocked apply", commands, artifacts, denials));
            return stopped(seed, AgenticProposal.Status.STOPPED_TRUST_BOUNDARY, provenance, warnings,
                    "Refusing to apply approved test mutation");
        }

        commands.add("write_proposal");
        commands.add("write_provenance");
        artifacts.add("proposals/" + seed.journeyId() + ".diff");
        artifacts.add("proposals/" + seed.journeyId() + ".provenance.json");
        String diff = """
                --- /dev/null
                +++ b/%s
                @@ review-only; not applied @@
                %s
                # diagnosis: %s
                # seed: %s
                """.formatted(target, draft.indent(1).stripTrailing(), diagnosis, seed.seedHash());
        provenance.add(record(
                AgenticPhase.PROPOSAL,
                seed,
                "emitted review-only proposal with complete provenance",
                commands,
                artifacts,
                denials));
        warnings.add("Human/policy review required before any test change");
        return new AgenticProposal(
                AgenticProposal.CURRENT_SCHEMA_VERSION,
                seed.journeyId(),
                seed.seedHash(),
                AgenticProposal.Status.PROPOSAL_READY,
                diff,
                target,
                true,
                provenance,
                warnings);
    }

    private PhaseBinding binding(AgenticPhase phase, AgenticSeed seed) {
        PhaseBinding base = PhaseBinding.forPhase(phase, seed);
        PhaseBudget override = budgetOverrides.get(phase);
        if (override == null) {
            return base;
        }
        return new PhaseBinding(phase, seed, base.permissions(), override);
    }

    private PhaseOutcome gateTools(PhaseBinding binding, List<String> tools, List<String> denials) {
        for (String tool : tools) {
            if (!TrustBoundary.isKnownTool(tool) || unavailableTools.contains(tool)) {
                String reason = "Unavailable tool: " + tool;
                denials.add(binding.phase() + ": " + reason);
                return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_UNAVAILABLE_TOOL, reason);
            }
            if (!binding.permissions().allowsTool(tool)) {
                String reason = "Tool not permitted in " + binding.phase() + ": " + tool;
                denials.add(binding.phase() + ": " + reason);
                return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_FAIL_CLOSED, reason);
            }
        }
        if (binding.budget().maxToolCalls() < tools.size()) {
            String reason = binding.phase() + " tool budget exhausted; approved tests unchanged";
            denials.add(reason);
            return PhaseOutcome.stop(AgenticProposal.Status.STOPPED_BUDGET_EXHAUSTED, reason);
        }
        return null;
    }

    private static ProvenanceRecord record(
            AgenticPhase phase,
            AgenticSeed seed,
            String decision,
            List<String> commands,
            List<String> artifacts,
            List<String> denials) {
        return new ProvenanceRecord(phase, seed.seedHash(), decision, commands, artifacts, denials);
    }

    private static AgenticProposal stopped(
            AgenticSeed seed,
            AgenticProposal.Status status,
            List<ProvenanceRecord> provenance,
            List<String> warnings,
            String reason) {
        List<String> mergedWarnings = new ArrayList<>(warnings);
        if (reason != null && !reason.isBlank()) {
            mergedWarnings.add(reason);
        }
        return new AgenticProposal(
                AgenticProposal.CURRENT_SCHEMA_VERSION,
                seed.journeyId(),
                seed.seedHash(),
                status,
                "",
                "",
                true,
                provenance,
                mergedWarnings);
    }

    private static String toClassName(String journeyId) {
        StringBuilder builder = new StringBuilder("Governed");
        for (String part : journeyId.split("[^A-Za-z0-9]+")) {
            if (part.isBlank()) {
                continue;
            }
            builder.append(Character.toUpperCase(part.charAt(0)));
            if (part.length() > 1) {
                builder.append(part.substring(1));
            }
        }
        builder.append("Test");
        return builder.toString();
    }

    private static String toMethodName(String journeyId) {
        String className = toClassName(journeyId);
        if (className.endsWith("Test") && className.length() > 4) {
            String stem = className.substring(0, className.length() - 4);
            return Character.toLowerCase(stem.charAt(0)) + stem.substring(1);
        }
        return "executeJourney";
    }

    private static String firstLine(String text) {
        if (text == null || text.isBlank()) {
            return "";
        }
        int newline = text.indexOf('\n');
        return newline < 0 ? text.trim() : text.substring(0, newline).trim();
    }

    private static String summarize(String text) {
        if (text == null || text.isBlank()) {
            return "(none)";
        }
        String collapsed = text.replaceAll("\\s+", " ").trim();
        return collapsed.length() <= 120 ? collapsed : collapsed.substring(0, 117) + "...";
    }

    private record PhaseOutcome(String draft, AgenticProposal.Status stopStatus, String stopReason) {
        static PhaseOutcome ok(String draft) {
            return new PhaseOutcome(draft, null, null);
        }

        static PhaseOutcome stop(AgenticProposal.Status status, String reason) {
            return new PhaseOutcome("", status, reason);
        }
    }

    private record SeedAdmission(String sanitizedPage, AgenticProposal stop) {
        static SeedAdmission ok(String sanitizedPage) {
            return new SeedAdmission(sanitizedPage, null);
        }

        static SeedAdmission stopped(AgenticProposal stop) {
            return new SeedAdmission("", stop);
        }
    }

    /**
     * Convenience builder for tests that need a single-phase budget override map.
     *
     * @param phase phase
     * @param budget budget
     * @return singleton map
     */
    public static Map<AgenticPhase, PhaseBudget> budgetOverride(AgenticPhase phase, PhaseBudget budget) {
        Map<AgenticPhase, PhaseBudget> map = new LinkedHashMap<>();
        map.put(phase, budget);
        return Map.copyOf(map);
    }
}
