package com.shaft.mcp;

import com.shaft.ai.agentic.AgenticProposal;
import com.shaft.ai.agentic.AgenticSeed;
import com.shaft.ai.agentic.GovernedAgenticWorkflow;
import com.shaft.ai.agentic.UntrustedModelAdvice;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * MCP entrypoint for the governed agentic test workflow fixture (issue #5452).
 *
 * <p>Deterministic, review-first, fail-closed. Model/provider choice is out of scope.
 * Model output is never test or cleanup authority.</p>
 */
@Service
public class GovernedAgenticWorkflowService {
    private final GovernedAgenticWorkflow workflow;

    /**
     * Creates the default service.
     */
    public GovernedAgenticWorkflowService() {
        this(new GovernedAgenticWorkflow());
    }

    GovernedAgenticWorkflowService(GovernedAgenticWorkflow workflow) {
        this.workflow = workflow == null ? new GovernedAgenticWorkflow() : workflow;
    }

    /**
     * Runs the governed plan→generate→run→diagnose→propose fixture and returns a review-only proposal.
     *
     * @param journeyId stable journey id used in the deterministic seed
     * @param journeyText requested user journey
     * @param pageContent optional untrusted page content (sanitized; injection fails closed)
     * @param simulatedRunnerExitCode fixture runner exit code; {@code 0} means pass
     * @param suggestTestMutation optional model-suggested test path (denied and audited)
     * @param suggestCleanup whether model advice requests cleanup (denied)
     * @param suggestDestructiveBrowser whether model advice requests destructive browser actions (denied)
     * @param suggestCredentialRead whether model advice requests credential reads (denied)
     * @param alternateDiagnosis optional disagreeing model diagnosis (fail-closed when it conflicts)
     * @return reproducible proposal with provenance; always requires human review
     */
    @Tool(name = "agentic_workflow_run_fixture",
            description = "runs the governed agentic test workflow fixture (plan, generate, run, diagnose, propose) "
                    + "with deterministic seed bindings, scoped tools/permissions/budgets, provenance, and "
                    + "fail-closed trust boundaries; returns a review-only proposal (never applies tests or cleanup)")
    public McpAgenticWorkflowResult runFixture(
            String journeyId,
            String journeyText,
            @ToolParam(required = false) String pageContent,
            @ToolParam(required = false) Integer simulatedRunnerExitCode,
            @ToolParam(required = false) String suggestTestMutation,
            @ToolParam(required = false) Boolean suggestCleanup,
            @ToolParam(required = false) Boolean suggestDestructiveBrowser,
            @ToolParam(required = false) Boolean suggestCredentialRead,
            @ToolParam(required = false) String alternateDiagnosis) {
        AgenticSeed seed = AgenticSeed.of(journeyId, journeyText, pageContent);
        UntrustedModelAdvice advice = new UntrustedModelAdvice(
                suggestTestMutation,
                Boolean.TRUE.equals(suggestCleanup),
                Boolean.TRUE.equals(suggestDestructiveBrowser),
                Boolean.TRUE.equals(suggestCredentialRead),
                alternateDiagnosis,
                List.of());
        int exitCode = simulatedRunnerExitCode == null ? 0 : simulatedRunnerExitCode;
        AgenticProposal proposal = workflow.runFixture(seed, advice, exitCode);
        return McpAgenticWorkflowResult.from(proposal);
    }
}
