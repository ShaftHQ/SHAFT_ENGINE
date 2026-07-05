package com.shaft.mcp;

import java.util.List;

/**
 * Preview-only coding partner plan for IntelliJ and MCP clients.
 *
 * @param schemaVersion response schema version
 * @param workingSetSummary concise summary of current user intent, source context, and evidence
 * @param backend normalized SHAFT backend
 * @param reuseMatches ranked existing Java classes and anchors to reuse
 * @param stepPlan deterministic user-step plan with reuse and proof guidance
 * @param recommendedTargetSourcePath preferred repository-relative Java source path
 * @param recommendedInsertionAnchor preferred existing method or textual anchor
 * @param missingCodeItems code the caller may still need to create after reuse is exhausted
 * @param suggestedMcpCalls next MCP calls that can prove locators, gather evidence, or verify code
 * @param nextActions structured next MCP actions that IntelliJ and agents can prefill
 * @param verificationCommand smallest useful local verification command
 * @param evidencePaths user-supplied evidence paths echoed for PR/review workflow
 * @param warnings guardrails and approval reminders
 */
public record McpCodingPartnerPlan(
        String schemaVersion,
        String workingSetSummary,
        String backend,
        List<McpJavaTargetScanner.Candidate> reuseMatches,
        List<McpCodingPartnerStep> stepPlan,
        String recommendedTargetSourcePath,
        String recommendedInsertionAnchor,
        List<String> missingCodeItems,
        List<String> suggestedMcpCalls,
        List<McpCodingPartnerNextAction> nextActions,
        String verificationCommand,
        List<String> evidencePaths,
        List<String> warnings) {
    /**
     * Creates an immutable coding partner plan.
     */
    public McpCodingPartnerPlan {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.2" : schemaVersion.trim();
        workingSetSummary = workingSetSummary == null ? "" : workingSetSummary.trim();
        backend = backend == null || backend.isBlank() ? "WebDriver" : backend.trim();
        reuseMatches = reuseMatches == null ? List.of() : List.copyOf(reuseMatches);
        stepPlan = stepPlan == null ? List.of() : List.copyOf(stepPlan);
        recommendedTargetSourcePath = recommendedTargetSourcePath == null ? "" : recommendedTargetSourcePath.trim();
        recommendedInsertionAnchor = recommendedInsertionAnchor == null ? "" : recommendedInsertionAnchor.trim();
        missingCodeItems = missingCodeItems == null ? List.of() : List.copyOf(missingCodeItems);
        suggestedMcpCalls = suggestedMcpCalls == null ? List.of() : List.copyOf(suggestedMcpCalls);
        nextActions = nextActions == null ? List.of() : List.copyOf(nextActions);
        verificationCommand = verificationCommand == null ? "" : verificationCommand.trim();
        evidencePaths = evidencePaths == null ? List.of() : List.copyOf(evidencePaths);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    /**
     * Compatibility constructor for callers compiled against schema 1.1 fields.
     */
    @SuppressWarnings("PMD.ExcessiveParameterList")
    public McpCodingPartnerPlan(
            String schemaVersion,
            String workingSetSummary,
            String backend,
            List<McpJavaTargetScanner.Candidate> reuseMatches,
            List<McpCodingPartnerStep> stepPlan,
            String recommendedTargetSourcePath,
            String recommendedInsertionAnchor,
            List<String> missingCodeItems,
            List<String> suggestedMcpCalls,
            String verificationCommand,
            List<String> evidencePaths,
            List<String> warnings) {
        this(schemaVersion,
                workingSetSummary,
                backend,
                reuseMatches,
                stepPlan,
                recommendedTargetSourcePath,
                recommendedInsertionAnchor,
                missingCodeItems,
                suggestedMcpCalls,
                List.of(),
                verificationCommand,
                evidencePaths,
                warnings);
    }

    /**
     * Compatibility constructor for callers compiled against schema 1.0 fields.
     */
    @SuppressWarnings("PMD.ExcessiveParameterList")
    public McpCodingPartnerPlan(
            String schemaVersion,
            String workingSetSummary,
            String backend,
            List<McpJavaTargetScanner.Candidate> reuseMatches,
            List<String> missingCodeItems,
            List<String> suggestedMcpCalls,
            String verificationCommand,
            List<String> evidencePaths,
            List<String> warnings) {
        this(schemaVersion,
                workingSetSummary,
                backend,
                reuseMatches,
                List.of(),
                "",
                "",
                missingCodeItems,
                suggestedMcpCalls,
                List.of(),
                verificationCommand,
                evidencePaths,
                warnings);
    }
}
