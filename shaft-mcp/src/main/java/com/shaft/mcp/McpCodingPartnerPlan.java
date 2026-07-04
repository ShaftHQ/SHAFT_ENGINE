package com.shaft.mcp;

import java.util.List;

/**
 * Preview-only coding partner plan for IntelliJ and MCP clients.
 *
 * @param schemaVersion response schema version
 * @param workingSetSummary concise summary of current user intent, source context, and evidence
 * @param backend normalized SHAFT backend
 * @param reuseMatches ranked existing Java classes and anchors to reuse
 * @param missingCodeItems code the caller may still need to create after reuse is exhausted
 * @param suggestedMcpCalls next MCP calls that can prove locators, gather evidence, or verify code
 * @param verificationCommand smallest useful local verification command
 * @param evidencePaths user-supplied evidence paths echoed for PR/review workflow
 * @param warnings guardrails and approval reminders
 */
public record McpCodingPartnerPlan(
        String schemaVersion,
        String workingSetSummary,
        String backend,
        List<McpJavaTargetScanner.Candidate> reuseMatches,
        List<String> missingCodeItems,
        List<String> suggestedMcpCalls,
        String verificationCommand,
        List<String> evidencePaths,
        List<String> warnings) {
    /**
     * Creates an immutable coding partner plan.
     */
    public McpCodingPartnerPlan {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion.trim();
        workingSetSummary = workingSetSummary == null ? "" : workingSetSummary.trim();
        backend = backend == null || backend.isBlank() ? "WebDriver" : backend.trim();
        reuseMatches = reuseMatches == null ? List.of() : List.copyOf(reuseMatches);
        missingCodeItems = missingCodeItems == null ? List.of() : List.copyOf(missingCodeItems);
        suggestedMcpCalls = suggestedMcpCalls == null ? List.of() : List.copyOf(suggestedMcpCalls);
        verificationCommand = verificationCommand == null ? "" : verificationCommand.trim();
        evidencePaths = evidencePaths == null ? List.of() : List.copyOf(evidencePaths);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
