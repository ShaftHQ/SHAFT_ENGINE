package com.shaft.mcp;

import java.util.List;

/**
 * Test automation use case returned by the MCP scenario catalog.
 *
 * @param id stable scenario id
 * @param title short scenario title
 * @param areas supported automation areas
 * @param userPrompts sample user prompts that should route to this scenario
 * @param mcpTools MCP tools the calling agent should consider
 * @param agentActions ordered actions for the calling agent
 * @param repoPattern target repository design pattern
 * @param guardrails rules that must hold for generated or edited code
 * @param completionCriteria observable end state for the flow
 */
public record McpTestAutomationScenario(
        String id,
        String title,
        List<String> areas,
        List<String> userPrompts,
        List<String> mcpTools,
        List<String> agentActions,
        List<String> repoPattern,
        List<String> guardrails,
        List<String> completionCriteria) {
    /**
     * Creates an immutable scenario.
     */
    public McpTestAutomationScenario {
        id = text(id);
        title = text(title);
        areas = copy(areas);
        userPrompts = copy(userPrompts);
        mcpTools = copy(mcpTools);
        agentActions = copy(agentActions);
        repoPattern = copy(repoPattern);
        guardrails = copy(guardrails);
        completionCriteria = copy(completionCriteria);
    }

    private static List<String> copy(List<String> values) {
        return values == null ? List.of() : List.copyOf(values);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }
}
