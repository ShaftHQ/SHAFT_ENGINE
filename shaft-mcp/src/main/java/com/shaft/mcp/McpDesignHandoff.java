package com.shaft.mcp;

import java.util.List;
import java.util.Map;

/**
 * Design→Automation handoff pack (issue #5956). Never writes Java, locators, or repository files.
 *
 * @param schemaVersion      handoff schema
 * @param status             {@code ready}, {@code blocked}, or {@code error}
 * @param message            human-readable status
 * @param unmetConditions    blockers when status is not ready
 * @param scenarios          scenario titles extracted from accepted Gherkin
 * @param acceptanceCriteria AC IDs traced from coverage / tags
 * @param examples           example-table rows (headers + cells) when present
 * @param gapMap             fluent gap-map step classifications
 * @param oracles            evidence oracles from analysis
 * @param optionalUrl        caller-supplied URL; never invented
 * @param automationPrefill  typed prefill map for Automation canvas (intent/oracles/data)
 * @param wroteFiles         always {@code false}
 */
public record McpDesignHandoff(
        String schemaVersion,
        String status,
        String message,
        List<String> unmetConditions,
        List<String> scenarios,
        List<String> acceptanceCriteria,
        List<Map<String, String>> examples,
        List<McpDesignGapMapStep> gapMap,
        List<McpDesignOracle> oracles,
        String optionalUrl,
        Map<String, String> automationPrefill,
        boolean wroteFiles) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";
    public static final String STATUS_READY = "ready";
    public static final String STATUS_BLOCKED = "blocked";
    public static final String STATUS_ERROR = "error";

    public McpDesignHandoff {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        unmetConditions = unmetConditions == null ? List.of() : List.copyOf(unmetConditions);
        scenarios = scenarios == null ? List.of() : List.copyOf(scenarios);
        acceptanceCriteria = acceptanceCriteria == null ? List.of() : List.copyOf(acceptanceCriteria);
        examples = examples == null ? List.of() : List.copyOf(examples);
        gapMap = gapMap == null ? List.of() : List.copyOf(gapMap);
        oracles = oracles == null ? List.of() : List.copyOf(oracles);
        optionalUrl = optionalUrl == null ? "" : optionalUrl;
        automationPrefill = automationPrefill == null ? Map.of() : Map.copyOf(automationPrefill);
    }
}
