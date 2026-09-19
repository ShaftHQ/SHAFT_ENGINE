package com.shaft.mcp;

import java.util.List;

/**
 * Playbook analysis of an ingested Design pack. Never writes repository files (issue #5948).
 *
 * @param schemaVersion              analysis schema
 * @param status                     {@code error}, {@code needs_questions}, {@code analysis_complete},
 *                                   or {@code residual_risk_accepted}
 * @param message                    human-readable status
 * @param pack                       nested ingest pack
 * @param gaps                       gap register with stable IDs
 * @param oracles                    evidence oracles for measurable criteria
 * @param blockingCount              unaccepted blocking gaps
 * @param gherkinGenerationAllowed   S1-03 hook; this tool never emits Gherkin
 * @param residualRiskAccepted       true when only accepted waivable blocking gaps remain
 * @param playbook                   playbook identity
 * @param wroteFiles                 always {@code false}
 */
public record McpDesignAnalysis(
        String schemaVersion,
        String status,
        String message,
        McpDesignPack pack,
        List<McpDesignGap> gaps,
        List<McpDesignOracle> oracles,
        int blockingCount,
        boolean gherkinGenerationAllowed,
        boolean residualRiskAccepted,
        String playbook,
        boolean wroteFiles) {
    static final String CURRENT_SCHEMA_VERSION = "1.0";
    static final String PLAYBOOK = "shaft-requirements-analysis";
    static final String STATUS_ERROR = "error";
    static final String STATUS_NEEDS_QUESTIONS = "needs_questions";
    static final String STATUS_COMPLETE = "analysis_complete";
    static final String STATUS_RESIDUAL = "residual_risk_accepted";

    public McpDesignAnalysis {
        gaps = gaps == null ? List.of() : List.copyOf(gaps);
        oracles = oracles == null ? List.of() : List.copyOf(oracles);
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        status = status == null ? STATUS_ERROR : status;
        message = message == null ? "" : message;
        playbook = playbook == null ? PLAYBOOK : playbook;
    }
}
