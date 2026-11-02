package com.shaft.mcp;

import com.shaft.ai.agentic.AgenticProposal;
import com.shaft.ai.agentic.ProvenanceRecord;

import java.util.List;

/**
 * MCP view of a governed agentic workflow proposal.
 *
 * @param schemaVersion result schema version
 * @param journeyId journey id
 * @param seedHash deterministic seed hash
 * @param status workflow status name
 * @param humanVisibleDiff review-only diff text
 * @param recommendedTargetPath informational target path
 * @param requiresHumanReview always {@code true}
 * @param provenanceEntries compact provenance lines
 * @param warnings warnings and fail-closed reasons
 */
public record McpAgenticWorkflowResult(
        String schemaVersion,
        String journeyId,
        String seedHash,
        String status,
        String humanVisibleDiff,
        String recommendedTargetPath,
        boolean requiresHumanReview,
        List<String> provenanceEntries,
        List<String> warnings) {

    /**
     * Current MCP result schema.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable MCP result.
     */
    public McpAgenticWorkflowResult {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        journeyId = journeyId == null ? "" : journeyId;
        seedHash = seedHash == null ? "" : seedHash;
        status = status == null ? AgenticProposal.Status.STOPPED_FAIL_CLOSED.name() : status;
        humanVisibleDiff = humanVisibleDiff == null ? "" : humanVisibleDiff;
        recommendedTargetPath = recommendedTargetPath == null ? "" : recommendedTargetPath;
        requiresHumanReview = true;
        provenanceEntries = provenanceEntries == null ? List.of() : List.copyOf(provenanceEntries);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }

    /**
     * Maps a core proposal into the MCP result shape.
     *
     * @param proposal core proposal
     * @return MCP result
     */
    public static McpAgenticWorkflowResult from(AgenticProposal proposal) {
        List<String> entries = proposal.provenance().stream()
                .map(McpAgenticWorkflowResult::format)
                .toList();
        return new McpAgenticWorkflowResult(
                CURRENT_SCHEMA_VERSION,
                proposal.journeyId(),
                proposal.seedHash(),
                proposal.status().name(),
                proposal.humanVisibleDiff(),
                proposal.recommendedTargetPath(),
                true,
                entries,
                proposal.warnings());
    }

    private static String format(ProvenanceRecord record) {
        return record.phase()
                + "|seed=" + record.seedHash()
                + "|decision=" + record.decision()
                + "|commands=" + String.join(";", record.commands())
                + "|artifacts=" + String.join(";", record.artifacts())
                + "|denied=" + String.join(";", record.deniedMutations());
    }
}
