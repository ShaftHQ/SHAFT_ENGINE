package com.shaft.ai.agentic;

import java.util.List;
import java.util.Objects;

/**
 * Human-visible, review-required proposal. Never auto-applied (FR-3, FR-4).
 *
 * @param schemaVersion proposal schema version
 * @param journeyId journey id
 * @param seedHash seed hash
 * @param status workflow status
 * @param humanVisibleDiff unified-diff style text for human/policy review
 * @param recommendedTargetPath suggested path (informational only)
 * @param requiresHumanReview always {@code true} for this workflow
 * @param provenance complete provenance ledger
 * @param warnings fail-closed warnings
 */
public record AgenticProposal(
        String schemaVersion,
        String journeyId,
        String seedHash,
        Status status,
        String humanVisibleDiff,
        String recommendedTargetPath,
        boolean requiresHumanReview,
        List<ProvenanceRecord> provenance,
        List<String> warnings) {

    /**
     * Current proposal schema.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Workflow completion status.
     */
    public enum Status {
        PROPOSAL_READY,
        STOPPED_BUDGET_EXHAUSTED,
        STOPPED_TRUST_BOUNDARY,
        STOPPED_INCONSISTENT_SEED,
        STOPPED_UNAVAILABLE_TOOL,
        STOPPED_PROMPT_INJECTION,
        STOPPED_MODEL_DISAGREEMENT,
        STOPPED_FAIL_CLOSED
    }

    /**
     * Creates an immutable proposal. Human review is always required.
     */
    public AgenticProposal {
        schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                ? CURRENT_SCHEMA_VERSION
                : schemaVersion.trim();
        journeyId = Objects.requireNonNullElse(journeyId, "");
        seedHash = Objects.requireNonNullElse(seedHash, "");
        status = status == null ? Status.STOPPED_FAIL_CLOSED : status;
        humanVisibleDiff = Objects.requireNonNullElse(humanVisibleDiff, "");
        recommendedTargetPath = Objects.requireNonNullElse(recommendedTargetPath, "");
        requiresHumanReview = true;
        provenance = provenance == null ? List.of() : List.copyOf(provenance);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
    }
}
