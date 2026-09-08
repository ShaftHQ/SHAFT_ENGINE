package com.shaft.ai.agentic;

import java.util.List;
import java.util.Objects;

/**
 * One inspectable provenance entry (FR-3).
 *
 * @param phase producing phase
 * @param seedHash seed hash at the time of the record
 * @param decision human-readable decision or outcome
 * @param commands commands considered or executed (never applied from model authority)
 * @param artifacts artifact paths or logical names
 * @param deniedMutations audited denial reasons for trust-boundary attempts
 */
public record ProvenanceRecord(
        AgenticPhase phase,
        String seedHash,
        String decision,
        List<String> commands,
        List<String> artifacts,
        List<String> deniedMutations) {
    /**
     * Creates an immutable provenance record.
     */
    public ProvenanceRecord {
        Objects.requireNonNull(phase, "phase");
        seedHash = Objects.requireNonNullElse(seedHash, "");
        decision = Objects.requireNonNullElse(decision, "");
        commands = commands == null ? List.of() : List.copyOf(commands);
        artifacts = artifacts == null ? List.of() : List.copyOf(artifacts);
        deniedMutations = deniedMutations == null ? List.of() : List.copyOf(deniedMutations);
    }
}
