package com.shaft.capture.generate;

import java.util.List;
import java.util.Map;

/**
 * Reviewable, fingerprinted AI proposal that is never applied implicitly.
 *
 * @param schemaVersion preview schema version
 * @param deterministicFingerprint fingerprint of the deterministic source and session
 * @param provider provider identifier
 * @param proposal schema-validated proposal
 * @param diff visible proposed changes
 */
public record CaptureEnrichmentPreview(
        String schemaVersion,
        String deterministicFingerprint,
        String provider,
        Proposal proposal,
        List<String> diff) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Optional source naming and assertion suggestions.
     *
     * @param className optional Java class name
     * @param methodName optional Java test method name
     * @param elementNames logical element IDs mapped to Java field names
     * @param assertions additional approved state assertions
     */
    public record Proposal(
            String className,
            String methodName,
            Map<String, String> elementNames,
            List<AssertionSuggestion> assertions) {
        /**
         * Creates an immutable proposal.
         */
        public Proposal {
            className = className == null ? "" : className;
            methodName = methodName == null ? "" : methodName;
            elementNames = elementNames == null ? Map.of() : Map.copyOf(elementNames);
            assertions = assertions == null ? List.of() : List.copyOf(assertions);
        }

        /**
         * Returns an empty deterministic proposal.
         *
         * @return empty proposal
         */
        public static Proposal empty() {
            return new Proposal("", "", Map.of(), List.of());
        }
    }

    /**
     * Additional assertion bound to captured element state.
     *
     * @param eventSequence captured event sequence
     * @param verification verification kind
     * @param negated whether the state assertion is negated
     */
    public record AssertionSuggestion(long eventSequence, String verification, boolean negated) {
    }

    /**
     * Creates an immutable preview.
     */
    public CaptureEnrichmentPreview {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        deterministicFingerprint = deterministicFingerprint == null ? "" : deterministicFingerprint;
        provider = provider == null ? "none" : provider;
        proposal = proposal == null ? Proposal.empty() : proposal;
        diff = diff == null ? List.of() : List.copyOf(diff);
    }
}
