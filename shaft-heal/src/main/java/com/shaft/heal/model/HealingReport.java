package com.shaft.heal.model;

import java.util.List;
import java.util.Objects;

/**
 * Structured, explainable SHAFT Heal attempt report.
 *
 * @param schemaVersion report schema version
 * @param attemptId safe correlation identifier
 * @param timestamp UTC timestamp
 * @param originalLocator original failed locator
 * @param failureCategory normalized failure category
 * @param pageKey query-free page identity
 * @param context frame and shadow context
 * @param candidates ranked deterministic candidates
 * @param decision final decision
 * @param provider provider and fallback metadata
 * @param privacy minimization and redaction metadata
 * @param action action and post-action metadata
 */
public record HealingReport(
        String schemaVersion,
        String attemptId,
        String timestamp,
        String originalLocator,
        String failureCategory,
        String pageKey,
        String context,
        List<HealingCandidate> candidates,
        HealingDecision decision,
        ProviderMetadata provider,
        PrivacyMetadata privacy,
        ActionMetadata action) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Creates an immutable report.
     */
    public HealingReport {
        schemaVersion = Objects.requireNonNullElse(schemaVersion, CURRENT_SCHEMA_VERSION);
        attemptId = Objects.requireNonNullElse(attemptId, "");
        timestamp = Objects.requireNonNullElse(timestamp, "");
        originalLocator = Objects.requireNonNullElse(originalLocator, "");
        failureCategory = Objects.requireNonNullElse(failureCategory, "LOCATOR_NOT_FOUND");
        pageKey = Objects.requireNonNullElse(pageKey, "");
        context = Objects.requireNonNullElse(context, "");
        candidates = candidates == null ? List.of() : List.copyOf(candidates);
        decision = Objects.requireNonNull(decision, "decision");
        provider = provider == null ? ProviderMetadata.disabled() : provider;
        privacy = privacy == null ? PrivacyMetadata.minimized() : privacy;
        action = action == null ? ActionMetadata.pending("") : action;
    }

    /**
     * Optional provider metadata.
     *
     * @param enabled whether reranking was requested
     * @param provider provider identifier
     * @param model model or configuration identifier
     * @param status provider status
     * @param fallbackReason deterministic fallback reason
     */
    public record ProviderMetadata(
            boolean enabled,
            String provider,
            String model,
            String status,
            String fallbackReason) {
        /**
         * Creates safe provider metadata.
         */
        public ProviderMetadata {
            provider = Objects.requireNonNullElse(provider, "none");
            model = Objects.requireNonNullElse(model, "");
            status = Objects.requireNonNullElse(status, "DISABLED");
            fallbackReason = Objects.requireNonNullElse(fallbackReason, "");
        }

        /**
         * @return disabled provider metadata
         */
        public static ProviderMetadata disabled() {
            return new ProviderMetadata(false, "none", "", "DISABLED", "");
        }
    }

    /**
     * Privacy metadata.
     *
     * @param policy evidence policy summary
     * @param redactedFields number of redacted fields
     * @param remoteEvidenceSent whether evidence left the local process
     */
    public record PrivacyMetadata(String policy, int redactedFields, boolean remoteEvidenceSent) {
        /**
         * Creates safe privacy metadata.
         */
        public PrivacyMetadata {
            policy = Objects.requireNonNullElse(policy, "");
            redactedFields = Math.max(0, redactedFields);
        }

        /**
         * @return metadata for whitelist-only local evidence
         */
        public static PrivacyMetadata minimized() {
            return new PrivacyMetadata(
                    "Whitelist-only semantic evidence; values, cookies, authorization data, and full DOM are excluded.",
                    0,
                    false);
        }
    }

    /**
     * Action metadata.
     *
     * @param name intended action
     * @param recoveryUsed whether recovery was used
     * @param outcome pending, passed, or failed
     * @param postActionVerification verification result or explicit unverifiable state
     * @param failure safe failure category
     */
    public record ActionMetadata(
            String name,
            boolean recoveryUsed,
            String outcome,
            String postActionVerification,
            String failure) {
        /**
         * Creates safe action metadata.
         */
        public ActionMetadata {
            name = Objects.requireNonNullElse(name, "");
            outcome = Objects.requireNonNullElse(outcome, "PENDING");
            postActionVerification = Objects.requireNonNullElse(postActionVerification, "UNVERIFIABLE");
            failure = Objects.requireNonNullElse(failure, "");
        }

        /**
         * Creates pending action metadata.
         *
         * @param name action name
         * @return pending metadata
         */
        public static ActionMetadata pending(String name) {
            return new ActionMetadata(name, true, "PENDING", "UNVERIFIABLE", "");
        }
    }
}
