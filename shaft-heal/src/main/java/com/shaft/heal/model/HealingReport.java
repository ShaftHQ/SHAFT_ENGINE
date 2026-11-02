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
 * @param contextMetadata platform and execution context metadata
 * @param candidates ranked deterministic candidates
 * @param decision final decision
 * @param provider provider and fallback metadata
 * @param privacy minimization and redaction metadata
 * @param action action and post-action metadata
 * @param ladder re-suggestion ladder metadata (issue #4027)
 * @param visibilityRequired whether this attempt required the selected candidate to be visible
 *                            and enabled, mirroring {@code HealingDecisionEngine}'s own
 *                            eligibility gate (issue #4215)
 */
public record HealingReport(
        String schemaVersion,
        String attemptId,
        String timestamp,
        String originalLocator,
        String failureCategory,
        String pageKey,
        String context,
        HealingContext contextMetadata,
        List<HealingCandidate> candidates,
        HealingDecision decision,
        ProviderMetadata provider,
        PrivacyMetadata privacy,
        ActionMetadata action,
        LadderMetadata ladder,
        boolean visibilityRequired) {
    public static final String CURRENT_SCHEMA_VERSION = "2.0";

    /**
     * Creates a backward-compatible report without structured context metadata or re-suggestion
     * ladder metadata.
     */
    public HealingReport(
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
        this(schemaVersion, attemptId, timestamp, originalLocator, failureCategory, pageKey,
                context, new HealingContext(
                        HealingContext.CURRENT_SCHEMA_VERSION,
                        HealingPlatform.UNKNOWN,
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        ""),
                candidates, decision, provider, privacy, action);
    }

    /**
     * Creates a backward-compatible report without re-suggestion ladder metadata (issue #4027);
     * the ladder was not engaged, matching today's one-shot behavior.
     */
    public HealingReport(
            String schemaVersion,
            String attemptId,
            String timestamp,
            String originalLocator,
            String failureCategory,
            String pageKey,
            String context,
            HealingContext contextMetadata,
            List<HealingCandidate> candidates,
            HealingDecision decision,
            ProviderMetadata provider,
            PrivacyMetadata privacy,
            ActionMetadata action) {
        this(schemaVersion, attemptId, timestamp, originalLocator, failureCategory, pageKey,
                context, contextMetadata, candidates, decision, provider, privacy, action,
                LadderMetadata.disabled());
    }

    /**
     * Creates a backward-compatible report without a persisted visibility-required flag
     * (issue #4215); defaults to {@code true}, preserving the previous safe-direction assumption
     * that a candidate must be interactable.
     */
    public HealingReport(
            String schemaVersion,
            String attemptId,
            String timestamp,
            String originalLocator,
            String failureCategory,
            String pageKey,
            String context,
            HealingContext contextMetadata,
            List<HealingCandidate> candidates,
            HealingDecision decision,
            ProviderMetadata provider,
            PrivacyMetadata privacy,
            ActionMetadata action,
            LadderMetadata ladder) {
        this(schemaVersion, attemptId, timestamp, originalLocator, failureCategory, pageKey,
                context, contextMetadata, candidates, decision, provider, privacy, action, ladder,
                true);
    }

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
        contextMetadata = contextMetadata == null
                ? new HealingContext(
                        HealingContext.CURRENT_SCHEMA_VERSION,
                        HealingPlatform.UNKNOWN,
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "")
                : contextMetadata;
        candidates = candidates == null ? List.of() : List.copyOf(candidates);
        decision = Objects.requireNonNull(decision, "decision");
        provider = provider == null ? ProviderMetadata.disabled() : provider;
        privacy = privacy == null ? PrivacyMetadata.minimized() : privacy;
        action = action == null ? ActionMetadata.pending("") : action;
        ladder = ladder == null ? LadderMetadata.disabled() : ladder;
    }

    /**
     * Optional provider metadata.
     *
     * @param enabled whether reranking was requested
     * @param provider provider identifier
     * @param model model or configuration identifier
     * @param status provider status
     * @param fallbackReason deterministic fallback reason
     * @param processingLocation explicitly configured processing location
     * @param endpointClass local, on-prem, remote, or none
     * @param redactionSummary safe redaction summary
     */
    public record ProviderMetadata(
            boolean enabled,
            String provider,
            String model,
            String status,
            String fallbackReason,
            String processingLocation,
            String endpointClass,
            String redactionSummary) {
        /**
         * Creates backward-compatible provider metadata.
         */
        public ProviderMetadata(
                boolean enabled,
                String provider,
                String model,
                String status,
                String fallbackReason) {
            this(enabled, provider, model, status, fallbackReason, "NONE", "none", "");
        }

        /**
         * Creates safe provider metadata.
         */
        public ProviderMetadata {
            provider = Objects.requireNonNullElse(provider, "none");
            model = Objects.requireNonNullElse(model, "");
            status = Objects.requireNonNullElse(status, "DISABLED");
            fallbackReason = Objects.requireNonNullElse(fallbackReason, "");
            processingLocation = Objects.requireNonNullElse(processingLocation, "NONE");
            endpointClass = Objects.requireNonNullElse(endpointClass, "none");
            redactionSummary = Objects.requireNonNullElse(redactionSummary, "");
        }

        /**
         * @return disabled provider metadata
         */
        public static ProviderMetadata disabled() {
            return new ProviderMetadata(false, "none", "", "DISABLED", "",
                    "NONE", "none", "");
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

    /**
     * Re-suggestion ladder metadata (issue #4027): how many deterministic rungs ran and how much
     * total elapsed time they consumed against the configured hard budget, so the 60-second budget
     * is observable in the report instead of only enforced silently.
     *
     * @param rungsAttempted how many deterministic re-suggestion attempts ran (always at least 1)
     * @param elapsedMillis total elapsed time across every rung, in milliseconds
     * @param budgetSeconds the configured hard total budget in seconds; {@code 0} means the ladder
     *                       was not engaged (today's one-shot behavior)
     */
    public record LadderMetadata(int rungsAttempted, long elapsedMillis, long budgetSeconds) {
        /**
         * Creates safe ladder metadata.
         */
        public LadderMetadata {
            rungsAttempted = Math.max(1, rungsAttempted);
            elapsedMillis = Math.max(0, elapsedMillis);
            budgetSeconds = Math.max(0, budgetSeconds);
        }

        /**
         * @return metadata for a single-attempt resolution with the ladder not engaged
         */
        public static LadderMetadata disabled() {
            return new LadderMetadata(1, 0, 0);
        }
    }
}
