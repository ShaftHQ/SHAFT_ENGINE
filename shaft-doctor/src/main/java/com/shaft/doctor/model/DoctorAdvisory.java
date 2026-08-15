package com.shaft.doctor.model;

import com.shaft.pilot.ai.AiResponseStatus;
import com.shaft.pilot.ai.AiUsage;

import java.util.List;

/**
 * Separately identified optional provider advisory that never replaces the deterministic diagnosis.
 *
 * @param schemaVersion Doctor advisory envelope schema version
 * @param status advisory outcome
 * @param analysis schema-validated provider analysis
 * @param metadata safe provider and fallback metadata
 * @param confidence SHAFT-computed confidence score 0-100
 * @param confidenceRationale human-readable rationale for the confidence score
 */
public record DoctorAdvisory(
        String schemaVersion,
        Status status,
        ProviderAnalysis analysis,
        Metadata metadata,
        int confidence,
        String confidenceRationale) {
    /**
     * Current advisory envelope schema version.
     */
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Advisory outcome.
     */
    public enum Status {
        SUCCESS,
        FALLBACK,
        DISABLED
    }

    /**
     * Versioned provider output accepted by Doctor.
     *
     * @param schemaVersion provider analysis schema version
     * @param observations evidence-cited observations
     * @param hypotheses advisory hypotheses
     * @param missingEvidence evidence gaps identified by the provider
     * @param recommendedActions advisory actions
     * @param limitations limitations stated by the provider
     */
    public record ProviderAnalysis(
            String schemaVersion,
            List<Observation> observations,
            List<Hypothesis> hypotheses,
            List<String> missingEvidence,
            List<RecommendedAction> recommendedActions,
            List<String> limitations) {
        /**
         * Current provider analysis schema version.
         */
        public static final String CURRENT_SCHEMA_VERSION = "2.0";

        /**
         * Creates immutable provider analysis.
         */
        public ProviderAnalysis {
            schemaVersion = DoctorModelSupport.requireText(schemaVersion, "Advisory analysis schema version");
            observations = DoctorModelSupport.list(observations);
            hypotheses = DoctorModelSupport.list(hypotheses);
            missingEvidence = DoctorModelSupport.list(missingEvidence);
            recommendedActions = DoctorModelSupport.list(recommendedActions);
            limitations = DoctorModelSupport.list(limitations);
        }

        /**
         * Returns an empty provider analysis.
         *
         * @return empty analysis
         */
        public static ProviderAnalysis empty() {
            return new ProviderAnalysis(CURRENT_SCHEMA_VERSION, List.of(), List.of(),
                    List.of(), List.of(), List.of());
        }
    }

    /**
     * One provider observation.
     *
     * @param statement safe observation text
     * @param evidenceIds submitted evidence references
     * @param cited whether at least one submitted evidence item supports the claim
     */
    public record Observation(String statement, List<String> evidenceIds, boolean cited) {
        /**
         * Creates an immutable observation.
         */
        public Observation {
            statement = DoctorModelSupport.requireText(statement, "Advisory observation");
            evidenceIds = DoctorModelSupport.list(evidenceIds);
        }
    }

    /**
     * One provider hypothesis.
     *
     * @param causeCategory proposed cause category
     * @param statement safe hypothesis text
     * @param confidence provider confidence
     * @param evidenceIds submitted evidence references
     * @param cited whether at least one submitted evidence item supports the claim
     * @param contradictsDeterministic whether the category conflicts with the deterministic diagnosis
     */
    public record Hypothesis(
            CauseCategory causeCategory,
            String statement,
            Confidence confidence,
            List<String> evidenceIds,
            boolean cited,
            boolean contradictsDeterministic) {
        /**
         * Creates an immutable hypothesis.
         */
        public Hypothesis {
            causeCategory = causeCategory == null ? CauseCategory.UNKNOWN : causeCategory;
            statement = DoctorModelSupport.requireText(statement, "Advisory hypothesis");
            confidence = confidence == null ? Confidence.UNKNOWN : confidence;
            evidenceIds = DoctorModelSupport.list(evidenceIds);
        }
    }

    /**
     * One provider-recommended action.
     *
     * @param title concise action title
     * @param action safe action text
     * @param evidenceIds submitted evidence references
     * @param cited whether at least one submitted evidence item supports the action
     */
    public record RecommendedAction(
            String title,
            String action,
            List<String> evidenceIds,
            boolean cited,
            ActionOperation operation,
            ActionTarget target) {
        /** Allowlisted advisory action targets. */
        public enum ActionTarget {
            DOM_SNAPSHOT,
            RUNTIME_CONFIGURATION,
            TEST_LOCATOR,
            WAIT_CONDITION,
            TEST_DATA,
            INFRASTRUCTURE_STATUS
        }

        /** Allowlisted operations with their only valid target and SHAFT-owned display text. */
        public enum ActionOperation {
            COLLECT_EVIDENCE(ActionTarget.DOM_SNAPSHOT, "Collect current DOM evidence",
                    "Capture a current DOM snapshot and rerun deterministic Doctor analysis."),
            REVIEW_CONFIGURATION(ActionTarget.RUNTIME_CONFIGURATION, "Review runtime configuration",
                    "Compare the effective runtime configuration with the reviewed project settings."),
            UPDATE_TEST_LOCATOR(ActionTarget.TEST_LOCATOR, "Review the test locator",
                    "Compare the typed test locator with the current application DOM."),
            ADD_EXPLICIT_WAIT(ActionTarget.WAIT_CONDITION, "Review the wait condition",
                    "Add or adjust an explicit condition only after confirming the observed timing boundary."),
            REVIEW_TEST_DATA(ActionTarget.TEST_DATA, "Review test data",
                    "Compare the failing test data with the deterministic evidence and product state."),
            RETRY_INFRASTRUCTURE_CHECK(ActionTarget.INFRASTRUCTURE_STATUS, "Recheck infrastructure",
                    "Recheck the reported infrastructure dependency before rerunning the test." );

            private final ActionTarget target;
            private final String title;
            private final String displayText;

            ActionOperation(ActionTarget target, String title, String displayText) {
                this.target = target;
                this.title = title;
                this.displayText = displayText;
            }
        }

        /**
         * Creates an immutable recommended action.
         */
        public RecommendedAction {
            title = DoctorModelSupport.requireText(title, "Advisory action title");
            action = DoctorModelSupport.requireText(action, "Advisory action");
            evidenceIds = DoctorModelSupport.list(evidenceIds);
            if ((operation == null) != (target == null)) {
                throw new IllegalArgumentException("Advisory operation and target must be supplied together.");
            }
            if (operation != null) {
                if (operation.target != target) {
                    throw new IllegalArgumentException("Advisory operation and target pair is not allowlisted.");
                }
                title = operation.title;
                action = operation.displayText;
            }
        }

        /**
         * Retains source compatibility for callers that construct display-only legacy actions.
         */
        public RecommendedAction(String title, String action, List<String> evidenceIds, boolean cited) {
            this(title, action, evidenceIds, cited, null, null);
        }

        /**
         * Creates an allowlisted action whose display text is owned by SHAFT.
         */
        public RecommendedAction(ActionOperation operation, ActionTarget target,
                                 List<String> evidenceIds, boolean cited) {
            this(operation == null ? "Invalid action" : operation.title,
                    operation == null ? "Invalid action" : operation.displayText,
                    evidenceIds, cited, operation, target);
        }

        /** Returns whether this action came from the structured allowlist. */
        public boolean structured() {
            return operation != null;
        }
    }

    /**
     * Safe provider, timing, usage, cache, and fallback metadata.
     *
     * @param providerStatus normalized provider outcome
     * @param provider provider identifier
     * @param model model identifier
     * @param configurationIdentifier non-secret configuration checksum
     * @param durationMillis elapsed provider duration
     * @param usage provider-reported usage
     * @param fallbackReason safe fallback reason
     * @param cacheHit whether the advisory came from the explicit safe cache
     * @param warnings safe validation warnings
     */
    public record Metadata(
            AiResponseStatus providerStatus,
            String provider,
            String model,
            String configurationIdentifier,
            long durationMillis,
            AiUsage usage,
            String fallbackReason,
            boolean cacheHit,
            List<String> warnings) {
        /**
         * Creates immutable safe metadata.
         */
        public Metadata {
            providerStatus = providerStatus == null ? AiResponseStatus.DISABLED : providerStatus;
            provider = DoctorModelSupport.text(provider);
            model = DoctorModelSupport.text(model);
            configurationIdentifier = DoctorModelSupport.text(configurationIdentifier);
            if (durationMillis < 0) {
                throw new IllegalArgumentException("Advisory duration cannot be negative.");
            }
            usage = usage == null ? AiUsage.empty() : usage;
            fallbackReason = DoctorModelSupport.text(fallbackReason);
            warnings = DoctorModelSupport.list(warnings);
        }

        /**
         * Returns a cache-hit copy.
         *
         * @return cache-hit metadata
         */
        public Metadata asCacheHit() {
            return new Metadata(providerStatus, provider, model, configurationIdentifier,
                    durationMillis, usage, fallbackReason, true, warnings);
        }
    }

    /**
     * Creates a validated advisory.
     */
    public DoctorAdvisory {
        schemaVersion = DoctorModelSupport.requireText(schemaVersion, "Advisory schema version");
        status = status == null ? Status.DISABLED : status;
        analysis = analysis == null ? ProviderAnalysis.empty() : analysis;
        metadata = metadata == null
                ? new Metadata(AiResponseStatus.DISABLED, "none", "", "", 0,
                AiUsage.empty(), "", false, List.of())
                : metadata;
        confidence = Math.max(0, Math.min(100, confidence));
        confidenceRationale = confidenceRationale == null ? "" : confidenceRationale;
    }

    /**
     * Returns a disabled advisory that is not rendered into deterministic reports.
     *
     * @return disabled advisory
     */
    public static DoctorAdvisory disabled() {
        return new DoctorAdvisory(CURRENT_SCHEMA_VERSION, Status.DISABLED,
                ProviderAnalysis.empty(),
                new Metadata(AiResponseStatus.DISABLED, "none", "", "", 0,
                        AiUsage.empty(), "", false, List.of()),
                0,
                "");
    }
}
