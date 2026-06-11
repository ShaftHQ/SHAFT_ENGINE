package com.shaft.capture.generate;

import java.util.List;

/**
 * Deterministic generation, locator-selection, compilation, and replay report.
 *
 * @param schemaVersion report schema version
 * @param sessionId source Capture session
 * @param status generation status
 * @param sourcePath output-root-relative source path
 * @param testDataPath output-root-relative test-data path
 * @param locatorDecisions selected locator rationale
 * @param unsupportedEvents unsupported event diagnostics
 * @param flakySteps replay-risk diagnostics
 * @param fallbackLocators available fallback locator diagnostics
 * @param requiredUserInputs unresolved external inputs
 * @param warnings safe warnings
 * @param compilation compilation result
 * @param replay replay result
 * @param enrichment enrichment result
 */
public record CaptureGenerationReport(
        String schemaVersion,
        String sessionId,
        Status status,
        String sourcePath,
        String testDataPath,
        List<LocatorDecision> locatorDecisions,
        List<String> unsupportedEvents,
        List<String> flakySteps,
        List<String> fallbackLocators,
        List<String> requiredUserInputs,
        List<String> warnings,
        Validation compilation,
        Validation replay,
        Enrichment enrichment) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Generation status.
     */
    public enum Status {
        SUCCESS,
        FAILED
    }

    /**
     * One explainable locator choice.
     *
     * @param eventIds related event IDs
     * @param logicalElementId logical target
     * @param strategy selected strategy
     * @param expression sanitized expression
     * @param score deterministic score
     * @param scoreBreakdown ordered score explanation
     * @param alternatives ranked alternatives
     */
    public record LocatorDecision(
            List<String> eventIds,
            String logicalElementId,
            String strategy,
            String expression,
            int score,
            List<String> scoreBreakdown,
            List<String> alternatives) {
    }

    /**
     * Compile or replay result.
     *
     * @param status validation status
     * @param diagnostics safe diagnostics
     * @param allureResultCount populated Allure result count
     */
    public record Validation(
            ValidationStatus status,
            List<String> diagnostics,
            int allureResultCount) {
        /**
         * Validation status.
         */
        public enum ValidationStatus {
            PASSED,
            FAILED,
            SKIPPED
        }

        /**
         * Returns a skipped validation.
         *
         * @param reason skip reason
         * @return skipped validation
         */
        public static Validation skipped(String reason) {
            return new Validation(ValidationStatus.SKIPPED, List.of(reason), 0);
        }
    }

    /**
     * Optional AI enrichment state.
     *
     * @param status enrichment phase
     * @param previewPath output-root-relative preview path
     * @param diff reviewed or proposed changes
     * @param provider provider identifier
     */
    public record Enrichment(
            EnrichmentStatus status,
            String previewPath,
            List<String> diff,
            String provider) {
        /**
         * Enrichment status.
         */
        public enum EnrichmentStatus {
            NOT_REQUESTED,
            PREVIEWED,
            APPLIED,
            REJECTED
        }

        /**
         * Returns the default deterministic state.
         *
         * @return no-enrichment state
         */
        public static Enrichment notRequested() {
            return new Enrichment(EnrichmentStatus.NOT_REQUESTED, "", List.of(), "none");
        }
    }

    /**
     * Creates an immutable report.
     */
    public CaptureGenerationReport {
        schemaVersion = schemaVersion == null ? CURRENT_SCHEMA_VERSION : schemaVersion;
        sessionId = sessionId == null ? "" : sessionId;
        status = status == null ? Status.FAILED : status;
        sourcePath = sourcePath == null ? "" : sourcePath;
        testDataPath = testDataPath == null ? "" : testDataPath;
        locatorDecisions = locatorDecisions == null ? List.of() : List.copyOf(locatorDecisions);
        unsupportedEvents = unsupportedEvents == null ? List.of() : List.copyOf(unsupportedEvents);
        flakySteps = flakySteps == null ? List.of() : List.copyOf(flakySteps);
        fallbackLocators = fallbackLocators == null ? List.of() : List.copyOf(fallbackLocators);
        requiredUserInputs = requiredUserInputs == null ? List.of() : List.copyOf(requiredUserInputs);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        compilation = compilation == null ? Validation.skipped("Compilation was not requested.") : compilation;
        replay = replay == null ? Validation.skipped("Replay was not requested.") : replay;
        enrichment = enrichment == null ? Enrichment.notRequested() : enrichment;
    }
}
