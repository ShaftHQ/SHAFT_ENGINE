package com.shaft.capture.generate;

import com.shaft.capture.model.CaptureReadiness;

import java.util.List;

/**
 * Deterministic generation, locator-selection, compilation, and replay report.
 *
 * @param schemaVersion report schema version
 * @param sessionId source Capture session
 * @param status generation status
 * @param sourcePath output-root-relative source path
 * @param testDataPath output-root-relative test-data path
 * @param readiness live capture readiness state
 * @param readinessWarnings live capture readiness warnings
 * @param locatorDecisions selected locator rationale
 * @param unsupportedEvents unsupported event diagnostics
 * @param flakySteps replay-risk diagnostics
 * @param fallbackLocators available fallback locator diagnostics
 * @param controlFlowSuggestions deterministic control-flow suggestions
 * @param requiredUserInputs unresolved external inputs
 * @param warnings safe warnings
 * @param compilation compilation result
 * @param replay replay result
 * @param enrichment enrichment result
 * @param openApiCoverage OpenAPI coverage cross-report, or {@code null} when no spec was provided
 */
public record CaptureGenerationReport(
        String schemaVersion,
        String sessionId,
        Status status,
        String sourcePath,
        String testDataPath,
        CaptureReadiness.State readiness,
        List<String> readinessWarnings,
        List<LocatorDecision> locatorDecisions,
        List<String> unsupportedEvents,
        List<String> flakySteps,
        List<String> fallbackLocators,
        List<ControlFlowSuggestion> controlFlowSuggestions,
        List<String> requiredUserInputs,
        List<String> warnings,
        Validation compilation,
        Validation replay,
        Enrichment enrichment,
        OpenApiCoverage openApiCoverage) {
    public static final String CURRENT_SCHEMA_VERSION = "1.0";

    /**
     * Deterministic OpenAPI coverage cross-report (see {@code OpenApiCoverageReporter} in
     * {@code com.shaft.capture.generate.api.internal}, which this record mirrors so the report can
     * live in this module without an API-codegen-specific dependency).
     *
     * @param loadable whether the provided spec could be read and parsed
     * @param loadFailureReason safe reason {@code loadable} is {@code false}; empty otherwise
     * @param coveredOperations {@code METHOD normalized-path} operations recorded AND declared
     * @param missingOperations {@code METHOD normalized-path} operations declared but never recorded
     * @param undeclaredOperations {@code METHOD normalized-path} operations recorded but not declared
     * @param totalDeclaredOperations total number of operations declared by the spec
     * @param coverageRatio {@code coveredOperations.size() / totalDeclaredOperations}
     */
    public record OpenApiCoverage(
            boolean loadable,
            String loadFailureReason,
            List<String> coveredOperations,
            List<String> missingOperations,
            List<String> undeclaredOperations,
            int totalDeclaredOperations,
            double coverageRatio) {
        public OpenApiCoverage {
            loadFailureReason = loadFailureReason == null ? "" : loadFailureReason;
            coveredOperations = coveredOperations == null ? List.of() : List.copyOf(coveredOperations);
            missingOperations = missingOperations == null ? List.of() : List.copyOf(missingOperations);
            undeclaredOperations = undeclaredOperations == null ? List.of() : List.copyOf(undeclaredOperations);
        }

        /**
         * Returns the default state when no OpenAPI spec was provided.
         *
         * @return not-requested coverage
         */
        public static OpenApiCoverage notRequested() {
            return new OpenApiCoverage(false, "", List.of(), List.of(), List.of(), 0, 0.0);
        }
    }

    /**
     * Generation status.
     */
    public enum Status {
        SUCCESS,
        FAILED
    }

    /**
     * Deterministic control-flow suggestion kind.
     */
    public enum ControlFlowKind {
        REPEATED_GROUP,
        OPTIONAL_GUARD,
        RECOVERY_REVIEW
    }

    /**
     * One deterministic control-flow suggestion.
     *
     * @param id stable suggestion identifier
     * @param kind suggestion kind
     * @param evidenceIds related capture events or checkpoints
     * @param summary safe suggestion summary
     * @param recommendation deterministic remediation
     * @param applied whether generation applied this suggestion
     */
    public record ControlFlowSuggestion(
            String id,
            ControlFlowKind kind,
            List<String> evidenceIds,
            String summary,
            String recommendation,
            boolean applied) {
        /**
         * Creates an immutable suggestion.
         */
        public ControlFlowSuggestion {
            id = text(id);
            kind = kind == null ? ControlFlowKind.RECOVERY_REVIEW : kind;
            evidenceIds = evidenceIds == null ? List.of() : List.copyOf(evidenceIds);
            summary = text(summary);
            recommendation = text(recommendation);
        }

        private static String text(String value) {
            return value == null ? "" : value.trim();
        }
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
        readiness = readiness == null ? CaptureReadiness.State.READY : readiness;
        readinessWarnings = readinessWarnings == null ? List.of() : List.copyOf(readinessWarnings);
        locatorDecisions = locatorDecisions == null ? List.of() : List.copyOf(locatorDecisions);
        unsupportedEvents = unsupportedEvents == null ? List.of() : List.copyOf(unsupportedEvents);
        flakySteps = flakySteps == null ? List.of() : List.copyOf(flakySteps);
        fallbackLocators = fallbackLocators == null ? List.of() : List.copyOf(fallbackLocators);
        controlFlowSuggestions = controlFlowSuggestions == null ? List.of() : List.copyOf(controlFlowSuggestions);
        requiredUserInputs = requiredUserInputs == null ? List.of() : List.copyOf(requiredUserInputs);
        warnings = warnings == null ? List.of() : List.copyOf(warnings);
        compilation = compilation == null ? Validation.skipped("Compilation was not requested.") : compilation;
        replay = replay == null ? Validation.skipped("Replay was not requested.") : replay;
        enrichment = enrichment == null ? Enrichment.notRequested() : enrichment;
        openApiCoverage = openApiCoverage == null ? OpenApiCoverage.notRequested() : openApiCoverage;
    }

    /**
     * Compatibility constructor for callers compiled before OpenAPI coverage was added.
     *
     * @param schemaVersion report schema version
     * @param sessionId source Capture session
     * @param status generation status
     * @param sourcePath output-root-relative source path
     * @param testDataPath output-root-relative test-data path
     * @param readiness live capture readiness state
     * @param readinessWarnings live capture readiness warnings
     * @param locatorDecisions selected locator rationale
     * @param unsupportedEvents unsupported event diagnostics
     * @param flakySteps replay-risk diagnostics
     * @param fallbackLocators available fallback locator diagnostics
     * @param controlFlowSuggestions deterministic control-flow suggestions
     * @param requiredUserInputs unresolved external inputs
     * @param warnings safe warnings
     * @param compilation compilation result
     * @param replay replay result
     * @param enrichment enrichment result
     */
    public CaptureGenerationReport(
            String schemaVersion,
            String sessionId,
            Status status,
            String sourcePath,
            String testDataPath,
            CaptureReadiness.State readiness,
            List<String> readinessWarnings,
            List<LocatorDecision> locatorDecisions,
            List<String> unsupportedEvents,
            List<String> flakySteps,
            List<String> fallbackLocators,
            List<ControlFlowSuggestion> controlFlowSuggestions,
            List<String> requiredUserInputs,
            List<String> warnings,
            Validation compilation,
            Validation replay,
            Enrichment enrichment) {
        this(schemaVersion, sessionId, status, sourcePath, testDataPath, readiness, readinessWarnings,
                locatorDecisions, unsupportedEvents, flakySteps, fallbackLocators, controlFlowSuggestions,
                requiredUserInputs, warnings, compilation, replay, enrichment, null);
    }

    /**
     * Compatibility constructor for callers compiled before control-flow suggestions were added.
     *
     * @param schemaVersion report schema version
     * @param sessionId source Capture session
     * @param status generation status
     * @param sourcePath output-root-relative source path
     * @param testDataPath output-root-relative test-data path
     * @param readiness live capture readiness state
     * @param readinessWarnings live capture readiness warnings
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
    public CaptureGenerationReport(
            String schemaVersion,
            String sessionId,
            Status status,
            String sourcePath,
            String testDataPath,
            CaptureReadiness.State readiness,
            List<String> readinessWarnings,
            List<LocatorDecision> locatorDecisions,
            List<String> unsupportedEvents,
            List<String> flakySteps,
            List<String> fallbackLocators,
            List<String> requiredUserInputs,
            List<String> warnings,
            Validation compilation,
            Validation replay,
            Enrichment enrichment) {
        this(schemaVersion, sessionId, status, sourcePath, testDataPath, readiness, readinessWarnings,
                locatorDecisions, unsupportedEvents, flakySteps, fallbackLocators, List.of(),
                requiredUserInputs, warnings, compilation, replay, enrichment);
    }

    /**
     * Compatibility constructor for callers compiled before readiness was added.
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
    public CaptureGenerationReport(
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
        this(schemaVersion, sessionId, status, sourcePath, testDataPath, CaptureReadiness.State.READY,
                List.of(), locatorDecisions, unsupportedEvents, flakySteps, fallbackLocators,
                List.of(), requiredUserInputs, warnings, compilation, replay, enrichment);
    }
}
