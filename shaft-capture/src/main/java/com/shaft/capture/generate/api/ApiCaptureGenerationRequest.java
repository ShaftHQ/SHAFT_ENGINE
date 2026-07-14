package com.shaft.capture.generate.api;

import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.pilot.ai.ApprovalPolicy;

import java.nio.file.Path;
import java.util.List;

/**
 * Options for generating a {@code SHAFT.API} test class from a recorded session.
 *
 * @param sessionPath path to the recorded {@code CaptureSession} JSON file
 * @param outputDirectory project root the generated source/test-data/report are written under
 * @param packageName generated class package
 * @param className generated class name; blank derives one deterministically from the session ID
 * @param style how transactions are grouped into test methods
 * @param validationDepth how thoroughly each response is validated
 * @param compile whether to compile the generated source
 * @param replay whether to replay the compiled class (ignored unless {@code compile} is also true;
 *               off by default at the caller level since replaying non-idempotent HTTP methods
 *               against a live service is unsafe to do automatically)
 * @param overwrite whether existing output files may be overwritten
 * @param openApiSpecPath optional path to an OpenAPI (JSON or YAML) spec to cross-report recorded
 *                        endpoints against; {@code null} skips coverage reporting entirely
 * @param enrichmentMode optional AI naming-enrichment phase for the {@link ApiCodegenStyle#HYBRID_UI_API}
 *                       scenario method/class name; {@code NONE} (the default) never calls AI, and
 *                       generation always produces a valid deterministic name without it
 * @param enrichmentPreviewPath preview path to write ({@code PREVIEW}) or read ({@code APPLY})
 * @param enrichmentApproved whether a reviewed preview may be applied; required when
 *                           {@code enrichmentMode} is {@code APPLY}
 * @param aiApprovalPolicy explicit evidence and processing-location approval for preview generation;
 *                        {@link ApprovalPolicy#denyAll()} by default, so AI naming never runs
 *                        unless the caller explicitly approves it
 * @param excludedTransactionIds recorded transaction IDs (see {@code CaptureEvent.NetworkEvent#transactionId()})
 *                               to omit from the generated test entirely, driven by a caller-side
 *                               transaction include/exclude selection; empty includes every renderable
 *                               transaction (the default)
 */
public record ApiCaptureGenerationRequest(
        Path sessionPath,
        Path outputDirectory,
        String packageName,
        String className,
        ApiCodegenStyle style,
        ApiValidationDepth validationDepth,
        boolean compile,
        boolean replay,
        boolean overwrite,
        Path openApiSpecPath,
        CaptureGenerationRequest.EnrichmentMode enrichmentMode,
        Path enrichmentPreviewPath,
        boolean enrichmentApproved,
        ApprovalPolicy aiApprovalPolicy,
        List<String> excludedTransactionIds) {
    public ApiCaptureGenerationRequest {
        packageName = packageName == null || packageName.isBlank() ? "tests.generated" : packageName;
        className = className == null ? "" : className.trim();
        style = style == null ? ApiCodegenStyle.SCENARIO : style;
        validationDepth = validationDepth == null ? ApiValidationDepth.SCHEMA : validationDepth;
        enrichmentMode = enrichmentMode == null ? CaptureGenerationRequest.EnrichmentMode.NONE : enrichmentMode;
        enrichmentPreviewPath = enrichmentPreviewPath == null
                ? outputDirectory.resolve("target/shaft-capture/api-enrichment-preview.json")
                : enrichmentPreviewPath;
        aiApprovalPolicy = aiApprovalPolicy == null ? ApprovalPolicy.denyAll() : aiApprovalPolicy;
        excludedTransactionIds = excludedTransactionIds == null ? List.of() : List.copyOf(excludedTransactionIds);
        if (enrichmentMode == CaptureGenerationRequest.EnrichmentMode.APPLY && !enrichmentApproved) {
            throw new IllegalArgumentException("Applying AI enrichment requires explicit approval.");
        }
    }

    /**
     * Compatibility constructor for callers compiled before the transaction include/exclude filter
     * was added.
     *
     * @param sessionPath path to the recorded {@code CaptureSession} JSON file
     * @param outputDirectory project root the generated source/test-data/report are written under
     * @param packageName generated class package
     * @param className generated class name; blank derives one deterministically from the session ID
     * @param style how transactions are grouped into test methods
     * @param validationDepth how thoroughly each response is validated
     * @param compile whether to compile the generated source
     * @param replay whether to replay the compiled class
     * @param overwrite whether existing output files may be overwritten
     * @param openApiSpecPath optional path to an OpenAPI spec to cross-report recorded endpoints against
     * @param enrichmentMode optional AI naming-enrichment phase
     * @param enrichmentPreviewPath preview path to write or read
     * @param enrichmentApproved whether a reviewed preview may be applied
     * @param aiApprovalPolicy explicit evidence and processing-location approval for preview generation
     */
    public ApiCaptureGenerationRequest(
            Path sessionPath,
            Path outputDirectory,
            String packageName,
            String className,
            ApiCodegenStyle style,
            ApiValidationDepth validationDepth,
            boolean compile,
            boolean replay,
            boolean overwrite,
            Path openApiSpecPath,
            CaptureGenerationRequest.EnrichmentMode enrichmentMode,
            Path enrichmentPreviewPath,
            boolean enrichmentApproved,
            ApprovalPolicy aiApprovalPolicy) {
        this(sessionPath, outputDirectory, packageName, className, style, validationDepth, compile, replay,
                overwrite, openApiSpecPath, enrichmentMode, enrichmentPreviewPath, enrichmentApproved,
                aiApprovalPolicy, List.of());
    }

    /**
     * Compatibility constructor for callers compiled before AI naming enrichment was added.
     *
     * @param sessionPath path to the recorded {@code CaptureSession} JSON file
     * @param outputDirectory project root the generated source/test-data/report are written under
     * @param packageName generated class package
     * @param className generated class name; blank derives one deterministically from the session ID
     * @param style how transactions are grouped into test methods
     * @param validationDepth how thoroughly each response is validated
     * @param compile whether to compile the generated source
     * @param replay whether to replay the compiled class
     * @param overwrite whether existing output files may be overwritten
     * @param openApiSpecPath optional path to an OpenAPI spec to cross-report recorded endpoints against
     */
    public ApiCaptureGenerationRequest(
            Path sessionPath,
            Path outputDirectory,
            String packageName,
            String className,
            ApiCodegenStyle style,
            ApiValidationDepth validationDepth,
            boolean compile,
            boolean replay,
            boolean overwrite,
            Path openApiSpecPath) {
        this(sessionPath, outputDirectory, packageName, className, style, validationDepth, compile, replay,
                overwrite, openApiSpecPath, CaptureGenerationRequest.EnrichmentMode.NONE, null, false, null);
    }

    /**
     * Compatibility constructor for callers compiled before OpenAPI coverage reporting was added.
     *
     * @param sessionPath path to the recorded {@code CaptureSession} JSON file
     * @param outputDirectory project root the generated source/test-data/report are written under
     * @param packageName generated class package
     * @param className generated class name; blank derives one deterministically from the session ID
     * @param style how transactions are grouped into test methods
     * @param validationDepth how thoroughly each response is validated
     * @param compile whether to compile the generated source
     * @param replay whether to replay the compiled class
     * @param overwrite whether existing output files may be overwritten
     */
    public ApiCaptureGenerationRequest(
            Path sessionPath,
            Path outputDirectory,
            String packageName,
            String className,
            ApiCodegenStyle style,
            ApiValidationDepth validationDepth,
            boolean compile,
            boolean replay,
            boolean overwrite) {
        this(sessionPath, outputDirectory, packageName, className, style, validationDepth, compile, replay,
                overwrite, null);
    }
}
