package com.shaft.capture.generate;

import com.shaft.pilot.ai.ApprovalPolicy;

import java.nio.file.Path;
import java.time.Duration;

/**
 * Immutable options for converting a persisted Capture session into a SHAFT TestNG test.
 *
 * @param sessionPath persisted Capture session
 * @param outputDirectory generated project root
 * @param packageName generated Java package
 * @param className optional generated class name
 * @param overwrite whether existing generated artifacts may be replaced
 * @param compile whether to compile the generated source
 * @param replay whether to execute the compiled TestNG test
 * @param replayTimeout maximum replay duration
 * @param enrichmentMode optional AI enrichment phase
 * @param enrichmentPreviewPath preview path to write or apply
 * @param enrichmentApproved whether a reviewed preview may be applied
 * @param aiApprovalPolicy explicit evidence and processing-location approval for preview generation
 */
public record CaptureGenerationRequest(
        Path sessionPath,
        Path outputDirectory,
        String packageName,
        String className,
        boolean overwrite,
        boolean compile,
        boolean replay,
        Duration replayTimeout,
        EnrichmentMode enrichmentMode,
        Path enrichmentPreviewPath,
        boolean enrichmentApproved,
        ApprovalPolicy aiApprovalPolicy) {
    /**
     * AI enrichment lifecycle.
     */
    public enum EnrichmentMode {
        NONE,
        PREVIEW,
        APPLY
    }

    /**
     * Creates normalized generation options.
     */
    public CaptureGenerationRequest {
        if (sessionPath == null) {
            throw new IllegalArgumentException("Capture session path is required.");
        }
        outputDirectory = outputDirectory == null ? Path.of("generated-tests") : outputDirectory;
        packageName = packageName == null || packageName.isBlank()
                ? "generated.capture"
                : packageName.trim();
        className = className == null ? "" : className.trim();
        compile = compile || replay;
        replayTimeout = replayTimeout == null ? Duration.ofMinutes(5) : replayTimeout;
        if (replayTimeout.isNegative() || replayTimeout.isZero()) {
            throw new IllegalArgumentException("Replay timeout must be positive.");
        }
        enrichmentMode = enrichmentMode == null ? EnrichmentMode.NONE : enrichmentMode;
        enrichmentPreviewPath = enrichmentPreviewPath == null
                ? outputDirectory.resolve("target/shaft-capture/enrichment-preview.json")
                : enrichmentPreviewPath;
        aiApprovalPolicy = aiApprovalPolicy == null ? ApprovalPolicy.denyAll() : aiApprovalPolicy;
        if (enrichmentMode == EnrichmentMode.APPLY && !enrichmentApproved) {
            throw new IllegalArgumentException("Applying AI enrichment requires explicit approval.");
        }
    }

    /**
     * Creates deterministic defaults for one session.
     *
     * @param sessionPath persisted session
     * @return default request
     */
    public static CaptureGenerationRequest defaults(Path sessionPath) {
        return new CaptureGenerationRequest(
                sessionPath,
                Path.of("generated-tests"),
                "generated.capture",
                "",
                false,
                true,
                false,
                Duration.ofMinutes(5),
                EnrichmentMode.NONE,
                null,
                false,
                ApprovalPolicy.denyAll());
    }
}
