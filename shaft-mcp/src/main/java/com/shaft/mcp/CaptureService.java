package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStatus;
import com.shaft.pilot.ai.ApprovalPolicy;
import com.shaft.pilot.ai.EvidenceCategory;
import jakarta.annotation.PreDestroy;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.EnumSet;

/**
 * MCP adapter for deterministic managed-browser SHAFT Capture recording.
 */
@Service
public class CaptureService {
    private static final DateTimeFormatter FILE_TIME = DateTimeFormatter
            .ofPattern("yyyyMMdd-HHmmss")
            .withZone(ZoneOffset.UTC);
    private static final Path RUNTIME_DIRECTORY = Path.of("target", "shaft-capture-mcp");

    private final CaptureManager manager = new CaptureManager();

    /**
     * Launches a fresh SHAFT-managed browser and starts deterministic capture.
     *
     * @param targetUrl initial http, https, or file URL
     * @param browser Chrome or Edge; blank selects Chrome
     * @param outputPath capture JSON path; blank selects a timestamped recording
     * @param headless whether to launch without a visible window
     * @return safe recorder status
     */
    @Tool(name = "capture_start",
            description = "starts a privacy-safe SHAFT managed-browser recording with no AI provider")
    public CaptureStatus start(
            String targetUrl,
            String browser,
            String outputPath,
            boolean headless) {
        Path output = outputPath == null || outputPath.isBlank()
                ? Path.of("recordings", "capture-" + FILE_TIME.format(Instant.now()) + ".json")
                : Path.of(outputPath);
        return manager.start(new CaptureStartRequest(
                targetUrl,
                browser == null || browser.isBlank()
                        ? CaptureBrowser.CHROME
                        : CaptureBrowser.parse(browser),
                output,
                RUNTIME_DIRECTORY,
                headless));
    }

    /**
     * Returns safe recorder status without captured values.
     *
     * @return current or final recorder status
     */
    @Tool(name = "capture_status",
            description = "returns SHAFT Capture session, browser, URL, event count, warnings, and output status")
    public CaptureStatus status() {
        return manager.status();
    }

    /**
     * Stops the active recording.
     *
     * @param discard whether to delete capture artifacts after shutdown
     * @return final recorder status
     */
    @Tool(name = "capture_stop",
            description = "stops SHAFT Capture and optionally discards the local recording")
    public CaptureStatus stop(boolean discard) {
        return manager.stop(discard);
    }

    /**
     * Generates a deterministic SHAFT TestNG test from a persisted Capture session.
     *
     * @param sessionPath persisted Capture JSON path
     * @param outputDirectory generated project root
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing artifacts may be replaced
     * @param replay whether to execute the compiled generated test
     * @param aiPreview whether to request a review-only AI proposal
     * @param enrichmentPreviewPath preview path to write or apply
     * @param applyEnrichment whether to apply the reviewed preview
     * @param approveEnrichment explicit approval to apply the preview
     * @param allowLocalAi explicit approval for local inference
     * @param allowRemoteAi explicit approval for remote inference
     * @return generated artifacts and validation report
     */
    @Tool(name = "capture_generate",
            description = "generates, compiles, and optionally replays a deterministic SHAFT TestNG test")
    public CaptureGenerationResult generate(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            boolean replay,
            boolean aiPreview,
            String enrichmentPreviewPath,
            boolean applyEnrichment,
            boolean approveEnrichment,
            boolean allowLocalAi,
            boolean allowRemoteAi) {
        Path output = outputDirectory == null || outputDirectory.isBlank()
                ? Path.of("generated-tests")
                : Path.of(outputDirectory);
        Path preview = enrichmentPreviewPath == null || enrichmentPreviewPath.isBlank()
                ? output.resolve("target/shaft-capture/enrichment-preview.json")
                : Path.of(enrichmentPreviewPath);
        CaptureGenerationRequest.EnrichmentMode mode = applyEnrichment
                ? CaptureGenerationRequest.EnrichmentMode.APPLY
                : aiPreview
                ? CaptureGenerationRequest.EnrichmentMode.PREVIEW
                : CaptureGenerationRequest.EnrichmentMode.NONE;
        return new CaptureGenerator().generate(new CaptureGenerationRequest(
                Path.of(sessionPath),
                output,
                packageName,
                className,
                overwrite,
                true,
                replay,
                Duration.ofMinutes(5),
                mode,
                preview,
                approveEnrichment,
                new ApprovalPolicy(
                        allowLocalAi,
                        allowRemoteAi,
                        EnumSet.of(EvidenceCategory.TEXT))));
    }

    /**
     * Preserves an incomplete session when the MCP server shuts down unexpectedly.
     */
    @PreDestroy
    void close() {
        manager.close();
    }
}
