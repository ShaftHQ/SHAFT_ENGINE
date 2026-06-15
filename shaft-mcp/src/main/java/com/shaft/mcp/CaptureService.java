package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.model.Checkpoint;
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

    private final CaptureManager manager;
    private final McpWorkspacePolicy workspacePolicy;
    private final McpCaptureCodeBlockService codeBlocks;

    /**
     * Creates the default MCP Capture service.
     */
    public CaptureService() {
        this(new CaptureManager(), McpWorkspacePolicy.current(), new McpCaptureCodeBlockService());
    }

    CaptureService(
            CaptureManager manager,
            McpWorkspacePolicy workspacePolicy,
            McpCaptureCodeBlockService codeBlocks) {
        this.manager = manager;
        this.workspacePolicy = workspacePolicy;
        this.codeBlocks = codeBlocks;
    }

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
                ? workspacePolicy.output("recordings/capture-" + FILE_TIME.format(Instant.now()) + ".json",
                "Capture output path")
                : workspacePolicy.output(outputPath, "Capture output path");
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
     * Adds a human-review checkpoint to the active recording.
     *
     * @param description checkpoint description
     * @param kind checkpoint kind; blank selects USER_MARKER
     * @return safe recorder status
     */
    @Tool(name = "capture_checkpoint",
            description = "records a USER_MARKER, ASSERTION, PAGE_TRANSITION, or RECOVERY checkpoint")
    public CaptureStatus checkpoint(String description, String kind) {
        Checkpoint.CheckpointKind checkpointKind = kind == null || kind.isBlank()
                ? Checkpoint.CheckpointKind.USER_MARKER
                : Checkpoint.CheckpointKind.valueOf(kind.trim().toUpperCase(java.util.Locale.ROOT));
        return manager.checkpoint(description, checkpointKind);
    }

    /**
     * Generates, compiles, and optionally replays a deterministic SHAFT TestNG test from a Capture session.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing artifacts may be replaced
     * @param replay whether to execute the generated test
     * @param useAi whether to request optional AI enrichment preview
     * @param allowLocalAi explicit approval for local inference
     * @param allowRemoteAi explicit approval for remote inference
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generation report plus copy-paste code blocks
     */
    @Tool(name = "capture_generate_replay",
            description = "generates, compiles, optionally replays, and returns copy-paste SHAFT code blocks")
    public McpCaptureReplayResult generateReplay(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            boolean replay,
            boolean useAi,
            boolean allowLocalAi,
            boolean allowRemoteAi,
            String driverVariableName) {
        CaptureGenerationResult result = generateInternal(
                sessionPath,
                outputDirectory,
                packageName,
                className,
                overwrite,
                replay,
                useAi,
                false,
                false,
                allowLocalAi,
                allowRemoteAi);
        return replayResult(result, driverVariableName);
    }

    /**
     * Generates deterministic copy-paste code blocks from a persisted Capture session without replaying.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing artifacts may be replaced
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generated snippets and report
     */
    @Tool(name = "capture_code_blocks",
            description = "generates reusable copy-paste SHAFT code blocks from a Capture session")
    public McpCaptureReplayResult codeBlocks(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            String driverVariableName) {
        CaptureGenerationResult result = generateInternal(
                sessionPath,
                outputDirectory,
                packageName,
                className,
                overwrite,
                false,
                false,
                false,
                false,
                false,
                false);
        return replayResult(result, driverVariableName);
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
        return generateInternal(
                sessionPath,
                outputDirectory,
                packageName,
                className,
                overwrite,
                replay,
                aiPreview,
                applyEnrichment,
                approveEnrichment,
                allowLocalAi,
                allowRemoteAi);
    }

    /**
     * Preserves an incomplete session when the MCP server shuts down unexpectedly.
     */
    @PreDestroy
    void close() {
        manager.close();
    }

    private CaptureGenerationResult generateInternal(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            boolean replay,
            boolean aiPreview,
            boolean applyEnrichment,
            boolean approveEnrichment,
            boolean allowLocalAi,
            boolean allowRemoteAi) {
        Path output = outputDirectory == null || outputDirectory.isBlank()
                ? workspacePolicy.output("generated-tests", "Capture generation output directory")
                : workspacePolicy.output(outputDirectory, "Capture generation output directory");
        Path preview = output.resolve("target/shaft-capture/enrichment-preview.json");
        CaptureGenerationRequest.EnrichmentMode mode = applyEnrichment
                ? CaptureGenerationRequest.EnrichmentMode.APPLY
                : aiPreview
                ? CaptureGenerationRequest.EnrichmentMode.PREVIEW
                : CaptureGenerationRequest.EnrichmentMode.NONE;
        return new CaptureGenerator().generate(new CaptureGenerationRequest(
                workspacePolicy.existing(sessionPath, "Capture session path"),
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

    private McpCaptureReplayResult replayResult(CaptureGenerationResult result, String driverVariableName) {
        var blocks = result.successful() && result.sourcePath() != null
                ? codeBlocks.fromGeneratedSource(result.sourcePath(), driverVariableName)
                : java.util.List.<McpCodeBlock>of();
        return new McpCaptureReplayResult(
                result.sourcePath(),
                result.testDataPath(),
                result.reportPath(),
                result.successful(),
                blocks,
                result.report(),
                result.report() == null ? java.util.List.of() : result.report().warnings());
    }
}
