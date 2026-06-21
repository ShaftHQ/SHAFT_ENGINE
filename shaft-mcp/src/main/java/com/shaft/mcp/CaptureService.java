package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.capture.generate.CodegenFeatureCatalog;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStartOptions;
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
import java.util.List;

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
            description = "starts a privacy-safe SHAFT managed-browser recording with live browser controls")
    public CaptureStatus start(
            String targetUrl,
            String browser,
            String outputPath,
            boolean headless) {
        CaptureCodegenStartRequest request = new CaptureCodegenStartRequest();
        request.targetUrl = targetUrl;
        request.browser = browser;
        request.outputPath = outputPath;
        request.headless = headless;
        return startWithOptions(request);
    }

    /**
     * Launches a fresh SHAFT-managed browser with Playwright-codegen-shaped options.
     *
     * @param request structured codegen recording request
     * @return safe recorder status
     */
    @Tool(name = "capture_start_codegen",
            description = "starts SHAFT Capture with Playwright-codegen-compatible options and live browser controls")
    public CaptureStatus startWithOptions(CaptureCodegenStartRequest request) {
        CaptureCodegenStartRequest options = request == null ? new CaptureCodegenStartRequest() : request;
        Path output = options.outputPath == null || options.outputPath.isBlank()
                ? workspacePolicy.output("recordings/capture-" + FILE_TIME.format(Instant.now()) + ".json",
                "Capture output path")
                : workspacePolicy.output(options.outputPath, "Capture output path");
        return manager.start(new CaptureStartRequest(
                options.targetUrl,
                options.browser == null || options.browser.isBlank()
                        ? CaptureBrowser.CHROME
                        : CaptureBrowser.parse(options.browser),
                output,
                RUNTIME_DIRECTORY,
                options.headless,
                new CaptureStartOptions(
                        options.targetLanguage,
                        options.testIdAttribute,
                        options.channel,
                        options.deviceName,
                        options.viewportSize,
                        options.colorScheme,
                        options.geolocation,
                        options.ignoreHttpsErrors,
                        options.blockServiceWorkers,
                        workspacePath(options.loadStoragePath, "Capture load storage path"),
                        workspacePath(options.saveStoragePath, "Capture save storage path"),
                        options.language,
                        options.timezone,
                        options.proxyServer,
                        options.proxyBypass,
                        workspacePath(options.saveHarPath, "Capture HAR output path"),
                        options.saveHarGlob,
                        options.timeoutMillis > 0 ? Duration.ofMillis(options.timeoutMillis) : Duration.ZERO,
                        options.userAgent,
                        options.userDataDirectory == null || options.userDataDirectory.isBlank()
                                ? null
                                : workspacePolicy.output(options.userDataDirectory, "Capture user data directory"))));
    }

    /**
     * Structured request fields for the Playwright-codegen-compatible MCP start tool.
     */
    public static final class CaptureCodegenStartRequest {
        public String targetUrl;
        public String browser;
        public String outputPath;
        public boolean headless;
        public String targetLanguage;
        public String testIdAttribute;
        public String channel;
        public String deviceName;
        public String viewportSize;
        public String colorScheme;
        public String geolocation;
        public boolean ignoreHttpsErrors;
        public boolean blockServiceWorkers;
        public String loadStoragePath;
        public String saveStoragePath;
        public String language;
        public String timezone;
        public String proxyServer;
        public String proxyBypass;
        public String saveHarPath;
        public String saveHarGlob;
        public long timeoutMillis;
        public String userAgent;
        public String userDataDirectory;
    }

    /**
     * Returns the Playwright codegen feature inventory and SHAFT support mapping.
     *
     * @return codegen feature mapping
     */
    @Tool(name = "capture_codegen_features",
            description = "returns Playwright codegen features and how SHAFT Capture/MCP maps each feature")
    public List<CodegenFeatureCatalog.Feature> codegenFeatures() {
        return CodegenFeatureCatalog.features();
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
            description = "stops SHAFT Capture; after COMPLETED, call capture_code_blocks and ask snippet or insertion")
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
                allowRemoteAi,
                CodegenBackend.WEBDRIVER);
        return replayResult(result, driverVariableName);
    }

    /**
     * Generates, compiles, and optionally replays a SHAFT Playwright TestNG test from a Capture session.
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
    @Tool(name = "playwright_capture_generate_replay",
            description = "generates, compiles, optionally replays, and returns copy-paste SHAFT Playwright code blocks")
    @SuppressWarnings("PMD.ExcessiveParameterList")
    public McpCaptureReplayResult generatePlaywrightReplay(
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
                allowRemoteAi,
                CodegenBackend.PLAYWRIGHT);
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
            description = "generates a Java full-class snippet plus agent guidance for repo-aware insertion")
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
                false,
                CodegenBackend.WEBDRIVER);
        return replayResult(result, driverVariableName);
    }

    /**
     * Generates deterministic copy-paste SHAFT Playwright code blocks from a persisted Capture session.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing artifacts may be replaced
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generated snippets and report
     */
    @Tool(name = "playwright_capture_code_blocks",
            description = "generates a Java full-class snippet plus agent guidance for SHAFT Playwright insertion")
    public McpCaptureReplayResult playwrightCodeBlocks(
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
                false,
                CodegenBackend.PLAYWRIGHT);
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
                allowRemoteAi,
                CodegenBackend.WEBDRIVER);
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
            boolean allowRemoteAi,
            CodegenBackend backend) {
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
                        allowLocalAi || aiPreview || applyEnrichment,
                        allowRemoteAi || aiPreview || applyEnrichment,
                        EnumSet.of(EvidenceCategory.TEXT))),
                backend);
    }

    private String workspacePath(String value, String label) {
        return value == null || value.isBlank()
                ? ""
                : workspacePolicy.output(value, label).toString();
    }

    private McpCaptureReplayResult replayResult(CaptureGenerationResult result, String driverVariableName) {
        var blocks = result.successful() && result.sourcePath() != null
                ? codeBlocks.fromGeneratedSource(result.sourcePath(), driverVariableName)
                : java.util.List.<McpCodeBlock>of();
        return new McpCaptureReplayResult(
                result.sourcePath(),
                result.testDataPath(),
                result.reportPath(),
                result.reviewPath(),
                result.successful(),
                blocks,
                result.report(),
                result.report() == null ? java.util.List.of() : result.report().warnings());
    }
}
