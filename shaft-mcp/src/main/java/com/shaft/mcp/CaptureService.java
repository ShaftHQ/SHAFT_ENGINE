package com.shaft.mcp;

import com.shaft.capture.generate.CaptureGenerationReport;
import com.shaft.capture.generate.CaptureGenerationRequest;
import com.shaft.capture.generate.CaptureGenerationResult;
import com.shaft.capture.generate.CaptureGenerator;
import com.shaft.capture.generate.CaptureGenerator.CodegenBackend;
import com.shaft.capture.generate.CodegenFeatureCatalog;
import com.shaft.capture.generate.api.ApiCaptureGenerationRequest;
import com.shaft.capture.generate.api.ApiCaptureGenerationResult;
import com.shaft.capture.generate.api.ApiCaptureGenerator;
import com.shaft.capture.generate.api.ApiCodegenStyle;
import com.shaft.capture.generate.api.ApiValidationDepth;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.runtime.CaptureBrowser;
import com.shaft.capture.runtime.CaptureManager;
import com.shaft.capture.runtime.CaptureStartRequest;
import com.shaft.capture.runtime.CaptureStartOptions;
import com.shaft.capture.runtime.CaptureStatus;
import com.shaft.capture.runtime.NetworkCaptureOptions;
import com.shaft.capture.runtime.NetworkTransaction;
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
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;

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
                                : workspacePolicy.output(options.userDataDirectory, "Capture user data directory"),
                        options.sessionGoal,
                        options.apiCapture,
                        options.networkOptions)));
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
        public String sessionGoal;
        public boolean apiCapture;
        public NetworkCaptureOptions networkOptions;
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
            description = "stops SHAFT Capture; after COMPLETED, show outputPath and tell IntelliJ users to run /codegen <outputPath>")
    public CaptureStatus stop(boolean discard) {
        return manager.stop(discard);
    }

    /**
     * Launches a fresh SHAFT-managed browser and starts API capture with network recording.
     *
     * @param targetUrl initial http, https, or file URL
     * @param browser Chrome or Edge; blank selects Chrome
     * @param headless whether to launch without a visible window; unspecified defaults to
     *                  headless per repo policy
     * @param networkOptions network capture filtering options; blank selects capture-all defaults
     * @return safe recorder status with network transaction count
     */
    @Tool(name = "capture_api_start",
            description = "starts SHAFT Capture with API network recording enabled")
    public CaptureStatus apiStart(
            String targetUrl,
            String browser,
            Boolean headless,
            NetworkCaptureOptions networkOptions) {
        CaptureCodegenStartRequest request = new CaptureCodegenStartRequest();
        request.targetUrl = targetUrl;
        request.browser = browser;
        request.headless = resolveHeadless(headless);
        request.apiCapture = true;
        request.networkOptions = networkOptions == null ? new NetworkCaptureOptions() : networkOptions;
        return startWithOptions(request);
    }

    /**
     * Resolves the effective headless flag, defaulting to headless execution per repo policy
     * when the caller does not specify a value.
     *
     * @param headless caller-supplied headless preference, or {@code null} when unspecified
     * @return {@code true} unless the caller explicitly requested a visible window
     */
    static boolean resolveHeadless(Boolean headless) {
        return headless == null || headless;
    }

    /**
     * Returns safe recorder status with network transaction count and recent endpoints.
     *
     * @return current recorder status including network metrics
     */
    @Tool(name = "capture_api_status",
            description = "returns SHAFT Capture session status including network transaction count and recent endpoints")
    public CaptureStatus apiStatus() {
        return manager.status();
    }

    /**
     * Stops the active API capture recording.
     *
     * @param discard whether to delete capture artifacts after shutdown
     * @return final recorder status
     */
    @Tool(name = "capture_api_stop",
            description = "stops SHAFT API Capture with the same single-session lock guarantee as capture_stop")
    public CaptureStatus apiStop(boolean discard) {
        return manager.stop(discard);
    }

    /**
     * Returns sanitized summaries of captured network transactions.
     *
     * @param includeAssets whether to include asset resource types (images, fonts, stylesheets, media)
     * @param excludePattern optional {@code |}-separated glob pattern(s) of URLs to exclude
     * @return ordered, bounded list of transaction summaries without sensitive data
     */
    @Tool(name = "capture_api_transactions",
            description = "returns captured network transactions without bodies or sensitive headers; supports filtering asset noise")
    public List<NetworkTransaction> apiTransactions(
            boolean includeAssets,
            String excludePattern) {
        NetworkCaptureOptions filter = new NetworkCaptureOptions();
        filter.excludeAssets = !includeAssets;
        filter.excludePattern = excludePattern == null ? "" : excludePattern;
        return manager.networkTransactions(filter, 100);
    }

    /**
     * Generates, compiles, and returns copy-paste {@code SHAFT.API} code blocks from a recorded
     * session's API transactions.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace; blank selects
     *                        {@code generated-tests}
     * @param packageName generated Java package
     * @param className optional generated class name; blank derives one from the session ID
     * @param style {@code SCENARIO} (default; chains correlated values through variables),
     *              {@code PER_REQUEST} (one independent test per transaction), or
     *              {@code HYBRID_UI_API} (interleaves API assertions after their correlated UI
     *              anchor; deterministic even with no AI enrichment)
     * @param validationDepth {@code STATUS}, {@code STATUS_HEADERS}, {@code SCHEMA} (default), or
     *                        {@code FULL_BODY}
     * @param overwrite whether existing artifacts may be replaced
     * @param replay whether to execute the generated test after compiling (off by default; unsafe
     *               to enable automatically for non-idempotent methods)
     * @param openApiSpecPath optional path (inside the MCP workspace) to an OpenAPI JSON/YAML spec
     *                        to cross-report recorded endpoints against; blank skips coverage
     * @return generated artifacts, compile/replay result, and copy-paste code blocks
     */
    @Tool(name = "capture_api_generate",
            description = "generates, compiles, and returns copy-paste SHAFT.API code blocks from recorded API transactions")
    public McpCaptureReplayResult generateApi(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            String style,
            String validationDepth,
            boolean overwrite,
            boolean replay,
            String openApiSpecPath) {
        Path output = outputDirectory == null || outputDirectory.isBlank()
                ? workspacePolicy.output("generated-tests", "Capture generation output directory")
                : workspacePolicy.output(outputDirectory, "Capture generation output directory");
        Path openApiSpec = openApiSpecPath == null || openApiSpecPath.isBlank()
                ? null
                : workspacePolicy.existing(openApiSpecPath, "OpenAPI spec path");
        ApiCaptureGenerationResult result = new ApiCaptureGenerator().generate(new ApiCaptureGenerationRequest(
                workspacePolicy.existing(sessionPath, "Capture session path"),
                output,
                packageName,
                className,
                parseStyle(style),
                parseValidationDepth(validationDepth),
                true,
                replay,
                overwrite,
                openApiSpec));
        boolean successful = result.report().status() == CaptureGenerationReport.Status.SUCCESS;
        List<McpCodeBlock> blocks = successful && result.sourcePath() != null
                ? codeBlocks.fromGeneratedSource(result.sourcePath(), "api", result.report())
                : List.of();
        return new McpCaptureReplayResult(
                result.sourcePath(),
                result.testDataDirectory(),
                result.reportPath(),
                null,
                successful,
                blocks,
                result.report(),
                result.report() == null ? List.of() : result.report().warnings());
    }

    private static ApiCodegenStyle parseStyle(String value) {
        if (value == null || value.isBlank()) {
            return ApiCodegenStyle.SCENARIO;
        }
        try {
            return ApiCodegenStyle.valueOf(value.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException invalid) {
            return ApiCodegenStyle.SCENARIO;
        }
    }

    private static ApiValidationDepth parseValidationDepth(String value) {
        if (value == null || value.isBlank()) {
            return ApiValidationDepth.SCHEMA;
        }
        try {
            return ApiValidationDepth.valueOf(value.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException invalid) {
            return ApiValidationDepth.SCHEMA;
        }
    }

    /**
     * Adds a human-review checkpoint to the active recording.
     *
     * @param description checkpoint description
     * @param kind checkpoint kind; blank selects USER_MARKER
     * @return safe recorder status
     */
    @Tool(name = "capture_checkpoint",
            description = "records a USER_MARKER, ASSERTION, PAGE_TRANSITION, RECOVERY, "
                    + "FLOW_START, or FLOW_END checkpoint")
    public CaptureStatus checkpoint(String description, String kind) {
        Checkpoint.CheckpointKind checkpointKind = kind == null || kind.isBlank()
                ? Checkpoint.CheckpointKind.USER_MARKER
                : Checkpoint.CheckpointKind.valueOf(kind.trim().toUpperCase(java.util.Locale.ROOT));
        return manager.checkpoint(description, checkpointKind);
    }

    /**
     * Reads or toggles the active recorder's live authoring mode.
     *
     * @param mode blank reads the current mode without changing it; {@code "record"} or
     *             {@code "inspect"} (case-insensitive) sets it
     * @return the mode now in effect
     */
    @Tool(name = "capture_set_mode",
            description = "reads or toggles the active recorder's live authoring mode: record (default) or inspect")
    public String setMode(String mode) {
        return mode == null || mode.isBlank() ? manager.mode() : manager.setMode(mode);
    }

    /**
     * Ranks caller-supplied locator candidates for one picked element (best-first) and renders a
     * copy-paste {@code SHAFT.GUI.Locator...} Java snippet for the winner. Mirrors
     * {@code CaptureControlServer}'s {@code /locator/pick} endpoint (used by a detached recorder
     * UI); this MCP tool serves the same in-process, without a loopback HTTP round trip.
     *
     * @param candidates locator candidates observed for the picked element, e.g. from the recorder
     *                   overlay's inspect-mode click handler
     * @return the winning candidate's snippet plus every candidate ranked best-first
     */
    @Tool(name = "capture_pick_locator",
            description = "ranks caller-supplied locator candidates for a picked element and returns a copy-paste SHAFT.GUI.Locator snippet")
    public McpPickLocatorResult pickLocator(List<McpLocatorCandidate> candidates) {
        List<com.shaft.capture.model.LocatorCandidate> parsed = parseCandidates(candidates);
        if (parsed.isEmpty()) {
            return new McpPickLocatorResult("", List.of());
        }
        List<com.shaft.capture.model.LocatorCandidate> ranked =
                parsed.stream().sorted(com.shaft.capture.model.LocatorCandidate.BEST_FIRST).toList();
        List<McpRankedLocatorCandidate> rankedResult = ranked.stream()
                .map(candidate -> new McpRankedLocatorCandidate(
                        candidate.strategy().name(), candidate.expression(), candidate.score(),
                        com.shaft.capture.control.PickedLocatorSnippetBuilder.snippet(candidate)))
                .toList();
        return new McpPickLocatorResult(rankedResult.getFirst().snippet(), rankedResult);
    }

    private static List<com.shaft.capture.model.LocatorCandidate> parseCandidates(List<McpLocatorCandidate> candidates) {
        if (candidates == null) {
            return List.of();
        }
        List<com.shaft.capture.model.LocatorCandidate> parsed = new ArrayList<>();
        for (McpLocatorCandidate candidate : candidates) {
            com.shaft.capture.model.LocatorCandidate.LocatorStrategy strategy;
            try {
                strategy = com.shaft.capture.model.LocatorCandidate.LocatorStrategy.valueOf(
                        (candidate.strategy() == null ? "" : candidate.strategy()).trim().toUpperCase(Locale.ROOT));
            } catch (IllegalArgumentException invalidStrategy) {
                continue;
            }
            parsed.add(new com.shaft.capture.model.LocatorCandidate(
                    strategy, candidate.expression(), candidate.uniquenessCount(),
                    candidate.visible(), candidate.stable(), java.util.Set.of()));
        }
        return parsed;
    }

    /**
     * One locator candidate observed for a picked element.
     *
     * @param strategy locator strategy name (ROLE, ACCESSIBLE_NAME, LABEL, TEST_ID, ID, NAME, CSS, XPATH)
     * @param expression raw locator expression
     * @param uniquenessCount number of matching elements observed on the live page
     * @param visible whether the target was visible
     * @param stable whether the evidence appeared stable
     */
    public record McpLocatorCandidate(
            String strategy, String expression, int uniquenessCount, boolean visible, boolean stable) {
    }

    /**
     * One ranked locator candidate.
     *
     * @param strategy locator strategy name
     * @param expression raw locator expression
     * @param score deterministic score
     * @param snippet copy-paste Java locator expression
     */
    public record McpRankedLocatorCandidate(String strategy, String expression, int score, String snippet) {
    }

    /**
     * Pick-locator result.
     *
     * @param snippet winning candidate's copy-paste Java locator expression, blank when no valid
     *                candidate was supplied
     * @param ranked every valid supplied candidate, ranked best-first
     */
    public record McpPickLocatorResult(String snippet, List<McpRankedLocatorCandidate> ranked) {
        /**
         * Creates an immutable pick-locator result.
         */
        public McpPickLocatorResult {
            snippet = snippet == null ? "" : snippet;
            ranked = ranked == null ? List.of() : List.copyOf(ranked);
        }
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
     * Generates focused record-at-target code blocks for an existing Java source anchor.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing artifacts may be replaced
     * @param targetSourcePath existing Java source path inside the MCP workspace
     * @param insertAfter method name or textual anchor to insert after
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generated snippets and report
     */
    @Tool(name = "capture_record_at_target_code_blocks",
            description = "generates focused Capture snippets for insertion at an existing Java source anchor")
    public McpCaptureReplayResult recordAtTargetCodeBlocks(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            String targetSourcePath,
            String insertAfter,
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
        return replayResult(
                result,
                driverVariableName,
                workspacePolicy.existing(targetSourcePath, "Capture target source path"),
                insertAfter);
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
     * Suggests existing Java classes and anchors for record-at-target insertion.
     *
     * @param repositoryPath workspace-contained repository or source root
     * @param maxResults maximum candidates to return
     * @return ranked Java target candidates
     */
    @Tool(name = "capture_target_candidates",
            description = "suggests existing Java Page Object or test targets for Capture record-at-target insertion")
    public List<McpJavaTargetScanner.Candidate> targetCandidates(String repositoryPath, int maxResults) {
        return new McpJavaTargetScanner().scan(
                workspacePolicy.existing(repositoryPath, "Capture target candidate root"),
                maxResults);
    }

    /**
     * Compares generated WebDriver and Playwright code blocks for the same Capture session.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing artifacts may be replaced
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return backend comparison result
     */
    @Tool(name = "capture_backend_comparison",
            description = "compares WebDriver and Playwright Capture code-block outputs without editing source")
    public CaptureBackendComparisonResult compareCodeBlocks(
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            String driverVariableName) {
        Path baseOutput = outputDirectory == null || outputDirectory.isBlank()
                ? workspacePolicy.output("generated-comparison", "Capture backend comparison output directory")
                : workspacePolicy.output(outputDirectory, "Capture backend comparison output directory");
        String baseClass = className == null || className.isBlank() ? "ComparedCaptureTest" : className;
        CaptureGenerationResult webdriver = generateInternal(
                sessionPath,
                baseOutput.resolve("webdriver").toString(),
                packageName,
                baseClass,
                overwrite,
                false,
                false,
                false,
                false,
                false,
                false,
                CodegenBackend.WEBDRIVER);
        CaptureGenerationResult playwright = generateInternal(
                sessionPath,
                baseOutput.resolve("playwright").toString(),
                packageName,
                "Playwright" + baseClass,
                overwrite,
                false,
                false,
                false,
                false,
                false,
                false,
                CodegenBackend.PLAYWRIGHT);
        return new CaptureBackendComparisonResult(
                "1.0",
                List.of(
                        backendBlocks("WEBDRIVER", webdriver, driverVariableName),
                        backendBlocks("PLAYWRIGHT", playwright, driverVariableName)),
                comparisonWarnings(webdriver, playwright));
    }

    /**
     * Builds a local manifest of Capture codegen evidence for PR review.
     *
     * @param sourcePath generated source path inside the MCP workspace
     * @param reportPath generation report path inside the MCP workspace
     * @param reviewPath review UI or workbench path inside the MCP workspace
     * @param screenshotPaths optional screenshot paths inside the MCP workspace
     * @return evidence manifest
     */
    @Tool(name = "capture_evidence_pack",
            description = "returns a local manifest of Capture source, report, review UI, screenshots, and checks")
    public McpEvidencePack evidencePack(
            String sourcePath,
            String reportPath,
            String reviewPath,
            List<String> screenshotPaths) {
        return McpEvidencePack.of(
                workspacePolicy.existing(sourcePath, "Capture evidence source path"),
                workspacePolicy.existing(reportPath, "Capture evidence report path"),
                workspacePolicy.existing(reviewPath, "Capture evidence review path"),
                workspacePolicy.existingList(screenshotPaths, "Capture evidence screenshot path"));
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
        return replayResult(result, driverVariableName, null, "");
    }

    private McpCaptureReplayResult replayResult(
            CaptureGenerationResult result,
            String driverVariableName,
            Path targetSource,
            String insertAfter) {
        var blocks = generatedCodeUsable(result)
                ? codeBlocks.fromGeneratedSource(
                        result.sourcePath(), driverVariableName, result.report(), targetSource, insertAfter)
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

    /**
     * Whether the generated code itself is worth returning to the caller: generation produced a
     * source file and it compiled. A replay failure marks the overall result unsuccessful but
     * must not swallow the generated, compiling code blocks — the failure report tells the user
     * what to fix, and returning nothing turned every replay hiccup into a silent "no code"
     * dead end (issue #3409). Generation-level failures (for example the privacy gate) keep
     * suppressing code blocks because compilation never passed for them.
     */
    private static boolean generatedCodeUsable(CaptureGenerationResult result) {
        return result.sourcePath() != null
                && (result.successful()
                || (result.report() != null
                && result.report().compilation().status()
                == CaptureGenerationReport.Validation.ValidationStatus.PASSED));
    }

    private CaptureBackendBlocks backendBlocks(
            String backend,
            CaptureGenerationResult result,
            String driverVariableName) {
        List<McpCodeBlock> blocks = generatedCodeUsable(result)
                ? codeBlocks.fromGeneratedSource(result.sourcePath(), driverVariableName, result.report())
                : List.of();
        return new CaptureBackendBlocks(
                backend,
                result.sourcePath(),
                result.successful(),
                blocks.stream().map(McpCodeBlock::id).toList(),
                result.report() == null ? List.of() : result.report().warnings());
    }

    private static List<String> comparisonWarnings(
            CaptureGenerationResult webdriver,
            CaptureGenerationResult playwright) {
        List<String> warnings = new ArrayList<>();
        if (!webdriver.successful()) {
            warnings.add("WebDriver Capture generation did not complete successfully.");
        }
        if (!playwright.successful()) {
            warnings.add("Playwright Capture generation did not complete successfully.");
        }
        return List.copyOf(warnings);
    }

    /**
     * Backend comparison result.
     *
     * @param schemaVersion result schema version
     * @param backends backend block summaries
     * @param warnings safe warnings
     */
    public record CaptureBackendComparisonResult(
            String schemaVersion,
            List<CaptureBackendBlocks> backends,
            List<String> warnings) {
        /**
         * Creates an immutable comparison result.
         */
        public CaptureBackendComparisonResult {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank() ? "1.0" : schemaVersion;
            backends = backends == null ? List.of() : List.copyOf(backends);
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }

    /**
     * Code-block summary for one generated backend.
     *
     * @param backend backend name
     * @param sourcePath generated source path
     * @param successful whether generation succeeded
     * @param blockIds returned code-block identifiers
     * @param warnings safe warnings
     */
    public record CaptureBackendBlocks(
            String backend,
            Path sourcePath,
            boolean successful,
            List<String> blockIds,
            List<String> warnings) {
        /**
         * Creates an immutable backend summary.
         */
        public CaptureBackendBlocks {
            backend = backend == null ? "" : backend.trim();
            blockIds = blockIds == null ? List.of() : List.copyOf(blockIds);
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }
    }
}
