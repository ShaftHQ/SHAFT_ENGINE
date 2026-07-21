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
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpSchema;
import jakarta.annotation.PreDestroy;
import org.springframework.ai.mcp.annotation.McpProgressToken;
import org.springframework.ai.mcp.annotation.McpTool;
import org.springframework.ai.mcp.annotation.McpToolParam;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Locale;
import java.util.function.BiConsumer;
import java.util.stream.Stream;

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
    private final PlaywrightService playwrightService;
    private final MobileService mobileService;

    /**
     * Creates the default MCP Capture service, wiring default Playwright/mobile services for direct
     * Java callers and tests that construct this class without Spring.
     */
    public CaptureService() {
        this(new CaptureManager(), McpWorkspacePolicy.current(), new McpCaptureCodeBlockService(),
                new PlaywrightService(), new MobileService(new EngineService()));
        // Only a fully-default-wired instance registers: element_* tools fall back to driving the
        // active capture session's browser when no driver_initialize session exists.
        // Package-private test constructors below intentionally skip this so throwaway per-test
        // CaptureManager instances never leak into this shared static bridge.
        registerCaptureDriverBridge();
    }

    /**
     * Spring entry point: {@code capture_start}/{@code capture_status}/{@code capture_stop}/
     * {@code capture_step_delete}/{@code capture_step_reorder} dispatch to these engine services'
     * existing recorder tools when a Playwright or mobile session is active (design doc amendment A3).
     */
    @Autowired
    CaptureService(PlaywrightService playwrightService, MobileService mobileService) {
        this(new CaptureManager(), McpWorkspacePolicy.current(), new McpCaptureCodeBlockService(),
                playwrightService, mobileService);
        registerCaptureDriverBridge();
    }

    CaptureService(
            CaptureManager manager,
            McpWorkspacePolicy workspacePolicy,
            McpCaptureCodeBlockService codeBlocks) {
        this(manager, workspacePolicy, codeBlocks, new PlaywrightService(), new MobileService(new EngineService()));
    }

    CaptureService(
            CaptureManager manager,
            McpWorkspacePolicy workspacePolicy,
            McpCaptureCodeBlockService codeBlocks,
            PlaywrightService playwrightService,
            MobileService mobileService) {
        this.manager = manager;
        this.workspacePolicy = workspacePolicy;
        this.codeBlocks = codeBlocks;
        this.playwrightService = playwrightService;
        this.mobileService = mobileService;
    }

    private void registerCaptureDriverBridge() {
        EngineService.registerCaptureDriverBridge(() -> manager.activeShaftDriver().orElse(null));
    }

    /**
     * Starts deterministic capture, dispatching on the MCP session's active engine (design doc
     * amendment A3): a WEB session (or no active session) always launches a fresh SHAFT-managed
     * browser via its own CDP capture, regardless of what else is active; a PLAYWRIGHT or mobile
     * (MOBILE_NATIVE/MOBILE_WEB) session instead records actions performed on that already-active
     * driver, delegating to that engine's own recorder start (package-private
     * {@link PlaywrightService#recordStart}/{@link MobileService#recordStart}, no longer separate
     * tool names since commit 4).
     *
     * @param targetUrl initial http, https, or file URL; WEB/NONE only
     * @param browser Chrome or Edge; blank selects Chrome; WEB/NONE only
     * @param outputPath recording/capture JSON path; blank selects a timestamped recording
     * @param headless whether to launch without a visible window; unspecified defaults to headless
     *                  per repo policy, matching {@link #apiStart}; WEB/NONE only
     * @param sessionGoal WEB/NONE: optional user intent for the journey, drives generated test class
     *                     and method names; PLAYWRIGHT/mobile: used as the recorder's descriptive mode label
     * @return union recorder status; {@link McpCaptureUnionStatus#engine()} names the dispatched engine
     */
    @Tool(name = "capture_start",
            description = "starts a recording, dispatching to the active engine: WEB (or no active session) "
                    + "always launches its own privacy-safe SHAFT-managed browser via CDP capture, and while "
                    + "it is active, element_* tools drive the recorded browser directly (no driver_initialize "
                    + "needed); PLAYWRIGHT/mobile sessions instead record actions on the already-active driver "
                    + "(targetUrl/browser/headless are WEB-only; sessionGoal names the generated test on WEB, "
                    + "or is used as the recorder's mode label on PLAYWRIGHT/mobile); optional nested "
                    + "codegenOptions replaces the flat targetUrl/browser/outputPath/headless/sessionGoal args "
                    + "with the full Playwright-codegen-compatible request (viewport, geolocation, HAR, proxy, "
                    + "etc.), absorbing the former capture_start_codegen tool -- WEB/NONE only")
    public McpCaptureUnionStatus start(
            @ToolParam(required = false, description = "initial http, https, or file URL; blank opens an empty browser; WEB/NONE only")
            String targetUrl,
            @ToolParam(required = false, description = "Chrome or Edge; blank selects Chrome; WEB/NONE only")
            String browser,
            @ToolParam(required = false, description = "recording/capture JSON path; blank selects a timestamped recording")
            String outputPath,
            @ToolParam(required = false, description = "whether to launch without a visible window; blank/omitted defaults to headless per repo policy; WEB/NONE only")
            Boolean headless,
            @ToolParam(required = false, description = "optional user intent for the journey; drives generated test class and method names on WEB, or the recorder's mode label on PLAYWRIGHT/mobile")
            String sessionGoal,
            @ToolParam(required = false, description = "full Playwright-codegen-compatible request (viewport, "
                    + "geolocation, HAR, proxy, API capture, etc.); when supplied, its own fields replace "
                    + "targetUrl/browser/outputPath/headless/sessionGoal entirely; WEB/NONE only, absorbs "
                    + "capture_start_codegen")
            CaptureCodegenStartRequest codegenOptions) {
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> new McpCaptureUnionStatus(engine, null,
                    playwrightService.recordStart(outputPath, sessionGoal, false), null);
            case MOBILE_NATIVE, MOBILE_WEB -> new McpCaptureUnionStatus(engine, null, null,
                    mobileService.recordStart(outputPath, sessionGoal, false));
            case WEB, NONE -> {
                CaptureCodegenStartRequest request = codegenOptions != null ? codegenOptions : new CaptureCodegenStartRequest();
                if (codegenOptions == null) {
                    request.targetUrl = targetUrl;
                    request.browser = browser;
                    request.outputPath = outputPath;
                    request.headless = resolveHeadless(headless);
                    request.sessionGoal = sessionGoal;
                }
                yield new McpCaptureUnionStatus(ActiveEngine.WEB, startWithOptions(request), null, null);
            }
        };
    }

    /**
     * Java-caller convenience overload defaulting {@code codegenOptions} to unset; not an MCP tool.
     *
     * @param targetUrl initial http, https, or file URL
     * @param browser Chrome or Edge
     * @param outputPath recording/capture JSON path
     * @param headless whether to launch without a visible window
     * @param sessionGoal optional user intent for the journey
     * @return safe recorder status for the active engine
     */
    public McpCaptureUnionStatus start(String targetUrl, String browser, String outputPath, Boolean headless,
            String sessionGoal) {
        return start(targetUrl, browser, outputPath, headless, sessionGoal, null);
    }

    /**
     * Launches a fresh SHAFT-managed browser with Playwright-codegen-shaped options. Not an MCP tool
     * since commit 4 (design doc Decision 2): reached only through {@code capture_start}'s optional
     * {@code codegenOptions} nested request now (also still used internally by {@code capture_api_start}
     * on the WEB engine).
     *
     * @param request structured codegen recording request
     * @return safe recorder status
     */
    CaptureStatus startWithOptions(CaptureCodegenStartRequest request) {
        CaptureCodegenStartRequest options = request == null ? new CaptureCodegenStartRequest() : request;
        // Team recorder policy (issue #3425 C4): a checked-in .shaft/recorder-policy.json wins
        // over per-request settings so every recording in this workspace behaves consistently,
        // whichever client started it.
        com.shaft.capture.runtime.CaptureTeamPolicy teamPolicy =
                com.shaft.capture.runtime.CaptureTeamPolicy.load(workspacePolicy.root());
        if (teamPolicy.headless().isPresent()) {
            options.headless = teamPolicy.headless().get();
        }
        if (!teamPolicy.browser().isBlank() && (options.browser == null || options.browser.isBlank())) {
            options.browser = teamPolicy.browser();
        }
        String requestedOutput = teamPolicy.applyOutputDirectory(
                options.outputPath, "capture-" + FILE_TIME.format(Instant.now()) + ".json");
        Path output = requestedOutput.isBlank()
                ? workspacePolicy.output("recordings/capture-" + FILE_TIME.format(Instant.now()) + ".json",
                "Capture output path")
                : workspacePolicy.output(requestedOutput, "Capture output path");
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
                        options.networkOptions,
                        options.saveHarContent)));
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
        public String saveHarContent;
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
     * Returns safe recorder status, dispatching on the MCP session's active engine (design doc
     * amendment A3): the returned union's {@code engine} discriminator names which recording family
     * ({@code webStatus}, {@code playwrightStatus}, or {@code mobileStatus}) is populated.
     *
     * @return current or final recorder status for the active engine
     */
    @Tool(name = "capture_status",
            description = "returns recorder status for the active engine: WEB (or no active session) returns "
                    + "SHAFT Capture session, browser, URL, event count, pending debounced-signal count, "
                    + "warnings, and output status (when no session is active the warnings list persisted "
                    + "recordings that can be turned into code without a live session); PLAYWRIGHT/mobile "
                    + "return that engine's own recorder status; the union's engine field names which is populated")
    public McpCaptureUnionStatus status() {
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> new McpCaptureUnionStatus(engine, null, playwrightService.recordStatus(), null);
            case MOBILE_NATIVE, MOBILE_WEB ->
                    new McpCaptureUnionStatus(engine, null, null, mobileService.recordStatus());
            case WEB, NONE -> new McpCaptureUnionStatus(ActiveEngine.WEB, webStatus(), null, null);
        };
    }

    private CaptureStatus webStatus() {
        CaptureStatus status = manager.status();
        if (status.state() != CaptureStatus.State.NOT_RUNNING) {
            return status;
        }
        List<String> warnings = new ArrayList<>(status.warnings());
        warnings.add(noSessionGuidance());
        return new CaptureStatus(
                status.state(),
                status.sessionId(),
                status.browser(),
                status.currentUrl(),
                status.eventCount(),
                status.readiness(),
                warnings,
                status.outputPath(),
                status.aiEnabled(),
                status.processId(),
                status.startedAt(),
                status.networkTransactionCount(),
                status.lastEndpoints(),
                status.pendingSignalCount());
    }

    /**
     * A missing live session must never dead-end code generation: point agents at persisted
     * recordings (which the generate tools accept directly) or at starting a fresh session for a
     * scenario the user described in plain language.
     */
    private String noSessionGuidance() {
        List<String> recent = recentRecordings();
        StringBuilder guidance = new StringBuilder("No active capture session, and none is needed to generate code: "
                + "pass a persisted recording JSON path to capture_generate_replay (replay-proven locators) "
                + "or capture_code_blocks (faster, generate-only draft).");
        if (recent.isEmpty()) {
            guidance.append(" No recordings were found under ").append(workspacePolicy.root().resolve("recordings"))
                    .append(". If the user described a scenario instead of providing a recording, start a fresh "
                            + "session with capture_start, perform the described actions, call capture_stop, "
                            + "then generate code from the persisted outputPath.");
        } else {
            guidance.append(" Recent recordings in this workspace: ").append(String.join(", ", recent));
        }
        return guidance.toString();
    }

    private List<String> recentRecordings() {
        Path recordings = workspacePolicy.root().resolve("recordings");
        if (!Files.isDirectory(recordings)) {
            return List.of();
        }
        try (Stream<Path> files = Files.list(recordings)) {
            return files.filter(file -> file.getFileName().toString().endsWith(".json"))
                    .filter(Files::isRegularFile)
                    .sorted(Comparator.comparing(CaptureService::lastModified).reversed())
                    .limit(5)
                    .map(Path::toString)
                    .toList();
        } catch (IOException exception) {
            return List.of();
        }
    }

    private static FileTime lastModified(Path file) {
        try {
            return Files.getLastModifiedTime(file);
        } catch (IOException exception) {
            return FileTime.fromMillis(0);
        }
    }

    /**
     * Resolves the effective codegen session path: a blank value falls back to the most recently
     * modified recording under this workspace's {@code recordings/} directory, so the "record, then
     * generate" onboarding flow needs no explicit path (issue #3692 item 2). {@link #recentRecordings()}
     * already computes this same most-recent-first list for {@link #status()}'s idle guidance.
     *
     * @param sessionPath caller-supplied path, or blank/null to use the latest recording
     * @return effective session path; unchanged (blank) when no recordings exist, so
     *         {@link McpWorkspacePolicy#existing} still raises its normal "is required" error
     */
    private String resolveSessionPath(String sessionPath) {
        if (sessionPath != null && !sessionPath.isBlank()) {
            return sessionPath;
        }
        List<String> recent = recentRecordings();
        return recent.isEmpty() ? sessionPath : recent.getFirst();
    }

    /**
     * Stops the active recording, dispatching on the MCP session's active engine (design doc
     * amendment A3); see {@link #status()} for the union return shape.
     *
     * @param discard whether to delete capture artifacts after shutdown
     * @return final recorder status for the active engine
     */
    @Tool(name = "capture_stop",
            description = "stops the active recording for the active engine; after WEB reaches COMPLETED, show "
                    + "outputPath and generate replay-proven code by passing it as sessionPath to "
                    + "capture_generate_replay (or capture_code_blocks for a faster, unproven draft); the "
                    + "union's engine field names which recorder was stopped")
    public McpCaptureUnionStatus stop(boolean discard) {
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> new McpCaptureUnionStatus(engine, null, playwrightService.recordStop(discard), null);
            case MOBILE_NATIVE, MOBILE_WEB ->
                    new McpCaptureUnionStatus(engine, null, null, mobileService.recordStop(discard));
            case WEB, NONE -> new McpCaptureUnionStatus(ActiveEngine.WEB, manager.stop(discard), null, null);
        };
    }

    /**
     * Deletes a recorded step by its stable stepId from the active Playwright or mobile recording,
     * dispatching on the MCP session's active engine. A WEB CDP {@code capture_start} session has no
     * step editor for this (design doc amendment A3): {@link McpRecordingStepEditor} only understands
     * the mobile/Playwright JSON recording format, so this returns an actionable error naming the
     * active engine instead.
     *
     * @param stepId stable step id (e.g. "m2"), as surfaced in recorder status
     * @return updated recorder status for the active engine
     */
    @Tool(name = "capture_step_delete",
            description = "deletes a recorded step by its stable stepId from the active Playwright or mobile "
                    + "recording; returns an actionable error naming the active engine for a WEB CDP "
                    + "capture_start session, whose recording format has no step editor")
    public McpMobileRecordingStatus stepDelete(String stepId) {
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> playwrightService.stepDelete(stepId);
            case MOBILE_NATIVE, MOBILE_WEB -> mobileService.stepDelete(stepId);
            case WEB, NONE -> throw noStepEditorFor(engine, "capture_step_delete");
        };
    }

    /**
     * Moves a recorded step up or down by its stable stepId within the active Playwright or mobile
     * recording, dispatching on the MCP session's active engine; see {@link #stepDelete(String)} for
     * why a WEB CDP session returns an actionable error instead.
     *
     * @param stepId stable step id (e.g. "m2"), as surfaced in recorder status
     * @param direction "up" or "down"
     * @return updated recorder status for the active engine
     */
    @Tool(name = "capture_step_reorder",
            description = "moves a recorded step up or down by its stable stepId (direction: up|down) within "
                    + "the active Playwright or mobile recording; returns an actionable error naming the "
                    + "active engine for a WEB CDP capture_start session, whose recording format has no step editor")
    public McpMobileRecordingStatus stepReorder(String stepId, String direction) {
        ActiveEngine engine = EngineService.activeEngine();
        return switch (engine) {
            case PLAYWRIGHT -> playwrightService.stepReorder(stepId, direction);
            case MOBILE_NATIVE, MOBILE_WEB -> mobileService.stepReorder(stepId, direction);
            case WEB, NONE -> throw noStepEditorFor(engine, "capture_step_reorder");
        };
    }

    private static UnsupportedOperationException noStepEditorFor(ActiveEngine engine, String toolName) {
        return new UnsupportedOperationException(toolName + " is not supported for the active engine (" + engine
                + "): a WEB CDP capture_start recording has no step editor for this JSON format. Start a "
                + "capture_start recording with a Playwright or mobile engine active to edit steps.");
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
            description = "starts SHAFT Capture with API network recording enabled; dispatches to the mobile "
                    + "loopback MITM proxy when a mobile engine is active OR mobilePlatform is explicitly given "
                    + "(the mobile proxy never requires a live Appium/WebDriver session), absorbing "
                    + "mobile_api_record_start (outputPath/mobilePlatform/mobileDeviceLabel are mobile-only and "
                    + "ignored on WEB)")
    public McpCaptureApiUnionStatus apiStart(
            String targetUrl,
            String browser,
            Boolean headless,
            NetworkCaptureOptions networkOptions,
            @ToolParam(required = false) String outputPath,
            @ToolParam(required = false) String mobilePlatform,
            @ToolParam(required = false) String mobileDeviceLabel) {
        ActiveEngine engine = EngineService.activeEngine();
        // The mobile loopback MITM proxy is deliberately standalone (platform/deviceLabel are
        // stored for reference only, per mobileApiRecordStart's contract) -- an explicit
        // mobilePlatform is as strong a signal as an already-active mobile engine, so a caller
        // never needs to driver_initialize a mobile session just to record API traffic.
        if (engine == ActiveEngine.MOBILE_NATIVE || engine == ActiveEngine.MOBILE_WEB
                || (mobilePlatform != null && !mobilePlatform.isBlank())) {
            return new McpCaptureApiUnionStatus(engine, null,
                    mobileService.mobileApiRecordStart(mobilePlatform, mobileDeviceLabel, outputPath));
        }
        CaptureCodegenStartRequest request = new CaptureCodegenStartRequest();
        request.targetUrl = targetUrl;
        request.browser = browser;
        request.headless = resolveHeadless(headless);
        request.apiCapture = true;
        request.networkOptions = networkOptions == null ? new NetworkCaptureOptions() : networkOptions;
        return new McpCaptureApiUnionStatus(ActiveEngine.WEB, startWithOptions(request), null);
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
            description = "returns SHAFT Capture session status including network transaction count and recent "
                    + "endpoints; dispatches to the mobile loopback MITM proxy when a mobile engine is active, or "
                    + "when a standalone mobile API session (started with no active engine) is already running")
    public McpCaptureApiUnionStatus apiStatus() {
        if (mobileApiSessionActiveOrEngineIsMobile()) {
            return new McpCaptureApiUnionStatus(EngineService.activeEngine(), null, mobileService.mobileApiRecordStatus());
        }
        return new McpCaptureApiUnionStatus(ActiveEngine.WEB, manager.status(), null);
    }

    /**
     * Stops the active API capture recording.
     *
     * @param discard whether to delete capture artifacts after shutdown
     * @return final recorder status
     */
    @Tool(name = "capture_api_stop",
            description = "stops SHAFT API Capture with the same single-session lock guarantee as capture_stop; "
                    + "dispatches to the mobile loopback MITM proxy when a mobile engine is active, or when a "
                    + "standalone mobile API session (started with no active engine) is already running")
    public McpCaptureApiUnionStatus apiStop(boolean discard) {
        if (mobileApiSessionActiveOrEngineIsMobile()) {
            return new McpCaptureApiUnionStatus(EngineService.activeEngine(), null, mobileService.mobileApiRecordStop(discard));
        }
        return new McpCaptureApiUnionStatus(ActiveEngine.WEB, manager.stop(discard), null);
    }

    /**
     * Returns sanitized summaries of captured network transactions.
     *
     * @param includeAssets whether to include asset resource types (images, fonts, stylesheets, media)
     * @param excludePattern optional {@code |}-separated glob pattern(s) of URLs to exclude
     * @return ordered, bounded list of transaction summaries without sensitive data
     */
    @Tool(name = "capture_api_transactions",
            description = "returns captured network transactions without bodies or sensitive headers; supports "
                    + "filtering asset noise on WEB; dispatches to the mobile loopback MITM proxy's transactions "
                    + "when a mobile engine is active, or when a standalone mobile API session is already running "
                    + "(includeAssets/excludePattern are WEB-only there)")
    public List<NetworkTransaction> apiTransactions(
            boolean includeAssets,
            String excludePattern) {
        if (mobileApiSessionActiveOrEngineIsMobile()) {
            return mobileService.mobileApiRecordTransactions();
        }
        NetworkCaptureOptions filter = new NetworkCaptureOptions();
        filter.excludeAssets = !includeAssets;
        filter.excludePattern = excludePattern == null ? "" : excludePattern;
        return manager.networkTransactions(filter, 100);
    }

    /**
     * Whether capture_api_status/capture_api_stop/capture_api_transactions should dispatch to the
     * mobile loopback MITM proxy: either a mobile engine is active, or -- since that proxy is
     * deliberately standalone and never requires one (see {@link #apiStart}) -- a mobile API session
     * is already running with no engine active at all.
     */
    private boolean mobileApiSessionActiveOrEngineIsMobile() {
        ActiveEngine engine = EngineService.activeEngine();
        if (engine == ActiveEngine.MOBILE_NATIVE || engine == ActiveEngine.MOBILE_WEB) {
            return true;
        }
        return mobileService.mobileApiRecordStatus().active();
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
     * @param validationDepth {@code STATUS}, {@code STATUS_HEADERS}, {@code SCHEMA} (default),
     *                        {@code FULL_BODY}, or {@code BUSINESS} (pins each stable business
     *                        field by JSON path; skips volatile/correlated/sensitive leaves)
     * @param overwrite whether existing artifacts may be replaced
     * @param replay whether to execute the generated test after compiling (off by default; unsafe
     *               to enable automatically for non-idempotent methods)
     * @param openApiSpecPath optional path (inside the MCP workspace) to an OpenAPI JSON/YAML spec
     *                        to cross-report recorded endpoints against; blank skips coverage
     * @param excludedTransactionIds recorded transaction IDs to omit from generation entirely
     *                               (drives a recorder transaction-table include/exclude selection);
     *                               empty includes every renderable transaction
     * @param pinnedJsonPaths response JSON paths (e.g. {@code $.status}) to force-assert as business
     *                        assertions at {@code BUSINESS} validation depth even when the leaf is
     *                        classified volatile or correlated; a sensitive or blank-value leaf is
     *                        never asserted regardless of pinning; empty pins nothing (the default)
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
            String openApiSpecPath,
            List<String> excludedTransactionIds,
            List<String> pinnedJsonPaths) {
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
                openApiSpec,
                CaptureGenerationRequest.EnrichmentMode.NONE,
                null,
                false,
                null,
                excludedTransactionIds,
                pinnedJsonPaths == null ? List.of() : pinnedJsonPaths));
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

    /**
     * Lists classified response leaves (stable/volatile/sensitive) per transaction for a recorded
     * session, without generating any test source -- the data source for a "pin this path as a
     * business assertion" picker (issue #3530 negative-case). A sensitive leaf's value is redacted.
     *
     * @param sessionPath persisted Capture JSON path
     * @param excludedTransactionIds transaction ids to omit, same semantics as
     *                               {@code capture_api_generate}
     * @return one entry per renderable transaction, each carrying its classified leaves
     */
    @Tool(name = "capture_api_response_leaves",
            description = "returns classified response-body leaves (stable/volatile/sensitive) per transaction for a "
                    + "recorded session without generating any test source, for a pin-this-path picker; sensitive values are redacted")
    public List<ApiCaptureGenerator.TransactionLeaves> apiResponseLeaves(
            String sessionPath, List<String> excludedTransactionIds) {
        return new ApiCaptureGenerator().listResponseLeaves(
                workspacePolicy.existing(sessionPath, "Capture session path"),
                excludedTransactionIds == null ? List.of() : excludedTransactionIds);
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
     * <p>When no valid candidates are supplied (for example, an IntelliJ Pick-Locator caret action
     * probing readiness with an empty payload), this falls back to the last pick persisted under
     * this service's session runtime directory by {@code CaptureControlServer}'s
     * {@code /locator/pick} endpoint -- see {@link com.shaft.capture.control.CaptureControlFiles#readLastPick()}.
     * Freshness of that persisted pick (how long ago it was captured) is a documented non-goal for
     * v1; see {@link com.shaft.capture.control.CaptureControlFiles.LastPick#capturedAtMillis()}.
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
            return persistedPick();
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

    /**
     * Reads the last locator pick persisted under {@link #RUNTIME_DIRECTORY} -- the same session
     * runtime directory this service always uses to start managed capture sessions -- so a caller
     * with no candidates of its own can still recover the user's most recent recorder pick.
     *
     * @return the persisted pick, or a blank result when none has been persisted yet
     */
    private McpPickLocatorResult persistedPick() {
        com.shaft.capture.control.CaptureControlFiles.LastPick lastPick =
                new com.shaft.capture.control.CaptureControlFiles(RUNTIME_DIRECTORY).readLastPick();
        if (lastPick == null) {
            return new McpPickLocatorResult("", List.of());
        }
        List<McpRankedLocatorCandidate> ranked = lastPick.candidates().stream()
                .map(candidate -> new McpRankedLocatorCandidate(
                        candidate.strategy(), candidate.expression(), candidate.score(), candidate.snippet()))
                .toList();
        return new McpPickLocatorResult(lastPick.snippet(), ranked);
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
     * <p>Plain Java overload with no progress reporting, preserved for direct callers (for example,
     * white-box tests); the MCP-exposed tool is {@link #generateReplay(String, String, String, String,
     * boolean, boolean, boolean, boolean, boolean, String, String, McpSyncServerExchange)}.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing generated source and test-data files may be replaced; status reports are always refreshed
     * @param replay whether to execute the generated test
     * @param useAi whether to request optional AI enrichment preview
     * @param allowLocalAi explicit approval for local inference
     * @param allowRemoteAi explicit approval for remote inference
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generation report plus copy-paste code blocks
     */
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
        return generateReplay(sessionPath, outputDirectory, packageName, className, overwrite, replay, useAi,
                allowLocalAi, allowRemoteAi, driverVariableName, null, null, null);
    }

    /**
     * Generates, compiles, and optionally replays a deterministic SHAFT TestNG test from a Capture session,
     * streaming best-effort milestone progress over the MCP exchange when the caller requested it.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace; blank uses the most
     *                     recently modified recording under this workspace's {@code recordings/}
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing generated source and test-data files may be replaced; status reports are always refreshed
     * @param replay whether to execute the generated test
     * @param useAi whether to request optional AI enrichment preview
     * @param allowLocalAi explicit approval for local inference
     * @param allowRemoteAi explicit approval for remote inference
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @param progressToken MCP progress token supplied by the requester; null when the client did
     *                      not request progress notifications
     * @param exchange live MCP server exchange used to emit progress notifications; injected by the
     *                 annotation-scanning MCP tool provider, never exposed in the tool's input schema
     * @return generation report plus copy-paste code blocks
     */
    @McpTool(name = "capture_generate_replay",
            description = "generates, compiles, optionally replays, and returns copy-paste SHAFT code blocks from a "
                    + "persisted recording JSON (sessionPath); works on any recording file, no active capture session "
                    + "required; optional backend (web|playwright|mobile) selects the codegen target, defaulting to "
                    + "the active engine (absorbs playwright_capture_generate_replay/playwright_replay_recording/"
                    + "mobile_replay_recording)")
    public McpCaptureReplayResult generateReplay(
            // @McpTool-annotated methods register through Spring AI's separate annotation-scanning MCP
            // path (McpJsonSchemaGenerator), which does not honor @ToolParam -- confirmed empirically:
            // it silently kept sessionPath required. @McpToolParam is the equivalent for this path.
            @McpToolParam(required = false, description = "persisted Capture JSON path inside the MCP workspace; "
                    + "blank uses the most recently modified recording under recordings/")
            String sessionPath,
            String outputDirectory,
            String packageName,
            String className,
            boolean overwrite,
            boolean replay,
            boolean useAi,
            boolean allowLocalAi,
            boolean allowRemoteAi,
            String driverVariableName,
            @McpToolParam(required = false, description = "codegen target: web (default) | playwright | mobile; "
                    + "blank infers from the active engine")
            String backend,
            @McpProgressToken String progressToken,
            McpSyncServerExchange exchange) {
        String resolvedDriverVariableName = defaultIfBlank(driverVariableName, "driver");
        CodegenTarget target = resolveCodegenTarget(backend, resolveSessionPath(sessionPath));
        if (target == CodegenTarget.MOBILE) {
            return replay
                    ? fromMobileResult(mobileService.replayRecording(sessionPath, resolvedDriverVariableName))
                    : fromMobileResult(mobileService.recordingCodeBlocks(sessionPath, resolvedDriverVariableName));
        }
        if (target == CodegenTarget.PLAYWRIGHT_STEP_RECORDING) {
            return replay
                    ? fromMobileResult(playwrightService.replayRecording(sessionPath, resolvedDriverVariableName))
                    : fromMobileResult(playwrightService.recordingCodeBlocks(sessionPath, resolvedDriverVariableName));
        }
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
                target == CodegenTarget.PLAYWRIGHT ? CodegenBackend.PLAYWRIGHT : CodegenBackend.WEBDRIVER,
                progressReporter(progressToken, exchange));
        return replayResult(result, resolvedDriverVariableName);
    }

    /**
     * Builds a best-effort progress-notification sink for a Capture generation call: a no-op when
     * the client did not request progress (no token) or the tool is invoked outside a live MCP
     * exchange (for example, direct unit tests).
     *
     * @param progressToken MCP progress token supplied by the requester, or null/blank
     * @param exchange live MCP server exchange, or null outside a real MCP call
     * @return a progress sink safe to pass to {@link CaptureGenerator#generate}
     */
    private static BiConsumer<Double, String> progressReporter(String progressToken, McpSyncServerExchange exchange) {
        if (exchange == null || progressToken == null || progressToken.isBlank()) {
            return (fraction, message) -> { };
        }
        return (fraction, message) -> exchange.progressNotification(
                McpSchema.ProgressNotification.builder(progressToken, fraction)
                        .total(1.0)
                        .message(message)
                        .build());
    }

    /**
     * Generates deterministic copy-paste code blocks from a persisted Capture session without replaying.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace; blank uses the most
     *                     recently modified recording under this workspace's {@code recordings/}
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing generated source and test-data files may be replaced; status reports are always refreshed
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generated snippets and report
     */
    @Tool(name = "capture_code_blocks",
            description = "generates a Java full-class snippet plus agent guidance for repo-aware insertion from a "
                    + "persisted recording JSON (sessionPath); works on any recording file, no active capture session "
                    + "required; optional backend (web|playwright|mobile) selects the codegen target, defaulting to "
                    + "the active engine (absorbs playwright_capture_code_blocks/mobile_recording_code_blocks)")
    public McpCaptureReplayResult codeBlocks(
            @ToolParam(required = false, description = "persisted Capture JSON path inside the MCP workspace; "
                    + "blank uses the most recently modified recording under recordings/")
            String sessionPath,
            @ToolParam(required = false, description = "generated project root inside the MCP workspace; "
                    + "defaults to generated-tests under the workspace")
            String outputDirectory,
            @ToolParam(required = false, description = "Java package for the generated class; "
                    + "defaults to tests.generated")
            String packageName,
            @ToolParam(required = false, description = "Java class name for the generated class; "
                    + "defaults to RecordedFlowTest")
            String className,
            @ToolParam(required = false, description = "overwrite an existing generated file; "
                    + "defaults to false")
            Boolean overwrite,
            @ToolParam(required = false, description = "Java driver variable name used in extracted snippets; "
                    + "defaults to driver")
            String driverVariableName,
            @ToolParam(required = false, description = "codegen target: web (default) | playwright | mobile; "
                    + "blank infers from the active engine")
            String backend) {
        String resolvedDriverVariableName = defaultIfBlank(driverVariableName, "driver");
        CodegenTarget target = resolveCodegenTarget(backend, resolveSessionPath(sessionPath));
        if (target == CodegenTarget.MOBILE) {
            return fromMobileResult(mobileService.recordingCodeBlocks(sessionPath, resolvedDriverVariableName));
        }
        if (target == CodegenTarget.PLAYWRIGHT_STEP_RECORDING) {
            return fromMobileResult(playwrightService.recordingCodeBlocks(sessionPath, resolvedDriverVariableName));
        }
        String resolvedPackageName = defaultIfBlank(packageName, "tests.generated");
        String resolvedClassName = defaultIfBlank(className, "RecordedFlowTest");
        boolean resolvedOverwrite = Boolean.TRUE.equals(overwrite);

        CaptureGenerationResult result = generateInternal(
                sessionPath,
                outputDirectory,
                resolvedPackageName,
                resolvedClassName,
                resolvedOverwrite,
                false,
                false,
                false,
                false,
                false,
                false,
                target == CodegenTarget.PLAYWRIGHT ? CodegenBackend.PLAYWRIGHT : CodegenBackend.WEBDRIVER);
        return replayResult(result, resolvedDriverVariableName);
    }

    /**
     * Generates focused record-at-target code blocks for an existing Java source anchor.
     *
     * @param sessionPath persisted Capture JSON path inside the MCP workspace
     * @param outputDirectory generated project root inside the MCP workspace
     * @param packageName generated Java package
     * @param className optional generated class name
     * @param overwrite whether existing generated source and test-data files may be replaced; status reports are always refreshed
     * @param targetSourcePath existing Java source path inside the MCP workspace
     * @param insertAfter method name or textual anchor to insert after
     * @param driverVariableName Java driver variable name used in extracted snippets
     * @return generated snippets and report
     */
    @Tool(name = "capture_record_at_target_code_blocks",
            description = "generates focused Capture snippets for insertion at an existing Java source anchor; "
                    + "optional backend (web|playwright|mobile) selects the codegen target, defaulting to the "
                    + "active engine (absorbs playwright_record_at_target_code_blocks/mobile_record_at_target_code_blocks)")
    public McpCaptureReplayResult recordAtTargetCodeBlocks(
            @ToolParam(required = false, description = "persisted Capture JSON path inside the MCP workspace; "
                    + "blank uses the most recently modified recording under recordings/")
            String sessionPath,
            @ToolParam(required = false, description = "generated project root inside the MCP workspace; "
                    + "defaults to generated-tests under the workspace")
            String outputDirectory,
            @ToolParam(required = false, description = "Java package for the generated class; "
                    + "defaults to tests.generated")
            String packageName,
            @ToolParam(required = false, description = "Java class name for the generated class; "
                    + "defaults to RecordedFlowTest")
            String className,
            @ToolParam(required = false, description = "overwrite an existing generated file; "
                    + "defaults to false")
            Boolean overwrite,
            String targetSourcePath,
            String insertAfter,
            @ToolParam(required = false, description = "Java driver variable name used in extracted snippets; "
                    + "defaults to driver")
            String driverVariableName,
            @ToolParam(required = false, description = "codegen target: web (default) | playwright | mobile; "
                    + "blank infers from the active engine")
            String backend) {
        String resolvedDriverVariableName = defaultIfBlank(driverVariableName, "driver");
        CodegenTarget target = resolveCodegenTarget(backend, resolveSessionPath(sessionPath));
        if (target == CodegenTarget.MOBILE) {
            return fromMobileResult(mobileService.recordAtTargetCodeBlocks(
                    sessionPath, resolvedDriverVariableName, targetSourcePath, insertAfter));
        }
        if (target == CodegenTarget.PLAYWRIGHT_STEP_RECORDING) {
            return fromMobileResult(playwrightService.recordAtTargetCodeBlocks(
                    sessionPath, resolvedDriverVariableName, targetSourcePath, insertAfter));
        }
        String resolvedPackageName = defaultIfBlank(packageName, "tests.generated");
        String resolvedClassName = defaultIfBlank(className, "RecordedFlowTest");
        boolean resolvedOverwrite = Boolean.TRUE.equals(overwrite);

        CaptureGenerationResult result = generateInternal(
                sessionPath,
                outputDirectory,
                resolvedPackageName,
                resolvedClassName,
                resolvedOverwrite,
                false,
                false,
                false,
                false,
                false,
                false,
                target == CodegenTarget.PLAYWRIGHT ? CodegenBackend.PLAYWRIGHT : CodegenBackend.WEBDRIVER);
        return replayResult(
                result,
                resolvedDriverVariableName,
                workspacePolicy.existing(targetSourcePath, "Capture target source path"),
                insertAfter);
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
     * Defaults a string if it is null or blank.
     *
     * @param value the value to check
     * @param fallback the fallback value if value is null or blank
     * @return value if it is not blank, otherwise fallback
     */
    private static String defaultIfBlank(String value, String fallback) {
        return value == null || value.isBlank() ? fallback : value;
    }

    /**
     * Codegen target for the unified {@code capture_generate_replay}/{@code capture_code_blocks}/
     * {@code capture_record_at_target_code_blocks} tools (design doc Decision 2 absorption of the
     * former {@code playwright_*}/{@code mobile_*} codegen twins). {@code PLAYWRIGHT_STEP_RECORDING}
     * is a distinct target from {@code PLAYWRIGHT}: a live {@code capture_start} call on the
     * Playwright engine writes through {@link PlaywrightService}'s own step recorder (the same
     * {@code actions}-array JSON shape {@link MobileService} uses), never a Capture-JSON
     * ({@code events}-array) session -- so a bare backend="playwright" request must still route to
     * the correct one for whichever file it is actually pointed at.
     */
    private enum CodegenTarget { WEB, PLAYWRIGHT, PLAYWRIGHT_STEP_RECORDING, MOBILE }

    /**
     * Resolves which engine's codegen path a capture codegen tool should use: an explicit
     * {@code backend} override always wins, otherwise it is inferred from {@link EngineService}'s
     * active engine so a bare call reaches whichever session actually produced the recording. When
     * the resolved target is Playwright, the session file itself is sniffed to tell apart a
     * live-recorded step JSON from a Capture-JSON session rendered as Playwright code.
     *
     * @param backend explicit override ("web"/"playwright"/"mobile"), or blank to infer
     * @param sessionPath the (already-resolved, may be blank) session file path to sniff for PLAYWRIGHT
     * @return the resolved codegen target
     */
    private CodegenTarget resolveCodegenTarget(String backend, String sessionPath) {
        CodegenTarget requested;
        if (backend != null && !backend.isBlank()) {
            requested = switch (backend.trim().toUpperCase(Locale.ROOT)) {
                case "PLAYWRIGHT" -> CodegenTarget.PLAYWRIGHT;
                case "MOBILE" -> CodegenTarget.MOBILE;
                default -> CodegenTarget.WEB;
            };
        } else {
            requested = switch (EngineService.activeEngine()) {
                case PLAYWRIGHT -> CodegenTarget.PLAYWRIGHT;
                case MOBILE_NATIVE, MOBILE_WEB -> CodegenTarget.MOBILE;
                case WEB, NONE -> CodegenTarget.WEB;
            };
        }
        return requested == CodegenTarget.PLAYWRIGHT && looksLikeStepRecording(sessionPath)
                ? CodegenTarget.PLAYWRIGHT_STEP_RECORDING
                : requested;
    }

    /**
     * Sniffs whether a session file is a live step recording ({@link PlaywrightService}'s or
     * {@link MobileService}'s own JSON, top-level {@code actions} array) rather than a Capture-JSON
     * session (top-level {@code events} array). Cheap substring check, not a full parse: both shapes
     * are always written pretty-printed by their respective Jackson writers, so the discriminating
     * key reliably appears near the top of the file either way.
     *
     * @param sessionPath session file path; blank/unreadable safely resolves to false (Capture-JSON)
     * @return true when the file looks like a step recording
     */
    private boolean looksLikeStepRecording(String sessionPath) {
        if (sessionPath == null || sessionPath.isBlank()) {
            return false;
        }
        try {
            Path resolved = workspacePolicy.existing(sessionPath, "Capture session path");
            String content = Files.readString(resolved);
            return content.contains("\"actions\"") && !content.contains("\"events\"");
        } catch (RuntimeException | IOException ignored) {
            return false;
        }
    }

    /**
     * Adapts a mobile recorder's replay/codegen result into the shared {@link McpCaptureReplayResult}
     * shape so {@code capture_generate_replay}/{@code capture_code_blocks}/
     * {@code capture_record_at_target_code_blocks} can return a single result type regardless of
     * which engine produced the recording.
     *
     * @param mobile mobile recorder result
     * @return the equivalent capture replay result
     */
    private static McpCaptureReplayResult fromMobileResult(McpMobileReplayResult mobile) {
        return new McpCaptureReplayResult(
                mobile.sourcePath(),
                mobile.testDataPath(),
                mobile.reportPath(),
                mobile.reviewPath(),
                mobile.successful(),
                mobile.codeBlocks(),
                mobile.report(),
                mobile.warnings());
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
                backend,
                (fraction, message) -> { });
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
            CodegenBackend backend,
            BiConsumer<Double, String> progress) {
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
                workspacePolicy.existing(resolveSessionPath(sessionPath), "Capture session path"),
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
                backend,
                progress);
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
