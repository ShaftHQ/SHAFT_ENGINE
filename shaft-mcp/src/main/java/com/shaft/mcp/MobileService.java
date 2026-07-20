package com.shaft.mcp;

import com.shaft.capture.runtime.NetworkTransaction;
import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
import com.shaft.tools.io.internal.MobileTraceMetadata;
import com.shaft.tools.io.internal.TraceEventRecorder;
import io.appium.java_client.remote.SupportsContextSwitching;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.ScreenOrientation;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.interactions.Pause;
import org.openqa.selenium.interactions.PointerInput;
import org.openqa.selenium.interactions.Sequence;
import org.openqa.selenium.remote.RemoteWebDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Base64;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static com.shaft.mcp.EngineService.getDriver;
import static com.shaft.mcp.EngineService.getLocator;

/**
 * MCP tools for mobile web emulation, Appium native sessions, and SHAFT touch actions.
 */
@Service
public class MobileService {
    private static final Logger logger = LoggerFactory.getLogger(MobileService.class);
    private static final int DEFAULT_SOURCE_CHARACTER_LIMIT = 200_000;
    private static final String DEFAULT_APPIUM_SERVER = "http://127.0.0.1:4723";
    private static final int DEFAULT_SWIPE_COORDINATE_DURATION_MILLIS = 100;

    private final EngineService engineService;
    private final McpMobileRecordingService recorder;
    private final McpMobileInspectorRecordingService inspectorRecorder;
    private final McpWorkspacePolicy workspacePolicy;
    private final MobileApiCaptureController apiCaptureController;

    @Autowired
    public MobileService(EngineService engineService) {
        this(engineService, McpWorkspacePolicy.current());
    }

    MobileService(EngineService engineService, McpWorkspacePolicy workspacePolicy) {
        this(engineService, new McpMobileRecordingService(workspacePolicy), workspacePolicy);
    }

    MobileService(
            EngineService engineService,
            McpWorkspacePolicy workspacePolicy,
            MobileApiCaptureController apiCaptureController) {
        this(engineService, new McpMobileRecordingService(workspacePolicy), workspacePolicy, apiCaptureController);
    }

    MobileService(EngineService engineService, McpMobileRecordingService recorder) {
        this(engineService, recorder, McpWorkspacePolicy.current());
    }

    MobileService(EngineService engineService, McpMobileRecordingService recorder, McpWorkspacePolicy workspacePolicy) {
        this(engineService, recorder, workspacePolicy,
                new McpMobileInspectorRecordingService(workspacePolicy, recorder));
    }

    MobileService(
            EngineService engineService,
            McpMobileRecordingService recorder,
            McpWorkspacePolicy workspacePolicy,
            MobileApiCaptureController apiCaptureController) {
        this(engineService, recorder, workspacePolicy,
                new McpMobileInspectorRecordingService(workspacePolicy, recorder), apiCaptureController);
    }

    MobileService(
            EngineService engineService,
            McpMobileRecordingService recorder,
            McpWorkspacePolicy workspacePolicy,
            McpMobileInspectorRecordingService inspectorRecorder) {
        this(engineService, recorder, workspacePolicy, inspectorRecorder, new MobileApiCaptureController());
    }

    MobileService(
            EngineService engineService,
            McpMobileRecordingService recorder,
            McpWorkspacePolicy workspacePolicy,
            McpMobileInspectorRecordingService inspectorRecorder,
            MobileApiCaptureController apiCaptureController) {
        this.engineService = engineService;
        this.recorder = recorder;
        this.workspacePolicy = workspacePolicy;
        this.inspectorRecorder = inspectorRecorder;
        this.apiCaptureController = apiCaptureController;
        registerMobileInitBridge();
    }

    /**
     * Registers this instance as {@link EngineService}'s mobile-init bridge so {@code driver_initialize}
     * can dispatch {@code engine=MOBILE_NATIVE}/{@code MOBILE_WEB} requests here without EngineService
     * holding a direct {@link MobileService} field (design doc amendment A9; mirrors
     * {@link EngineService#registerCaptureDriverBridge}).
     */
    private void registerMobileInitBridge() {
        EngineService.registerMobileInitBridge((engine, options) -> {
            if (engine == ActiveEngine.MOBILE_NATIVE) {
                options.initializeNative(this);
            } else {
                options.initializeWebEmulation(this);
            }
        });
    }

    /**
     * Starts a local browser in Chrome/Edge mobile emulation mode. Not an MCP tool since commit 4
     * (design doc Decision 2/amendment A9): reached only through {@code driver_initialize}'s
     * {@code engine=MOBILE_WEB} + {@code mobileOptions} dispatch now.
     *
     * @param targetUrl initial URL; blank leaves the browser on its startup page
     * @param browser browser name, defaults to CHROME
     * @param deviceName Chrome/Edge emulated device name; blank defaults to Pixel 5
     * @param width custom device width; requires height
     * @param height custom device height; requires width
     * @param pixelRatio custom device pixel ratio
     * @param userAgent optional custom mobile user agent
     * @param headless whether the emulated browser should run headlessly
     * @return session result and copy-paste setup block
     */
    McpMobileSessionResult initializeWebEmulation(
            String targetUrl,
            String browser,
            String deviceName,
            int width,
            int height,
            double pixelRatio,
            String userAgent,
            boolean headless) {
        BrowserType browserType = browserType(browser);
        boolean customDevice = width > 0 && height > 0;
        String effectiveDevice = textOrDefault(deviceName, "Pixel 5");

        engineService.quitDriver();
        SHAFT.Properties.platform.set().targetPlatform("Linux");
        SHAFT.Properties.mobile.set().browserName("");
        SHAFT.Properties.web.set()
                .targetBrowserName(browserType.name())
                .headlessExecution(headless)
                .isMobileEmulation(true)
                .mobileEmulationIsCustomDevice(customDevice)
                .mobileEmulationDeviceName(customDevice ? "" : effectiveDevice)
                .mobileEmulationWidth(customDevice ? width : 0)
                .mobileEmulationHeight(customDevice ? height : 0)
                .mobileEmulationPixelRatio(pixelRatio > 0 ? pixelRatio : 1.0)
                .mobileEmulationUserAgent(text(userAgent));
        engineService.initializeConfiguredDriver("mobile web emulation", ActiveEngine.MOBILE_WEB);
        if (!text(targetUrl).isBlank()) {
            getDriver().browser().navigateToURL(targetUrl);
        }
        logger.info("Mobile web emulation initialized (browser: {}, customDevice: {})", browserType, customDevice);
        return new McpMobileSessionResult(
                "web-emulation",
                "",
                customDevice ? width + "x" + height : effectiveDevice,
                browserType.name(),
                true,
                List.of(webEmulationSetupBlock(targetUrl, browserType, effectiveDevice, width, height, pixelRatio,
                        userAgent, headless)),
                customDevice ? List.of() : List.of("Device names must match Chrome/Edge DevTools emulation names."),
                mobileDeviceProfile(browserType.name()));
    }

    /**
     * Starts an Appium native Android or iOS session through SHAFT driver setup. Not an MCP tool
     * since commit 4 (design doc Decision 2/amendment A9): reached only through
     * {@code driver_initialize}'s {@code engine=MOBILE_NATIVE} + {@code mobileOptions} dispatch now.
     *
     * @param platformName Android or iOS
     * @param deviceName emulator, simulator, or real device name
     * @param appiumServerUrl Appium server URL; blank defaults to http://127.0.0.1:4723
     * @param automationName Appium automation name; blank selects UiAutomator2 for Android and XCUITest for iOS
     * @param platformVersion optional platform version
     * @param udid optional device UDID
     * @param app optional app path or remote app URL
     * @param appPackage optional Android app package
     * @param appActivity optional Android app activity
     * @param bundleId optional iOS bundle identifier for installed apps
     * @return session result and copy-paste setup block
     */
    McpMobileSessionResult initializeNative(
            String platformName,
            String deviceName,
            String appiumServerUrl,
            String automationName,
            String platformVersion,
            String udid,
            String app,
            String appPackage,
            String appActivity,
            String bundleId) {
        String platform = platform(platformName);
        String automation = textOrDefault(automationName, platform.equals("Android") ? "UiAutomator2" : "XCUITest");
        String server = textOrDefault(appiumServerUrl, DEFAULT_APPIUM_SERVER);

        engineService.quitDriver();
        SHAFT.Properties.platform.set()
                .targetPlatform(platform)
                .executionAddress(server);
        SHAFT.Properties.web.set().isMobileEmulation(false);
        SHAFT.Properties.mobile.set()
                .platformName(platform)
                .platformVersion(text(platformVersion))
                .deviceName(text(deviceName))
                .automationName(automation)
                .udid(text(udid))
                .browserName("")
                .app(text(app))
                .appPackage(text(appPackage))
                .appActivity(text(appActivity))
                .bundleId(text(bundleId));
        engineService.initializeConfiguredDriver("mobile native " + platform, ActiveEngine.MOBILE_NATIVE);
        logger.info("Native mobile session initialized (platform: {}, device name length: {})",
                platform, text(deviceName).length());
        return new McpMobileSessionResult(
                "native",
                platform,
                text(deviceName),
                "",
                true,
                List.of(nativeSetupBlock(platform, deviceName, server, automation, platformVersion, udid, app,
                        appPackage, appActivity, bundleId)),
                nativeWarnings(platform, app, appPackage, appActivity, bundleId));
    }

    /**
     * Starts recording mobile actions performed through MCP mobile tools.
     *
     * @param outputPath workspace-contained JSON output path
     * @param mode recording label
     * @param includeSensitiveValues whether typed values should be stored for exact replay
     * @return recorder status
     */
    public McpMobileRecordingStatus recordStart(String outputPath, String mode, boolean includeSensitiveValues) {
        return recorder.start(outputPath, mode, includeSensitiveValues);
    }

    /**
     * Returns mobile recording status.
     *
     * @return recorder status
     */
    public McpMobileRecordingStatus recordStatus() {
        return recorder.status();
    }

    /**
     * Stops mobile recording.
     *
     * @param discard whether to delete the recording output
     * @return final recorder status
     */
    public McpMobileRecordingStatus recordStop(boolean discard) {
        return recorder.stop(discard);
    }

    /**
     * Deletes a recorded mobile step by its stable stepId, as surfaced in recorder status.
     *
     * @param stepId stable step id (e.g. "m2") from {@link McpMobileRecordingStatus#steps()}
     * @return updated recorder status
     */
    public McpMobileRecordingStatus stepDelete(String stepId) {
        return recorder.deleteStep(stepId);
    }

    /**
     * Moves a recorded mobile step up or down by its stable stepId, as surfaced in recorder status.
     *
     * @param stepId stable step id (e.g. "m2") from {@link McpMobileRecordingStatus#steps()}
     * @param direction "up" or "down"
     * @return updated recorder status
     */
    public McpMobileRecordingStatus stepReorder(String stepId, String direction) {
        return recorder.reorderStep(stepId, direction);
    }

    /**
     * Starts a loopback MITM proxy that captures native mobile API traffic as a first-class
     * capture session, independent of any Appium/WebDriver session. HTTPS interception requires the
     * device or emulator to trust the CA certificate returned in the status; plain HTTP traffic
     * needs no such installation. See the returned status's warnings for platform-specific pairing
     * steps and known limitations (Android 7+ CA trust restrictions, certificate pinning).
     *
     * @param platform "Android", "iOS", or a caller-supplied label, stored for reference only
     * @param deviceLabel emulator/simulator/device identifier, stored for reference only
     * @param outputPath workspace-contained JSON output path; blank generates a timestamped path
     * @return capture status, including the loopback proxy port and the per-installation CA
     *         certificate PEM to install as a trusted CA on the device before HTTPS traffic can be
     *         captured
     */
    public MobileApiCaptureStatus mobileApiRecordStart(String platform, String deviceLabel, String outputPath) {
        Path output = outputPath == null || outputPath.isBlank()
                ? workspacePolicy.output(
                        "recordings/mobile-api-" + Instant.now().toString().replace(':', '-') + ".json",
                        "Mobile API capture output path")
                : workspacePolicy.output(outputPath, "Mobile API capture output path");
        return apiCaptureController.start(text(platform), text(deviceLabel), output);
    }

    /**
     * Returns the active mobile API capture status without changing state.
     *
     * @return capture status, including the CA certificate PEM to install on the device and any
     *         non-sensitive warnings (pairing limitations, transactions that could not be recorded)
     */
    public MobileApiCaptureStatus mobileApiRecordStatus() {
        return apiCaptureController.status();
    }

    /**
     * Stops mobile API capture, finalizing (or discarding) the persisted JSON capture session.
     *
     * @param discard whether to mark the session incomplete instead of completed
     * @return final capture status
     */
    public MobileApiCaptureStatus mobileApiRecordStop(boolean discard) {
        return apiCaptureController.stop(discard);
    }

    /**
     * Returns the mobile API transactions captured so far in the active session, without bodies or
     * sensitive headers, so a pure-API session view can list rows live as they arrive.
     *
     * @return ordered, body-free network transaction summaries; empty when no session is active
     */
    public List<NetworkTransaction> mobileApiRecordTransactions() {
        return apiCaptureController.transactions();
    }

    /**
     * Returns local Appium/Android/iOS toolchain discovery status and repair diagnostics.
     *
     * @param platformName Android or iOS; blank defaults to Android
     * @return local mobile toolchain status
     */
    @Tool(name = "mobile_toolchain_status",
            description = "checks local Appium, Inspector, adb, emulator, and SDK tooling status with repair diagnostics")
    public McpMobileToolchainStatus toolchainStatus(String platformName) {
        return inspectorRecorder.toolchainStatus(platformName);
    }

    /**
     * Prepares a user-confirmable wrapped Appium Inspector recording session.
     *
     * @param platformName Android or iOS
     * @param outputPath workspace-contained recording JSON output path
     * @param includeSensitiveValues whether typed values should be stored for exact replay
     * @param app optional app path or remote app URL
     * @param appPackage optional Android app package
     * @param appActivity optional Android app activity
     * @param bundleId optional iOS bundle identifier
     * @param udid optional device UDID
     * @param deviceName optional device or simulator name
     * @param platformVersion optional mobile OS version
     * @param selectedAndroidAvdName cached Android AVD name to start when no real device is connected
     * @param androidApiLevel Android API level for new emulator proposal; non-positive uses SHAFT default
     * @param androidDeviceProfile Android device profile for new emulator proposal
     * @param androidImageTag Android image tag for new emulator proposal
     * @param androidAbi Android emulator image ABI
     * @param androidRamMb Android emulator RAM in MB
     * @param androidCores Android emulator CPU cores
     * @param provisionAndroidEmulator whether to propose creating a fresh Android emulator when needed
     * @return confirmation-ready recording plan
     */
    McpMobileInspectorPlan inspectorRecordPrepare(
            String platformName,
            String outputPath,
            boolean includeSensitiveValues,
            String app,
            String appPackage,
            String appActivity,
            String bundleId,
            String udid,
            String deviceName,
            String platformVersion,
            String selectedAndroidAvdName,
            int androidApiLevel,
            String androidDeviceProfile,
            String androidImageTag,
            String androidAbi,
            int androidRamMb,
            int androidCores,
            boolean provisionAndroidEmulator) {
        return inspectorRecorder.prepare(platformName, outputPath, includeSensitiveValues, app, appPackage,
                appActivity, bundleId, udid, deviceName, platformVersion, selectedAndroidAvdName, androidApiLevel,
                androidDeviceProfile, androidImageTag, androidAbi, androidRamMb, androidCores,
                provisionAndroidEmulator);
    }

    /**
     * Prepares and starts a wrapped Appium Inspector recording session in one call, auto-running the
     * former {@code mobile_inspector_record_prepare} step (design doc Decision 2) so the caller no
     * longer juggles a separate confirmation token round trip. The device/toolchain readiness check
     * ({@link McpMobileInspectorPlan#readyToStart()}) still runs and still fails the call with the
     * same actionable message when the device or toolchain is not ready.
     *
     * @param platformName Android or iOS
     * @param outputPath workspace-contained recording JSON output path
     * @param includeSensitiveValues whether typed values should be stored for exact replay
     * @param app optional app path or remote app URL
     * @param appPackage optional Android app package
     * @param appActivity optional Android app activity
     * @param bundleId optional iOS bundle identifier
     * @param udid optional device UDID
     * @param deviceName optional device or simulator name
     * @param platformVersion optional mobile OS version
     * @param selectedAndroidAvdName cached Android AVD name to start when no real device is connected
     * @param androidApiLevel Android API level for new emulator proposal; non-positive uses SHAFT default
     * @param androidDeviceProfile Android device profile for new emulator proposal
     * @param androidImageTag Android image tag for new emulator proposal
     * @param androidAbi Android emulator image ABI
     * @param androidRamMb Android emulator RAM in MB
     * @param androidCores Android emulator CPU cores
     * @param provisionAndroidEmulator whether to propose creating a fresh Android emulator when needed
     * @param openInspector whether to open the wrapped Inspector URL in the user's browser
     * @return active recording status and Inspector URL
     */
    @Tool(name = "mobile_inspector_record_start",
            description = "prepares and starts a wrapped Appium Inspector recording session in one call, "
                    + "absorbing mobile_inspector_record_prepare")
    public McpMobileInspectorRecordingStatus inspectorRecordStart(
            String platformName,
            String outputPath,
            boolean includeSensitiveValues,
            String app,
            String appPackage,
            String appActivity,
            String bundleId,
            String udid,
            String deviceName,
            String platformVersion,
            String selectedAndroidAvdName,
            int androidApiLevel,
            String androidDeviceProfile,
            String androidImageTag,
            String androidAbi,
            int androidRamMb,
            int androidCores,
            boolean provisionAndroidEmulator,
            boolean openInspector) {
        McpMobileInspectorPlan plan = inspectorRecordPrepare(platformName, outputPath, includeSensitiveValues, app,
                appPackage, appActivity, bundleId, udid, deviceName, platformVersion, selectedAndroidAvdName,
                androidApiLevel, androidDeviceProfile, androidImageTag, androidAbi, androidRamMb, androidCores,
                provisionAndroidEmulator);
        return inspectorRecorder.start(plan.confirmationToken(), selectedAndroidAvdName, openInspector);
    }

    /**
     * Returns wrapped Appium Inspector recording status, or performs a control action first when
     * {@code action} is supplied, absorbing the former {@code mobile_inspector_record_control} tool
     * (design doc Decision 2).
     *
     * @param action blank for a plain status read, or pause|resume|checkpoint|stop|discard to control
     *               the recording first
     * @param checkpointName optional checkpoint name; only used when action is "checkpoint"
     * @return current (or post-control) Inspector recording status
     */
    @Tool(name = "mobile_inspector_record_status",
            description = "returns the wrapped Appium Inspector recording status; optional action "
                    + "(pause|resume|checkpoint|stop|discard) performs that control first, absorbing "
                    + "mobile_inspector_record_control")
    public McpMobileInspectorRecordingStatus inspectorRecordStatus(
            @ToolParam(required = false) String action,
            @ToolParam(required = false) String checkpointName) {
        if (action != null && !action.isBlank()) {
            return inspectorRecorder.control(action, checkpointName);
        }
        return inspectorRecorder.status();
    }

    /**
     * Stops a wrapped Appium Inspector recording and returns generated replay snippets.
     *
     * @param discard whether to delete the recording output
     * @return final recording status
     */
    @Tool(name = "mobile_inspector_record_stop",
            description = "stops a wrapped Appium Inspector recording and returns generated replay code")
    public McpMobileInspectorRecordingStatus inspectorRecordStop(boolean discard) {
        return inspectorRecorder.stop(discard);
    }

    /**
     * Generates copy-paste replay code from a mobile recording.
     *
     * @param recordingPath workspace-contained recording path
     * @param driverVariableName driver variable name to use in generated snippets
     * @return replay code blocks
     */
    public McpMobileReplayResult recordingCodeBlocks(String recordingPath, String driverVariableName) {
        return recorder.codeBlocks(recordingPath, driverVariableName);
    }

    /**
     * Generates focused mobile recording snippets for insertion into an existing Page Object.
     *
     * @param recordingPath workspace-contained recording path
     * @param driverVariableName driver variable name to use in generated snippets
     * @param targetSourcePath workspace-contained Java Page Object source path
     * @param insertAfter method name or textual anchor to insert after
     * @return replay code blocks plus target insertion snippets
     */
    public McpMobileReplayResult recordAtTargetCodeBlocks(
            String recordingPath,
            String driverVariableName,
            String targetSourcePath,
            String insertAfter) {
        Path targetSource = targetSourcePath == null || targetSourcePath.isBlank()
                ? null
                : workspacePolicy.existing(targetSourcePath, "Mobile target source path");
        return recorder.codeBlocks(recordingPath, driverVariableName, targetSource, insertAfter);
    }

    /**
     * Replays a mobile recording against the active driver session.
     *
     * @param recordingPath workspace-contained recording path
     * @param driverVariableName driver variable name to use in generated snippets
     * @return replay result and replay code blocks
     */
    public McpMobileReplayResult replayRecording(String recordingPath, String driverVariableName) {
        McpMobileRecording recording = recorder.readRecording(recordingPath);
        int replayed = 0;
        for (McpMobileRecordedAction action : recording.actions()) {
            replay(action);
            replayed++;
        }
        McpMobileReplayResult blocks = recorder.codeBlocks(recordingPath, driverVariableName);
        return new McpMobileReplayResult(blocks.recordingPath(), true, replayed, blocks.codeBlocks(), blocks.warnings());
    }

    /**
     * Returns Appium contexts and a bounded current page source.
     *
     * @param maxCharacters maximum source characters to return; non-positive uses a safe default
     * @return context snapshot
     */
    @Tool(name = "mobile_get_contexts",
            description = "gets Appium contexts plus current native XML or web DOM source")
    public McpMobileContextSnapshot getContexts(int maxCharacters) {
        WebDriver seleniumDriver = getDriver().getDriver();
        List<String> contexts = new ArrayList<>();
        String currentContext = "";
        if (seleniumDriver instanceof SupportsContextSwitching contextDriver) {
            Set<String> handles = contextDriver.getContextHandles();
            contexts.addAll(handles);
            currentContext = contextDriver.getContext();
        }
        String source = seleniumDriver.getPageSource();
        String safeSource = source == null ? "" : source;
        int limit = maxCharacters <= 0 ? DEFAULT_SOURCE_CHARACTER_LIMIT : maxCharacters;
        boolean truncated = safeSource.length() > limit;
        return new McpMobileContextSnapshot(
                currentContext,
                contexts,
                truncated ? safeSource.substring(0, limit) : safeSource,
                safeSource.length(),
                truncated,
                truncated ? List.of("Source was truncated; increase maxCharacters for more context.") : List.of());
    }

    /**
     * Returns the current native accessibility XML tree or mobile web source.
     *
     * @param maxCharacters maximum source characters to return; non-positive uses a safe default
     * @return accessibility tree snapshot
     */
    @Tool(name = "mobile_get_accessibility_tree",
            description = "gets the current Appium native accessibility XML tree or mobile web source")
    public McpMobileAccessibilityTree getAccessibilityTree(int maxCharacters) {
        WebDriver seleniumDriver = getDriver().getDriver();
        String currentContext = "";
        if (seleniumDriver instanceof SupportsContextSwitching contextDriver) {
            currentContext = contextDriver.getContext();
        }
        String source = seleniumDriver.getPageSource();
        String safeSource = source == null ? "" : source;
        int limit = maxCharacters <= 0 ? DEFAULT_SOURCE_CHARACTER_LIMIT : maxCharacters;
        boolean truncated = safeSource.length() > limit;
        logger.info("Mobile accessibility tree/source retrieved (characters: {}, truncated: {})",
                safeSource.length(), truncated);
        return new McpMobileAccessibilityTree(
                currentContext,
                truncated ? safeSource.substring(0, limit) : safeSource,
                safeSource.length(),
                truncated,
                truncated ? List.of("Accessibility tree/source was truncated; increase maxCharacters for more context.")
                        : List.of());
    }

    /**
     * Takes a PNG screenshot of the current mobile device viewport.
     *
     * @param outputPath optional workspace-relative or workspace-contained output file path
     * @param includeBase64 whether to include screenshot bytes as base64 in the response
     * @return screenshot metadata and optional base64 payload
     */
    public McpScreenshotResult takeScreenshot(String outputPath, boolean includeBase64) {
        WebDriver seleniumDriver = getDriver().getDriver();
        if (!(seleniumDriver instanceof TakesScreenshot takesScreenshot)) {
            throw new IllegalStateException("The active mobile driver does not support screenshots.");
        }
        byte[] png = takesScreenshot.getScreenshotAs(OutputType.BYTES);
        Path writtenPath = writeScreenshot(outputPath, png);
        logger.info("Mobile screenshot captured (bytes: {}, persisted: {}, base64Included: {})",
                png.length, writtenPath != null, includeBase64);
        return new McpScreenshotResult(
                "image/png",
                png.length,
                includeBase64 ? Base64.getEncoder().encodeToString(png) : null,
                writtenPath == null ? null : writtenPath.toString(),
                includeBase64 ? List.of()
                        : List.of("Base64 omitted; set includeBase64=true to return inline PNG bytes."));
    }

    /**
     * Switches Appium context for native, hybrid, or mobile web sessions.
     *
     * @param contextName target context name such as NATIVE_APP or WEBVIEW_*
     * @return context snapshot after switching
     */
    @Tool(name = "mobile_switch_context",
            description = "switches Appium context, for example NATIVE_APP or WEBVIEW_*")
    public McpMobileContextSnapshot switchContext(String contextName) {
        WebDriver seleniumDriver = getDriver().getDriver();
        if (!(seleniumDriver instanceof SupportsContextSwitching contextDriver)) {
            throw new IllegalStateException("The active driver is not an Appium driver.");
        }
        String contextBefore = safeContext(contextDriver);
        try {
            contextDriver.context(contextName);
        } catch (RuntimeException exception) {
            recordContextSwitch(seleniumDriver, contextName, contextBefore, "", exception);
            throw exception;
        }
        McpMobileContextSnapshot snapshot = getContexts(DEFAULT_SOURCE_CHARACTER_LIMIT);
        recordContextSwitch(seleniumDriver, contextName, contextBefore, snapshot.currentContext(), null);
        return snapshot;
    }

    /**
     * Taps an element by locator.
     */
    public McpMobileActionResult tap(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("tap", locatorStrategy, locatorValue,
                locator -> getDriver().touch().tap(locator),
                "driver.element().touch().tap(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Double taps an element by locator.
     */
    public McpMobileActionResult doubleTap(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("doubleTap", locatorStrategy, locatorValue,
                locator -> getDriver().touch().doubleTap(locator),
                "driver.element().touch().doubleTap(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Long taps an element by locator.
     */
    public McpMobileActionResult longTap(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("longTap", locatorStrategy, locatorValue,
                locator -> getDriver().touch().longTap(locator),
                "driver.element().touch().longTap(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Dispatches a unified {@code element_click} call to the touch-action implementation for the
     * requested {@link ClickMode}. Package-private engine-dispatch seam used by {@link ElementService};
     * reuses the existing {@code @Tool} methods so recording behavior stays identical to calling
     * {@code mobile_tap}/{@code mobile_double_tap}/{@code mobile_long_tap} directly.
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param mode requested click gesture
     * @return recorded action metadata
     */
    McpMobileActionResult dispatchClick(locatorStrategy locatorStrategy, String locatorValue, ClickMode mode) {
        return switch (mode) {
            case DOUBLE -> doubleTap(locatorStrategy, locatorValue);
            case LONG -> longTap(locatorStrategy, locatorValue);
            case SINGLE -> tap(locatorStrategy, locatorValue);
        };
    }

    /**
     * Types text into a mobile element. The typed value is classified per field with the same
     * deterministic privacy policy web capture uses: only values typed into sensitive-looking
     * fields (password, token, and similar locators) or matching secret-value patterns are
     * redacted from the recording, so ordinary inputs such as search boxes stay replayable.
     */
    public McpMobileActionResult type(locatorStrategy locatorStrategy, String locatorValue, String textValue) {
        return typeInternal(locatorStrategy, locatorValue, textValue, false);
    }

    /**
     * Dispatches a unified {@code element_type} call to the touch-recording type/append implementation.
     * Package-private engine-dispatch seam used by {@link ElementService}; reuses the same recording
     * path as {@link #type(locatorStrategy, String, String)} so recording behavior stays identical to
     * calling {@code mobile_type} directly (design doc amendment A1).
     *
     * @param locatorStrategy locator strategy
     * @param locatorValue locator value
     * @param textValue text to type
     * @param append when true, appends to existing content via {@code typeAppend} instead of clearing first
     * @return recorded action metadata
     */
    McpMobileActionResult dispatchType(locatorStrategy locatorStrategy, String locatorValue, String textValue, boolean append) {
        return typeInternal(locatorStrategy, locatorValue, textValue, append);
    }

    private McpMobileActionResult typeInternal(
            locatorStrategy locatorStrategy, String locatorValue, String textValue, boolean append) {
        String method = append ? "typeAppend" : "type";
        String code = "driver.element()." + method + "(" + locatorCode(locatorStrategy, locatorValue)
                + ", " + java(textValue) + ");";
        String redactedCode = "driver.element()." + method + "(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"<redacted>\");";
        boolean sensitive = isSensitiveTypedValue(locatorValue, textValue);
        try {
            if (append) {
                getDriver().element().typeAppend(getLocator(locatorStrategy, locatorValue), textValue);
            } else {
                getDriver().element().type(getLocator(locatorStrategy, locatorValue), textValue);
            }
            return actionResult(method, locatorStrategy, locatorValue, Map.of("value", text(textValue)), code,
                    redactedCode, sensitive);
        } catch (Exception exception) {
            logger.error("Mobile {} failed (values redacted)", method, exception);
            throw exception;
        }
    }

    static boolean isSensitiveTypedValue(String locatorValue, String textValue) {
        var classified = new com.shaft.capture.privacy.CapturePrivacyClassifier().classifyValue(
                text(locatorValue), textValue, text(locatorValue), Map.of());
        return classified.reference().classification()
                != com.shaft.capture.model.ExternalTestDataReference.DataClassification.ORDINARY;
    }

    /**
     * Clears a mobile element.
     */
    public McpMobileActionResult clear(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("clear", locatorStrategy, locatorValue,
                locator -> getDriver().element().clear(locator),
                "driver.element().clear(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Unified mobile swipe gesture (design doc Decision 2): absorbs {@code mobile_swipe_by_offset},
     * {@code mobile_swipe_coordinates}, {@code mobile_swipe_element_into_view}, and
     * {@code mobile_swipe_text_into_view} into a single tool, selecting the underlying gesture from
     * whichever optional params are supplied -- checked most-specific-first: {@code text} (swipe to
     * text), then a locator with {@code offsetX}/{@code offsetY} (swipe by offset), then a locator
     * alone (swipe element into view), then raw {@code startX}/{@code startY}/{@code endX}/{@code endY}
     * coordinates (last-resort escape hatch, same as {@code mobile_tap_coordinates}).
     *
     * @param locatorStrategy locator strategy; used with locatorValue for by-offset/element-into-view
     * @param locatorValue locator value; used with locatorStrategy for by-offset/element-into-view
     * @param direction swipe direction for element-into-view; blank defaults to DOWN
     * @param text target text for Android UiScrollable text-into-view
     * @param movement scroll axis for text-into-view; blank defaults to VERTICAL
     * @param offsetX horizontal offset; requires a locator and offsetY
     * @param offsetY vertical offset; requires a locator and offsetX
     * @param startX coordinate swipe start x; requires startY/endX/endY and no locator/text
     * @param startY coordinate swipe start y
     * @param endX coordinate swipe end x
     * @param endY coordinate swipe end y
     * @return recorded action metadata
     */
    @Tool(name = "mobile_swipe", description = "swipes on the mobile screen; dispatches on whichever optional "
            + "params are supplied -- text (swipe to text) | locator+offsetX/offsetY (swipe by offset) | "
            + "locator alone (swipe element into view, optional direction) | startX/startY/endX/endY "
            + "(coordinate escape hatch); absorbs mobile_swipe_by_offset/mobile_swipe_coordinates/"
            + "mobile_swipe_element_into_view/mobile_swipe_text_into_view")
    public McpMobileActionResult swipe(
            @ToolParam(required = false) locatorStrategy locatorStrategy,
            @ToolParam(required = false) String locatorValue,
            @ToolParam(required = false) String direction,
            @ToolParam(required = false) String text,
            @ToolParam(required = false) String movement,
            @ToolParam(required = false) Integer offsetX,
            @ToolParam(required = false) Integer offsetY,
            @ToolParam(required = false) Integer startX,
            @ToolParam(required = false) Integer startY,
            @ToolParam(required = false) Integer endX,
            @ToolParam(required = false) Integer endY) {
        boolean hasLocator = locatorValue != null && !locatorValue.isBlank();
        boolean hasText = text != null && !text.isBlank();
        boolean hasOffset = offsetX != null && offsetY != null;
        boolean hasCoordinates = startX != null && startY != null && endX != null && endY != null;

        if (hasText) {
            return swipeTextIntoView(text, movement);
        }
        if (hasLocator && hasOffset) {
            return swipeByOffset(locatorStrategy, locatorValue, offsetX, offsetY);
        }
        if (hasLocator) {
            return swipeElementIntoView(locatorStrategy, locatorValue, direction);
        }
        if (hasCoordinates) {
            return swipeCoordinates(startX, startY, endX, endY, DEFAULT_SWIPE_COORDINATE_DURATION_MILLIS);
        }
        throw new IllegalArgumentException("mobile_swipe requires one of: text, a locator "
                + "(optionally with offsetX/offsetY), or startX/startY/endX/endY coordinates.");
    }

    /**
     * Swipes one element by an offset.
     */
    public McpMobileActionResult swipeByOffset(
            locatorStrategy locatorStrategy,
            String locatorValue,
            int xOffset,
            int yOffset) {
        String code = "driver.element().touch().swipeByOffset(" + locatorCode(locatorStrategy, locatorValue)
                + ", " + xOffset + ", " + yOffset + ");";
        try {
            getDriver().touch().swipeByOffset(getLocator(locatorStrategy, locatorValue), xOffset, yOffset);
            return actionResult("swipeByOffset", locatorStrategy, locatorValue,
                    Map.of("xOffset", String.valueOf(xOffset), "yOffset", String.valueOf(yOffset)), code, code, false);
        } catch (Exception exception) {
            logger.error("Mobile swipe-by-offset failed (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Swipes until a target element is visible.
     */
    public McpMobileActionResult swipeElementIntoView(
            locatorStrategy locatorStrategy,
            String locatorValue,
            String direction) {
        TouchActions.SwipeDirection swipeDirection = enumValue(TouchActions.SwipeDirection.class, direction,
                TouchActions.SwipeDirection.DOWN);
        String code = "driver.element().touch().swipeElementIntoView(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"" + swipeDirection.name() + "\");";
        try {
            getDriver().touch().swipeElementIntoView(getLocator(locatorStrategy, locatorValue), swipeDirection);
            return actionResult("swipeElementIntoView", locatorStrategy, locatorValue,
                    Map.of("direction", swipeDirection.name()), code, code, false);
        } catch (Exception exception) {
            logger.error("Mobile swipe element into view failed (locator redacted)", exception);
            throw exception;
        }
    }

    /**
     * Swipes to Android text through UiScrollable.
     */
    public McpMobileActionResult swipeTextIntoView(String targetText, String movement) {
        TouchActions.SwipeMovement swipeMovement = enumValue(TouchActions.SwipeMovement.class, movement,
                TouchActions.SwipeMovement.VERTICAL);
        String code = "driver.element().touch().swipeElementIntoView(" + java(targetText)
                + ", \"" + swipeMovement.name() + "\");";
        try {
            getDriver().touch().swipeElementIntoView(targetText, swipeMovement);
            return actionResult("swipeTextIntoView", null, "", Map.of(
                    "targetText", text(targetText),
                    "movement", swipeMovement.name()), code, code, false);
        } catch (Exception exception) {
            logger.error("Mobile swipe text into view failed (text redacted)", exception);
            throw exception;
        }
    }

    /**
     * Last-resort fallback that taps screen coordinates using W3C touch actions.
     */
    @Tool(name = "mobile_tap_coordinates",
            description = "fallback-only: taps viewport coordinates only after locator-based element_click cannot be used")
    public McpMobileActionResult tapCoordinates(int x, int y) {
        performTapCoordinates(x, y);
        String code = tapCoordinatesCode(x, y);
        return actionResult("tapCoordinates", null, "", Map.of("x", String.valueOf(x), "y", String.valueOf(y)),
                code, code, false);
    }

    /**
     * Last-resort fallback that swipes between screen coordinates using W3C touch actions.
     */
    public McpMobileActionResult swipeCoordinates(int startX, int startY, int endX, int endY, int durationMillis) {
        performSwipeCoordinates(startX, startY, endX, endY, durationMillis);
        String code = swipeCoordinatesCode(startX, startY, endX, endY, durationMillis);
        return actionResult("swipeCoordinates", null, "", Map.of(
                        "startX", String.valueOf(startX),
                        "startY", String.valueOf(startY),
                        "endX", String.valueOf(endX),
                        "endY", String.valueOf(endY),
                        "durationMillis", String.valueOf(durationMillis)),
                code, code, false);
    }

    /**
     * Rotates an Appium device.
     */
    @Tool(name = "mobile_rotate", description = "rotates the mobile device to PORTRAIT or LANDSCAPE")
    public McpMobileActionResult rotate(String orientation) {
        ScreenOrientation target = enumValue(ScreenOrientation.class, orientation, ScreenOrientation.PORTRAIT);
        getDriver().touch().rotate(target);
        String code = "driver.element().touch().rotate(\"" + target.name() + "\");";
        return actionResult("rotate", null, "", Map.of("orientation", target.name()), code, code, false);
    }

    /**
     * Hides the native keyboard.
     */
    @Tool(name = "mobile_hide_keyboard", description = "hides the native mobile keyboard")
    public McpMobileActionResult hideKeyboard() {
        getDriver().touch().hideNativeKeyboard();
        String code = "driver.element().touch().hideNativeKeyboard();";
        return actionResult("hideKeyboard", null, "", Map.of(), code, code, false);
    }

    /**
     * Sends a native keyboard action.
     */
    @Tool(name = "mobile_keyboard_key",
            description = "sends a native keyboard action such as DONE, SEARCH, GO, NEXT, or SEND")
    public McpMobileActionResult keyboardKey(String key) {
        TouchActions.KeyboardKeys keyboardKey = enumValue(TouchActions.KeyboardKeys.class, key,
                TouchActions.KeyboardKeys.DONE);
        getDriver().touch().nativeKeyboardKeyPress(keyboardKey);
        String code = "driver.element().touch().nativeKeyboardKeyPress(\"" + keyboardKey.name() + "\");";
        return actionResult("keyboardKey", null, "", Map.of("key", keyboardKey.name()), code, code, false);
    }

    /**
     * Sends the active app to the background.
     */
    @Tool(name = "mobile_background_app", description = "sends the active mobile app to the background")
    public McpMobileActionResult backgroundApp(int seconds) {
        getDriver().touch().sendAppToBackground(seconds);
        String code = "driver.element().touch().sendAppToBackground(" + seconds + ");";
        return actionResult("backgroundApp", null, "", Map.of("seconds", String.valueOf(seconds)), code, code, false);
    }

    /**
     * Activates an installed app by package or bundle id.
     */
    @Tool(name = "mobile_activate_app", description = "activates an installed app by Android package or iOS bundle id")
    public McpMobileActionResult activateApp(String appId) {
        getDriver().touch().activateAppFromBackground(appId);
        String code = "driver.element().touch().activateAppFromBackground(" + java(appId) + ");";
        return actionResult("activateApp", null, "", Map.of("appId", text(appId)), code, code, false);
    }

    private void replay(McpMobileRecordedAction action) {
        locatorStrategy strategy = action.locatorStrategy().isBlank()
                ? null
                : locatorStrategy.valueOf(action.locatorStrategy());
        Map<String, String> params = action.parameters();
        switch (action.action()) {
            case "tap" -> getDriver().touch().tap(getLocator(strategy, action.locatorValue()));
            case "doubleTap" -> getDriver().touch().doubleTap(getLocator(strategy, action.locatorValue()));
            case "longTap" -> getDriver().touch().longTap(getLocator(strategy, action.locatorValue()));
            case "type" -> {
                requireSensitive(action);
                getDriver().element().type(getLocator(strategy, action.locatorValue()), params.get("value"));
            }
            case "clear" -> getDriver().element().clear(getLocator(strategy, action.locatorValue()));
            case "swipeByOffset" -> getDriver().touch().swipeByOffset(getLocator(strategy, action.locatorValue()),
                    integer(params, "xOffset"), integer(params, "yOffset"));
            case "swipeElementIntoView" -> getDriver().touch().swipeElementIntoView(
                    getLocator(strategy, action.locatorValue()),
                    TouchActions.SwipeDirection.valueOf(params.get("direction")));
            case "swipeTextIntoView" -> getDriver().touch().swipeElementIntoView(
                    params.get("targetText"),
                    TouchActions.SwipeMovement.valueOf(params.get("movement")));
            case "tapCoordinates" -> performTapCoordinates(integer(params, "x"), integer(params, "y"));
            case "swipeCoordinates" -> performSwipeCoordinates(
                    integer(params, "startX"),
                    integer(params, "startY"),
                    integer(params, "endX"),
                    integer(params, "endY"),
                    integer(params, "durationMillis"));
            case "rotate" -> getDriver().touch().rotate(ScreenOrientation.valueOf(params.get("orientation")));
            case "hideKeyboard" -> getDriver().touch().hideNativeKeyboard();
            case "keyboardKey" -> getDriver().touch().nativeKeyboardKeyPress(
                    TouchActions.KeyboardKeys.valueOf(params.get("key")));
            case "backgroundApp" -> getDriver().touch().sendAppToBackground(integer(params, "seconds"));
            case "activateApp" -> getDriver().touch().activateAppFromBackground(params.get("appId"));
            default -> throw new IllegalArgumentException("Unsupported mobile recording action: " + action.action());
        }
    }

    private McpMobileActionResult locatorAction(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            LocatorOperation operation,
            String javaCode,
            boolean sensitive) {
        try {
            operation.execute(getLocator(locatorStrategy, locatorValue));
            return actionResult(action, locatorStrategy, locatorValue, Map.of(), javaCode, javaCode, sensitive);
        } catch (Exception exception) {
            logger.error("Mobile action failed (action: {}, locator redacted)", action, exception);
            throw exception;
        }
    }

    private McpMobileActionResult actionResult(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            Map<String, String> parameters,
            String javaCode,
            String redactedJavaCode,
            boolean sensitive) {
        McpMobileRecordedAction recorded = recorder.record(
                action,
                locatorStrategy,
                locatorValue,
                parameters,
                javaCode,
                redactedJavaCode,
                sensitive);
        List<String> warnings = actionWarnings(action, locatorStrategy, recorded,
                "Ignored: recording is not active — call capture_start (with a mobile engine active) to "
                        + "capture this step.");
        return new McpMobileActionResult(action, recorded != null, actionBlock(action, javaCode),
                warnings);
    }

    private static List<String> actionWarnings(
            String action,
            locatorStrategy locatorStrategy,
            McpMobileRecordedAction recorded,
            String notRecordedWarning) {
        LinkedHashSet<String> warnings = new LinkedHashSet<>();
        if (McpAppiumLocatorSuggester.isCoordinateFallback(action, locatorStrategy)) {
            warnings.add(McpAppiumLocatorSuggester.COORDINATE_FALLBACK_WARNING);
        }
        if (recorded == null) {
            warnings.add(notRecordedWarning);
        } else {
            warnings.addAll(recorded.warnings());
        }
        return List.copyOf(warnings);
    }

    private static String safeContext(SupportsContextSwitching contextDriver) {
        try {
            return text(contextDriver.getContext());
        } catch (RuntimeException ignored) {
            return "unsupported by active provider";
        }
    }

    private static Map<String, String> mobileDeviceProfile(String browserName) {
        try {
            return MobileTraceMetadata.mcpDeviceProfile(getDriver().getDriver(), browserName);
        } catch (RuntimeException exception) {
            return MobileTraceMetadata.mcpDeviceProfile(browserName);
        }
    }

    private static void recordContextSwitch(WebDriver driver, String requestedContext, String contextBefore,
                                            String contextAfter, RuntimeException exception) {
        Map<String, String> metadata = new java.util.LinkedHashMap<>(MobileTraceMetadata.mobileMetadata(
                driver, exception != null));
        metadata.put("contextBefore", contextBefore);
        metadata.put("contextAfter", text(contextAfter).isBlank() ? "unavailable" : contextAfter);
        metadata.put("requestedContext", text(requestedContext));
        TraceEventRecorder.record("mobile-context", "MOBILE_SWITCH_CONTEXT", exception == null ? "passed" : "failed",
                requestedContext, driver, "Switch MCP mobile context to \"" + requestedContext + "\"",
                exception, metadata, List.of());
    }

    private Path writeScreenshot(String outputPath, byte[] png) {
        if (outputPath == null || outputPath.isBlank()) {
            return null;
        }
        Path resolved = workspacePolicy.output(outputPath, "Mobile screenshot output path");
        try {
            Path parent = resolved.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            return Files.write(resolved, png);
        } catch (IOException exception) {
            throw new IllegalArgumentException(
                    "Mobile screenshot output path cannot be written inside the MCP workspace.", exception);
        }
    }

    private static void performTapCoordinates(int x, int y) {
        PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
        Sequence tap = new Sequence(finger, 0);
        tap.addAction(finger.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), x, y));
        tap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
        tap.addAction(new Pause(finger, Duration.ofMillis(100)));
        tap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
        ((RemoteWebDriver) getDriver().getDriver()).perform(List.of(tap));
    }

    private static void performSwipeCoordinates(int startX, int startY, int endX, int endY, int durationMillis) {
        PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
        Sequence swipe = new Sequence(finger, 0);
        swipe.addAction(finger.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), startX, startY));
        swipe.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
        swipe.addAction(finger.createPointerMove(
                Duration.ofMillis(Math.max(durationMillis, 100)),
                PointerInput.Origin.viewport(),
                endX,
                endY));
        swipe.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
        ((RemoteWebDriver) getDriver().getDriver()).perform(List.of(swipe));
    }

    private static McpCodeBlock actionBlock(String action, String javaCode) {
        return new McpCodeBlock(
                "mobile-action-" + action,
                "Mobile action snippet",
                McpCodeBlock.Kind.ACTION,
                "java",
                List.of(
                        "com.shaft.driver.SHAFT"),
                javaCode + System.lineSeparator(),
                "Paste inside a method that already owns a SHAFT.GUI.WebDriver named driver.",
                true,
                List.of(),
                List.of());
    }

    private static McpCodeBlock webEmulationSetupBlock(
            String targetUrl,
            BrowserType browserType,
            String deviceName,
            int width,
            int height,
            double pixelRatio,
            String userAgent,
            boolean headless) {
        boolean custom = width > 0 && height > 0;
        StringBuilder code = new StringBuilder();
        code.append("SHAFT.Properties.platform.set().targetPlatform(\"Linux\");\n");
        code.append("SHAFT.Properties.mobile.set().browserName(\"\");\n");
        code.append("SHAFT.Properties.web.set()\n");
        code.append("        .targetBrowserName(\"").append(browserType.name()).append("\")\n");
        code.append("        .headlessExecution(").append(headless).append(")\n");
        code.append("        .isMobileEmulation(true)\n");
        code.append("        .mobileEmulationIsCustomDevice(").append(custom).append(")\n");
        if (custom) {
            code.append("        .mobileEmulationWidth(").append(width).append(")\n");
            code.append("        .mobileEmulationHeight(").append(height).append(")\n");
            code.append("        .mobileEmulationPixelRatio(").append(pixelRatio > 0 ? pixelRatio : 1.0).append(")\n");
        } else {
            code.append("        .mobileEmulationDeviceName(").append(java(deviceName)).append(")\n");
        }
        code.append("        .mobileEmulationUserAgent(").append(java(userAgent)).append(");\n");
        code.append("SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();\n");
        if (!text(targetUrl).isBlank()) {
            code.append("driver.browser().navigateToURL(").append(java(targetUrl)).append(");\n");
        }
        return new McpCodeBlock(
                "mobile-web-emulation-setup",
                "Mobile web emulation setup",
                McpCodeBlock.Kind.SETUP,
                "java",
                List.of("com.shaft.driver.SHAFT"),
                code.toString(),
                "Paste into setup before mobile web emulation actions.",
                true,
                List.of(),
                List.of());
    }

    private static McpCodeBlock nativeSetupBlock(
            String platform,
            String deviceName,
            String server,
            String automation,
            String platformVersion,
            String udid,
            String app,
            String appPackage,
            String appActivity,
            String bundleId) {
        StringBuilder code = new StringBuilder();
        code.append("SHAFT.Properties.platform.set()\n");
        code.append("        .targetPlatform(").append(java(platform)).append(")\n");
        code.append("        .executionAddress(").append(java(server)).append(");\n");
        code.append("SHAFT.Properties.web.set().isMobileEmulation(false);\n");
        code.append("SHAFT.Properties.mobile.set()\n");
        code.append("        .platformName(").append(java(platform)).append(")\n");
        code.append("        .automationName(").append(java(automation)).append(")\n");
        code.append("        .deviceName(").append(java(deviceName)).append(")\n");
        code.append("        .platformVersion(").append(java(platformVersion)).append(")\n");
        code.append("        .udid(").append(java(udid)).append(")\n");
        code.append("        .browserName(\"\")\n");
        code.append("        .app(").append(java(app)).append(")\n");
        code.append("        .appPackage(").append(java(appPackage)).append(")\n");
        code.append("        .appActivity(").append(java(appActivity)).append(")\n");
        code.append("        .bundleId(").append(java(bundleId)).append(");\n");
        code.append("SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();\n");
        return new McpCodeBlock(
                "mobile-native-setup",
                "Native mobile Appium setup",
                McpCodeBlock.Kind.SETUP,
                "java",
                List.of("com.shaft.driver.SHAFT"),
                code.toString(),
                "Paste into setup before native mobile actions.",
                true,
                List.of(),
                nativeWarnings(platform, app, appPackage, appActivity, bundleId));
    }

    private static String locatorCode(locatorStrategy strategy, String value) {
        return McpMobileCode.locatorCode(strategy, value);
    }

    private static String tapCoordinatesCode(int x, int y) {
        return McpMobileCode.tapCoordinatesCode(x, y);
    }

    private static String swipeCoordinatesCode(int startX, int startY, int endX, int endY, int durationMillis) {
        return McpMobileCode.swipeCoordinatesCode(startX, startY, endX, endY, durationMillis);
    }

    private static List<String> nativeWarnings(
            String platform,
            String app,
            String appPackage,
            String appActivity,
            String bundleId) {
        List<String> warnings = new ArrayList<>();
        if ("Android".equals(platform) && text(app).isBlank()
                && (text(appPackage).isBlank() || text(appActivity).isBlank())) {
            warnings.add("For installed Android apps, provide appPackage and appActivity when app is blank.");
        }
        if ("iOS".equals(platform) && text(app).isBlank() && text(bundleId).isBlank()) {
            warnings.add("For installed iOS apps, provide bundleId when app is blank.");
        }
        return List.copyOf(warnings);
    }

    private static BrowserType browserType(String browser) {
        if (browser == null || browser.isBlank()) {
            return BrowserType.CHROME;
        }
        BrowserType browserType = BrowserType.valueOf(browser.trim().toUpperCase(Locale.ROOT));
        if (browserType != BrowserType.CHROME && browserType != BrowserType.EDGE) {
            throw new IllegalArgumentException("Mobile web emulation supports CHROME or EDGE.");
        }
        return browserType;
    }

    private static String platform(String platformName) {
        String platform = text(platformName).toLowerCase(Locale.ROOT);
        return switch (platform) {
            case "android" -> "Android";
            case "ios" -> "iOS";
            default -> throw new IllegalArgumentException("platformName must be Android or iOS.");
        };
    }

    private static <T extends Enum<T>> T enumValue(Class<T> type, String value, T fallback) {
        if (value == null || value.isBlank()) {
            return fallback;
        }
        return Enum.valueOf(type, value.trim().toUpperCase(Locale.ROOT));
    }

    private static int integer(Map<String, String> values, String key) {
        return Integer.parseInt(values.get(key));
    }

    private static void requireSensitive(McpMobileRecordedAction action) {
        if (!action.sensitiveValueStored()) {
            throw new IllegalArgumentException("Recording action `" + action.action()
                    + "` omitted sensitive values and cannot be replayed without manual replacement.");
        }
    }

    private static String textOrDefault(String value, String fallback) {
        String text = text(value);
        return text.isBlank() ? fallback : text;
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static String java(String value) {
        String text = value == null ? "" : value;
        return "\"" + text
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t") + "\"";
    }

    @FunctionalInterface
    private interface LocatorOperation {
        void execute(By locator);
    }
}
