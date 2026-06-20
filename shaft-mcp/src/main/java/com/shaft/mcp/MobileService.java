package com.shaft.mcp;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.TouchActions;
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
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;
import java.util.ArrayList;
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

    private final EngineService engineService;
    private final McpMobileRecordingService recorder;
    private final McpMobileInspectorRecordingService inspectorRecorder;
    private final McpWorkspacePolicy workspacePolicy;

    @Autowired
    public MobileService(EngineService engineService) {
        this(engineService, McpWorkspacePolicy.current());
    }

    MobileService(EngineService engineService, McpWorkspacePolicy workspacePolicy) {
        this(engineService, new McpMobileRecordingService(workspacePolicy), workspacePolicy);
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
            McpMobileInspectorRecordingService inspectorRecorder) {
        this.engineService = engineService;
        this.recorder = recorder;
        this.workspacePolicy = workspacePolicy;
        this.inspectorRecorder = inspectorRecorder;
    }

    /**
     * Starts a local browser in Chrome/Edge mobile emulation mode.
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
    @Tool(name = "mobile_initialize_web_emulation",
            description = "starts Chrome or Edge mobile web emulation using SHAFT browser setup")
    public McpMobileSessionResult initializeWebEmulation(
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
        engineService.initializeConfiguredDriver("mobile web emulation");
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
                customDevice ? List.of() : List.of("Device names must match Chrome/Edge DevTools emulation names."));
    }

    /**
     * Starts an Appium native Android or iOS session through SHAFT driver setup.
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
    @Tool(name = "mobile_initialize_native",
            description = "starts a real Android or iOS native Appium session using SHAFT driver setup")
    public McpMobileSessionResult initializeNative(
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
        engineService.initializeConfiguredDriver("mobile native " + platform);
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
    @Tool(name = "mobile_record_start",
            description = "starts recording MCP mobile actions to a workspace JSON file")
    public McpMobileRecordingStatus recordStart(String outputPath, String mode, boolean includeSensitiveValues) {
        return recorder.start(outputPath, mode, includeSensitiveValues);
    }

    /**
     * Returns mobile recording status.
     *
     * @return recorder status
     */
    @Tool(name = "mobile_record_status",
            description = "returns the active MCP mobile recording status")
    public McpMobileRecordingStatus recordStatus() {
        return recorder.status();
    }

    /**
     * Stops mobile recording.
     *
     * @param discard whether to delete the recording output
     * @return final recorder status
     */
    @Tool(name = "mobile_record_stop",
            description = "stops MCP mobile recording and optionally discards the JSON file")
    public McpMobileRecordingStatus recordStop(boolean discard) {
        return recorder.stop(discard);
    }

    /**
     * Returns local Appium/Android/iOS toolchain discovery status.
     *
     * @param platformName Android or iOS; blank defaults to Android
     * @return local mobile toolchain status
     */
    @Tool(name = "mobile_toolchain_status",
            description = "checks local Appium, Inspector, adb, emulator, and SDK tooling status")
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
    @Tool(name = "mobile_inspector_record_prepare",
            description = "prepares a wrapped Appium Inspector mobile recording plan with device and dependency details")
    public McpMobileInspectorPlan inspectorRecordPrepare(
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
     * Starts a previously prepared wrapped Appium Inspector recording session.
     *
     * @param confirmationToken token returned by mobile_inspector_record_prepare
     * @param selectedAndroidAvdName optional cached Android AVD override
     * @param openInspector whether to open the wrapped Inspector URL in the user's browser
     * @return active recording status and Inspector URL
     */
    @Tool(name = "mobile_inspector_record_start",
            description = "starts a confirmed wrapped Appium Inspector recording session")
    public McpMobileInspectorRecordingStatus inspectorRecordStart(
            String confirmationToken,
            String selectedAndroidAvdName,
            boolean openInspector) {
        return inspectorRecorder.start(confirmationToken, selectedAndroidAvdName, openInspector);
    }

    /**
     * Returns wrapped Appium Inspector recording status.
     *
     * @return current Inspector recording status
     */
    @Tool(name = "mobile_inspector_record_status",
            description = "returns the wrapped Appium Inspector recording status")
    public McpMobileInspectorRecordingStatus inspectorRecordStatus() {
        return inspectorRecorder.status();
    }

    /**
     * Controls a wrapped Appium Inspector recording.
     *
     * @param action status, pause, resume, checkpoint, stop, or discard
     * @param checkpointName optional checkpoint name
     * @return updated recording status
     */
    @Tool(name = "mobile_inspector_record_control",
            description = "pauses, resumes, checkpoints, stops, or discards a wrapped Appium Inspector recording")
    public McpMobileInspectorRecordingStatus inspectorRecordControl(String action, String checkpointName) {
        return inspectorRecorder.control(action, checkpointName);
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
    @Tool(name = "mobile_recording_code_blocks",
            description = "generates reusable copy-paste SHAFT mobile replay code blocks")
    public McpMobileReplayResult recordingCodeBlocks(String recordingPath, String driverVariableName) {
        return recorder.codeBlocks(recordingPath, driverVariableName);
    }

    /**
     * Replays a mobile recording against the active driver session.
     *
     * @param recordingPath workspace-contained recording path
     * @param driverVariableName driver variable name to use in generated snippets
     * @return replay result and replay code blocks
     */
    @Tool(name = "mobile_replay_recording",
            description = "replays an MCP mobile recording against the active SHAFT mobile driver")
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
    @Tool(name = "mobile_take_screenshot",
            description = "takes a PNG screenshot of the current mobile device viewport")
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
        contextDriver.context(contextName);
        return getContexts(DEFAULT_SOURCE_CHARACTER_LIMIT);
    }

    /**
     * Taps an element by locator.
     */
    @Tool(name = "mobile_tap", description = "taps a mobile element using SHAFT touch actions")
    public McpMobileActionResult tap(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("tap", locatorStrategy, locatorValue,
                locator -> getDriver().touch().tap(locator),
                "driver.touch().tap(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Double taps an element by locator.
     */
    @Tool(name = "mobile_double_tap", description = "double taps a mobile element using SHAFT touch actions")
    public McpMobileActionResult doubleTap(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("doubleTap", locatorStrategy, locatorValue,
                locator -> getDriver().touch().doubleTap(locator),
                "driver.touch().doubleTap(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Long taps an element by locator.
     */
    @Tool(name = "mobile_long_tap", description = "long taps a mobile element using SHAFT touch actions")
    public McpMobileActionResult longTap(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("longTap", locatorStrategy, locatorValue,
                locator -> getDriver().touch().longTap(locator),
                "driver.touch().longTap(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Types text into a mobile element.
     */
    @Tool(name = "mobile_type", description = "types a value into a mobile element")
    public McpMobileActionResult type(locatorStrategy locatorStrategy, String locatorValue, String textValue) {
        String code = "driver.element().type(" + locatorCode(locatorStrategy, locatorValue)
                + ", " + java(textValue) + ");";
        String redactedCode = "driver.element().type(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"<redacted>\");";
        try {
            getDriver().element().type(getLocator(locatorStrategy, locatorValue), textValue);
            return actionResult("type", locatorStrategy, locatorValue, Map.of("value", text(textValue)), code,
                    redactedCode, true);
        } catch (Exception exception) {
            logger.error("Mobile type failed (values redacted)", exception);
            throw exception;
        }
    }

    /**
     * Clears a mobile element.
     */
    @Tool(name = "mobile_clear", description = "clears a mobile element")
    public McpMobileActionResult clear(locatorStrategy locatorStrategy, String locatorValue) {
        return locatorAction("clear", locatorStrategy, locatorValue,
                locator -> getDriver().element().clear(locator),
                "driver.element().clear(" + locatorCode(locatorStrategy, locatorValue) + ");",
                false);
    }

    /**
     * Swipes one element by an offset.
     */
    @Tool(name = "mobile_swipe_by_offset", description = "swipes a mobile element by x/y offset")
    public McpMobileActionResult swipeByOffset(
            locatorStrategy locatorStrategy,
            String locatorValue,
            int xOffset,
            int yOffset) {
        String code = "driver.touch().swipeByOffset(" + locatorCode(locatorStrategy, locatorValue)
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
    @Tool(name = "mobile_swipe_element_into_view",
            description = "swipes the mobile screen until the target element is visible")
    public McpMobileActionResult swipeElementIntoView(
            locatorStrategy locatorStrategy,
            String locatorValue,
            String direction) {
        TouchActions.SwipeDirection swipeDirection = enumValue(TouchActions.SwipeDirection.class, direction,
                TouchActions.SwipeDirection.DOWN);
        String code = "driver.touch().swipeElementIntoView(" + locatorCode(locatorStrategy, locatorValue)
                + ", TouchActions.SwipeDirection." + swipeDirection.name() + ");";
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
    @Tool(name = "mobile_swipe_text_into_view",
            description = "swipes to text using SHAFT Android UiScrollable support")
    public McpMobileActionResult swipeTextIntoView(String targetText, String movement) {
        TouchActions.SwipeMovement swipeMovement = enumValue(TouchActions.SwipeMovement.class, movement,
                TouchActions.SwipeMovement.VERTICAL);
        String code = "driver.touch().swipeElementIntoView(" + java(targetText)
                + ", TouchActions.SwipeMovement." + swipeMovement.name() + ");";
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
     * Taps screen coordinates using W3C touch actions.
     */
    @Tool(name = "mobile_tap_coordinates", description = "taps viewport coordinates on the active mobile session")
    public McpMobileActionResult tapCoordinates(int x, int y) {
        performTapCoordinates(x, y);
        String code = tapCoordinatesCode(x, y);
        return actionResult("tapCoordinates", null, "", Map.of("x", String.valueOf(x), "y", String.valueOf(y)),
                code, code, false);
    }

    /**
     * Swipes between screen coordinates using W3C touch actions.
     */
    @Tool(name = "mobile_swipe_coordinates", description = "swipes between viewport coordinates on the active mobile session")
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
        String code = "driver.touch().rotate(ScreenOrientation." + target.name() + ");";
        return actionResult("rotate", null, "", Map.of("orientation", target.name()), code, code, false);
    }

    /**
     * Hides the native keyboard.
     */
    @Tool(name = "mobile_hide_keyboard", description = "hides the native mobile keyboard")
    public McpMobileActionResult hideKeyboard() {
        getDriver().touch().hideNativeKeyboard();
        String code = "driver.touch().hideNativeKeyboard();";
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
        String code = "driver.touch().nativeKeyboardKeyPress(TouchActions.KeyboardKeys." + keyboardKey.name() + ");";
        return actionResult("keyboardKey", null, "", Map.of("key", keyboardKey.name()), code, code, false);
    }

    /**
     * Sends the active app to the background.
     */
    @Tool(name = "mobile_background_app", description = "sends the active mobile app to the background")
    public McpMobileActionResult backgroundApp(int seconds) {
        getDriver().touch().sendAppToBackground(seconds);
        String code = "driver.touch().sendAppToBackground(" + seconds + ");";
        return actionResult("backgroundApp", null, "", Map.of("seconds", String.valueOf(seconds)), code, code, false);
    }

    /**
     * Activates an installed app by package or bundle id.
     */
    @Tool(name = "mobile_activate_app", description = "activates an installed app by Android package or iOS bundle id")
    public McpMobileActionResult activateApp(String appId) {
        getDriver().touch().activateAppFromBackground(appId);
        String code = "driver.touch().activateAppFromBackground(" + java(appId) + ");";
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
        return new McpMobileActionResult(action, recorded != null, actionBlock(action, javaCode),
                recorded == null ? List.of("Action was not recorded; call mobile_record_start to capture it.")
                        : recorded.warnings());
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
                        "com.shaft.driver.SHAFT",
                        "com.shaft.gui.element.TouchActions",
                        "io.appium.java_client.AppiumBy",
                        "org.openqa.selenium.By",
                        "org.openqa.selenium.ScreenOrientation",
                        "org.openqa.selenium.interactions.Pause",
                        "org.openqa.selenium.interactions.PointerInput",
                        "org.openqa.selenium.interactions.Sequence",
                        "org.openqa.selenium.remote.RemoteWebDriver",
                        "java.time.Duration",
                        "java.util.List"),
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
        String literal = java(value);
        return switch (strategy) {
            case ID -> "SHAFT.GUI.Locator.hasAnyTagName().hasId(" + literal + ").build()";
            case CSSSELECTOR, CSS, SELECTOR -> "By.cssSelector(" + literal + ")";
            case XPATH -> "By.xpath(" + literal + ")";
            case NAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"name\", " + literal + ").build()";
            case TAGNAME -> "SHAFT.GUI.Locator.hasTagName(" + literal + ").build()";
            case CLASSNAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"class\", " + literal + ").build()";
            case ACCESSIBILITY_ID -> "AppiumBy.accessibilityId(" + literal + ")";
            case ANDROID_UIAUTOMATOR -> "AppiumBy.androidUIAutomator(" + literal + ")";
            case IOS_PREDICATE -> "AppiumBy.iOSNsPredicateString(" + literal + ")";
            case IOS_CLASS_CHAIN -> "AppiumBy.iOSClassChain(" + literal + ")";
        };
    }

    private static String tapCoordinatesCode(int x, int y) {
        return """
                PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
                Sequence tap = new Sequence(finger, 0);
                tap.addAction(finger.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), %d, %d));
                tap.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
                tap.addAction(new Pause(finger, Duration.ofMillis(100)));
                tap.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
                ((RemoteWebDriver) driver.getDriver()).perform(List.of(tap));""".formatted(x, y);
    }

    private static String swipeCoordinatesCode(int startX, int startY, int endX, int endY, int durationMillis) {
        return """
                PointerInput finger = new PointerInput(PointerInput.Kind.TOUCH, "finger");
                Sequence swipe = new Sequence(finger, 0);
                swipe.addAction(finger.createPointerMove(Duration.ZERO, PointerInput.Origin.viewport(), %d, %d));
                swipe.addAction(finger.createPointerDown(PointerInput.MouseButton.LEFT.asArg()));
                swipe.addAction(finger.createPointerMove(Duration.ofMillis(%d), PointerInput.Origin.viewport(), %d, %d));
                swipe.addAction(finger.createPointerUp(PointerInput.MouseButton.LEFT.asArg()));
                ((RemoteWebDriver) driver.getDriver()).perform(List.of(swipe));"""
                .formatted(startX, startY, Math.max(durationMillis, 100), endX, endY);
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
