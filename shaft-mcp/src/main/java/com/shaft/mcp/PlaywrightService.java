package com.shaft.mcp;

import com.microsoft.playwright.Page;
import com.shaft.driver.SHAFT;
import com.shaft.gui.driver.ShaftLocator;
import org.openqa.selenium.By;
import org.openqa.selenium.WindowType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.List;
import java.util.Map;

import static com.shaft.mcp.EngineService.getLocator;

/**
 * MCP-safe Playwright tools backed by {@link SHAFT.GUI.Playwright}.
 */
@Service
public class PlaywrightService {
    private static final int DEFAULT_DOM_CHARACTER_LIMIT = 200_000;
    private static final Logger logger = LoggerFactory.getLogger(PlaywrightService.class);

    private final McpWorkspacePolicy workspacePolicy;
    private final McpPlaywrightRecordingService recorder;
    private SHAFT.GUI.Playwright driver;

    /**
     * Creates the default Playwright MCP service.
     */
    public PlaywrightService() {
        this(McpWorkspacePolicy.current());
    }

    PlaywrightService(McpWorkspacePolicy workspacePolicy) {
        this.workspacePolicy = workspacePolicy;
        this.recorder = new McpPlaywrightRecordingService(workspacePolicy);
    }

    /**
     * Initializes a SHAFT Playwright browser session.
     *
     * @param browser chromium, chrome, firefox, webkit, or safari; blank uses SHAFT defaults
     * @param headless whether to launch headlessly
     */
    @Tool(name = "playwright_initialize", description = "launches a SHAFT Playwright browser")
    public void initialize(String browser, boolean headless) {
        EngineService.ensureEngineInitialized();
        SHAFT.Properties.web.set().headlessExecution(headless);
        if (browser != null && !browser.isBlank()) {
            SHAFT.Properties.playwright.set().browserName(browser.trim());
        }
        quit();
        driver = new SHAFT.GUI.Playwright();
        logger.info("Playwright driver initialized (browser length: {}, headless: {})",
                browser == null ? 0 : browser.length(), headless);
    }

    /**
     * Quits the active SHAFT Playwright session.
     */
    @Tool(name = "playwright_quit", description = "closes the active SHAFT Playwright browser")
    public void quit() {
        if (driver != null) {
            driver.quit();
            driver = null;
        }
    }

    /**
     * Starts recording Playwright actions performed through MCP Playwright tools.
     *
     * @param outputPath workspace-contained recording JSON output path
     * @param mode recording label
     * @param includeSensitiveValues whether typed values may be persisted
     * @return recorder status
     */
    @Tool(name = "playwright_record_start",
            description = "starts recording MCP Playwright actions to a workspace JSON file")
    public McpMobileRecordingStatus recordStart(String outputPath, String mode, boolean includeSensitiveValues) {
        return recorder.start(outputPath, mode, includeSensitiveValues);
    }

    /**
     * Returns Playwright recording status.
     *
     * @return recorder status
     */
    @Tool(name = "playwright_record_status", description = "returns the active MCP Playwright recording status")
    public McpMobileRecordingStatus recordStatus() {
        return recorder.status();
    }

    /**
     * Stops Playwright recording.
     *
     * @param discard whether to delete the recording output
     * @return final recorder status
     */
    @Tool(name = "playwright_record_stop",
            description = "stops MCP Playwright recording and optionally discards the JSON file")
    public McpMobileRecordingStatus recordStop(boolean discard) {
        return recorder.stop(discard);
    }

    /**
     * Generates copy-paste replay code from a Playwright recording.
     *
     * @param recordingPath workspace-contained recording path
     * @param driverVariableName driver variable name to use in generated snippets
     * @return replay code blocks
     */
    @Tool(name = "playwright_recording_code_blocks",
            description = "generates reusable copy-paste SHAFT Playwright replay code blocks")
    public McpMobileReplayResult recordingCodeBlocks(String recordingPath, String driverVariableName) {
        return recorder.codeBlocks(recordingPath, driverVariableName);
    }

    /**
     * Replays a Playwright recording against the active Playwright session.
     *
     * @param recordingPath workspace-contained recording path
     * @param driverVariableName driver variable name to use in generated snippets
     * @return replay result
     */
    @Tool(name = "playwright_replay_recording",
            description = "replays an MCP Playwright recording against the active SHAFT Playwright driver")
    public McpMobileReplayResult replayRecording(String recordingPath, String driverVariableName) {
        McpMobileRecording recording = recorder.readRecording(recordingPath);
        int replayed = 0;
        for (McpMobileRecordedAction action : recording.actions()) {
            if (!action.sensitiveValueStored()) {
                continue;
            }
            executeRecorded(action);
            replayed++;
        }
        McpMobileReplayResult blocks = recorder.codeBlocks(recordingPath, driverVariableName);
        return new McpMobileReplayResult(blocks.recordingPath(), true, replayed,
                blocks.codeBlocks(), blocks.warnings());
    }

    /**
     * Navigates the Playwright page to a URL.
     *
     * @param targetUrl target URL
     * @return recorded action metadata
     */
    @Tool(name = "playwright_browser_navigate", description = "navigates the SHAFT Playwright page to a URL")
    public McpMobileActionResult navigate(String targetUrl) {
        getDriver().browser().navigateToURL(targetUrl);
        return record("navigate", null, "", Map.of("url", text(targetUrl)),
                "driver.browser().navigateToURL(\"" + javaString(targetUrl) + "\");", false);
    }

    /**
     * Refreshes the Playwright page.
     *
     * @return recorded action metadata
     */
    @Tool(name = "playwright_browser_refresh", description = "refreshes the SHAFT Playwright page")
    public McpMobileActionResult refresh() {
        getDriver().browser().refreshCurrentPage();
        return record("refresh", null, "", Map.of(),
                "driver.browser().refreshCurrentPage();", false);
    }

    /**
     * Navigates back in the Playwright page history.
     *
     * @return recorded action metadata
     */
    @Tool(name = "playwright_browser_navigate_back", description = "navigates the SHAFT Playwright page back")
    public McpMobileActionResult navigateBack() {
        getDriver().browser().navigateBack();
        return record("navigate_back", null, "", Map.of(),
                "driver.browser().navigateBack();", false);
    }

    /**
     * Navigates forward in the Playwright page history.
     *
     * @return recorded action metadata
     */
    @Tool(name = "playwright_browser_navigate_forward", description = "navigates the SHAFT Playwright page forward")
    public McpMobileActionResult navigateForward() {
        getDriver().browser().navigateForward();
        return record("navigate_forward", null, "", Map.of(),
                "driver.browser().navigateForward();", false);
    }

    /**
     * Sets the Playwright viewport size.
     *
     * @param width viewport width
     * @param height viewport height
     * @return recorded action metadata
     */
    @Tool(name = "playwright_browser_set_window_size", description = "sets the SHAFT Playwright viewport size")
    public McpMobileActionResult setWindowSize(int width, int height) {
        getDriver().browser().setWindowSize(width, height);
        return record("set_window_size", null, "", Map.of(
                        "width", String.valueOf(width),
                        "height", String.valueOf(height)),
                "driver.browser().setWindowSize(" + width + ", " + height + ");", false);
    }

    /**
     * Opens a new Playwright tab or window.
     *
     * @param targetUrl optional URL; blank opens about:blank
     * @param windowType TAB or WINDOW
     * @return recorded action metadata
     */
    @Tool(name = "playwright_browser_new_window", description = "opens a new SHAFT Playwright tab or window")
    public McpMobileActionResult newWindow(String targetUrl, String windowType) {
        WindowType type = "WINDOW".equalsIgnoreCase(windowType) ? WindowType.WINDOW : WindowType.TAB;
        String url = targetUrl == null || targetUrl.isBlank() ? "about:blank" : targetUrl;
        getDriver().browser().navigateToURL(url, type);
        return record("new_window", null, "", Map.of("url", url, "windowType", type.name()),
                "driver.browser().navigateToURL(\"" + javaString(url) + "\", WindowType." + type.name() + ");",
                false);
    }

    /**
     * Gets current Playwright page URL.
     *
     * @return current URL
     */
    @Tool(name = "playwright_browser_get_current_url", description = "gets current SHAFT Playwright page URL")
    public String getCurrentUrl() {
        return getDriver().browser().getCurrentURL();
    }

    /**
     * Gets current Playwright page title.
     *
     * @return page title
     */
    @Tool(name = "playwright_browser_get_title", description = "gets current SHAFT Playwright page title")
    public String getTitle() {
        return getDriver().browser().getCurrentWindowTitle();
    }

    /**
     * Captures the current Playwright page DOM.
     *
     * @param maxCharacters maximum DOM characters to return
     * @return page DOM snapshot
     */
    @Tool(name = "playwright_browser_get_page_dom",
            description = "gets the current SHAFT Playwright page DOM for browser automation inspection")
    public McpPageDomSnapshot getPageDom(int maxCharacters) {
        Page page = getDriver().getDriver();
        int limit = maxCharacters <= 0 ? DEFAULT_DOM_CHARACTER_LIMIT : maxCharacters;
        String dom = page.content();
        String safeDom = dom == null ? "" : dom;
        boolean truncated = safeDom.length() > limit;
        String returnedDom = truncated ? safeDom.substring(0, limit) : safeDom;
        return new McpPageDomSnapshot(
                page.url(),
                page.title(),
                returnedDom,
                safeDom.length(),
                truncated,
                truncated ? List.of("DOM was truncated; increase maxCharacters for more context.") : List.of());
    }

    /**
     * Takes a PNG screenshot of the current Playwright page.
     *
     * @param outputPath optional workspace-contained output file path
     * @param includeBase64 whether to include base64 bytes in the response
     * @return screenshot metadata
     */
    @Tool(name = "playwright_browser_take_screenshot",
            description = "takes a PNG screenshot of the current SHAFT Playwright page")
    public McpScreenshotResult takeScreenshot(String outputPath, boolean includeBase64) {
        byte[] png = getDriver().getDriver().screenshot(new Page.ScreenshotOptions());
        Path writtenPath = writeScreenshot(outputPath, png);
        return new McpScreenshotResult(
                "image/png",
                png.length,
                includeBase64 ? Base64.getEncoder().encodeToString(png) : null,
                writtenPath == null ? null : writtenPath.toString(),
                includeBase64 ? List.of() : List.of("Base64 omitted; set includeBase64=true to return inline PNG bytes."));
    }

    /**
     * Clicks a Playwright element.
     */
    @Tool(name = "playwright_element_click", description = "clicks an element using SHAFT Playwright")
    public McpMobileActionResult click(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().click(locator));
        return recordLocator("click", locatorStrategy, locatorValue,
                "driver.element().click(" + locatorCode(locatorStrategy, locatorValue) + ");", false);
    }

    /**
     * Clicks a Playwright element using JavaScript.
     */
    @Tool(name = "playwright_element_click_js", description = "clicks an element using JavaScript in SHAFT Playwright")
    public McpMobileActionResult clickUsingJavaScript(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().clickUsingJavascript(locator));
        return recordLocator("click_js", locatorStrategy, locatorValue,
                "driver.element().clickUsingJavascript(" + locatorCode(locatorStrategy, locatorValue) + ");", false);
    }

    /**
     * Double-clicks a Playwright element.
     */
    @Tool(name = "playwright_element_double_click", description = "double-clicks an element using SHAFT Playwright")
    public McpMobileActionResult doubleClick(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().doubleClick(locator));
        return recordLocator("double_click", locatorStrategy, locatorValue,
                "driver.element().doubleClick(" + locatorCode(locatorStrategy, locatorValue) + ");", false);
    }

    /**
     * Hovers over a Playwright element.
     */
    @Tool(name = "playwright_element_hover", description = "hovers over an element using SHAFT Playwright")
    public McpMobileActionResult hover(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().hover(locator));
        return recordLocator("hover", locatorStrategy, locatorValue,
                "driver.element().hover(" + locatorCode(locatorStrategy, locatorValue) + ");", false);
    }

    /**
     * Types into a Playwright element.
     */
    @Tool(name = "playwright_element_type", description = "types value into an element using SHAFT Playwright")
    public McpMobileActionResult type(locatorStrategy locatorStrategy, String locatorValue, String textValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().type(locator, textValue));
        String code = "driver.element().type(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"" + javaString(textValue) + "\");";
        String redacted = "driver.element().type(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"<redacted>\");";
        return recordLocator("type", locatorStrategy, locatorValue, code, redacted, true,
                Map.of("value", text(textValue)));
    }

    /**
     * Appends text to a Playwright element.
     */
    @Tool(name = "playwright_element_append_text",
            description = "appends text to an element using SHAFT Playwright")
    public McpMobileActionResult appendText(locatorStrategy locatorStrategy, String locatorValue, String textValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().typeAppend(locator, textValue));
        String code = "driver.element().typeAppend(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"" + javaString(textValue) + "\");";
        String redacted = "driver.element().typeAppend(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"<redacted>\");";
        return recordLocator("append_text", locatorStrategy, locatorValue, code, redacted, true,
                Map.of("value", text(textValue)));
    }

    /**
     * Sets an element value using JavaScript.
     */
    @Tool(name = "playwright_element_set_value_js",
            description = "sets an element value using JavaScript in SHAFT Playwright")
    public McpMobileActionResult setValueUsingJavaScript(
            locatorStrategy locatorStrategy,
            String locatorValue,
            String textValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().setValueUsingJavaScript(locator, textValue));
        String code = "driver.element().setValueUsingJavaScript(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"" + javaString(textValue) + "\");";
        String redacted = "driver.element().setValueUsingJavaScript(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"<redacted>\");";
        return recordLocator("set_value_js", locatorStrategy, locatorValue, code, redacted, true,
                Map.of("value", text(textValue)));
    }

    /**
     * Clears a Playwright element.
     */
    @Tool(name = "playwright_element_clear", description = "clears an element using SHAFT Playwright")
    public McpMobileActionResult clear(locatorStrategy locatorStrategy, String locatorValue) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().clear(locator));
        return recordLocator("clear", locatorStrategy, locatorValue,
                "driver.element().clear(" + locatorCode(locatorStrategy, locatorValue) + ");", false);
    }

    /**
     * Uploads a file through a Playwright file input.
     */
    @Tool(name = "playwright_element_upload_file",
            description = "uploads a file through an element using SHAFT Playwright")
    public McpMobileActionResult uploadFile(locatorStrategy locatorStrategy, String locatorValue, String filePath) {
        withLocator(locatorStrategy, locatorValue, locator -> getDriver().element().typeFileLocationForUpload(locator, filePath));
        String code = "driver.element().typeFileLocationForUpload(" + locatorCode(locatorStrategy, locatorValue)
                + ", \"" + javaString(filePath) + "\");";
        return recordLocator("upload_file", locatorStrategy, locatorValue, code, false,
                Map.of("value", text(filePath)));
    }

    /**
     * Drags one Playwright element to another.
     */
    @Tool(name = "playwright_element_drag_and_drop",
            description = "drags and drops an element using SHAFT Playwright")
    public McpMobileActionResult dragAndDrop(
            locatorStrategy sourceLocatorStrategy,
            String sourceLocatorValue,
            locatorStrategy targetLocatorStrategy,
            String targetLocatorValue) {
        By source = getLocator(sourceLocatorStrategy, sourceLocatorValue);
        By target = getLocator(targetLocatorStrategy, targetLocatorValue);
        getDriver().element().dragAndDrop(source, target);
        String code = "driver.element().dragAndDrop(" + locatorCode(sourceLocatorStrategy, sourceLocatorValue)
                + ", " + locatorCode(targetLocatorStrategy, targetLocatorValue) + ");";
        return record("drag_and_drop", sourceLocatorStrategy, sourceLocatorValue, Map.of(
                        "targetStrategy", targetLocatorStrategy.name(),
                        "targetValue", targetLocatorValue),
                code, false);
    }

    /**
     * Checks Playwright element visibility.
     */
    @Tool(name = "playwright_element_is_displayed",
            description = "checks whether a SHAFT Playwright element is visible")
    public boolean isDisplayed(locatorStrategy locatorStrategy, String locatorValue) {
        return locator(locatorStrategy, locatorValue).isVisible();
    }

    /**
     * Checks Playwright element enabled state.
     */
    @Tool(name = "playwright_element_is_enabled",
            description = "checks whether a SHAFT Playwright element is enabled")
    public boolean isEnabled(locatorStrategy locatorStrategy, String locatorValue) {
        return locator(locatorStrategy, locatorValue).isEnabled();
    }

    private SHAFT.GUI.Playwright getDriver() {
        if (driver == null) {
            throw new IllegalStateException("No active Playwright browser session");
        }
        return driver;
    }

    private void executeRecorded(McpMobileRecordedAction action) {
        locatorStrategy strategy = action.locatorStrategy().isBlank()
                ? null
                : locatorStrategy.valueOf(action.locatorStrategy());
        String locatorValue = action.locatorValue();
        Map<String, String> parameters = action.parameters();
        switch (action.action()) {
            case "navigate" -> getDriver().browser().navigateToURL(parameters.get("url"));
            case "refresh" -> getDriver().browser().refreshCurrentPage();
            case "navigate_back" -> getDriver().browser().navigateBack();
            case "navigate_forward" -> getDriver().browser().navigateForward();
            case "set_window_size" -> getDriver().browser().setWindowSize(
                    Integer.parseInt(parameters.get("width")),
                    Integer.parseInt(parameters.get("height")));
            case "new_window" -> getDriver().browser().navigateToURL(
                    parameters.get("url"),
                    "WINDOW".equals(parameters.get("windowType")) ? WindowType.WINDOW : WindowType.TAB);
            case "click" -> getDriver().element().click(getLocator(strategy, locatorValue));
            case "click_js" -> getDriver().element().clickUsingJavascript(getLocator(strategy, locatorValue));
            case "double_click" -> getDriver().element().doubleClick(getLocator(strategy, locatorValue));
            case "hover" -> getDriver().element().hover(getLocator(strategy, locatorValue));
            case "type" -> getDriver().element().type(getLocator(strategy, locatorValue), parameters.get("value"));
            case "append_text" -> getDriver().element().typeAppend(getLocator(strategy, locatorValue), parameters.get("value"));
            case "set_value_js" -> getDriver().element().setValueUsingJavaScript(
                    getLocator(strategy, locatorValue), parameters.get("value"));
            case "clear" -> getDriver().element().clear(getLocator(strategy, locatorValue));
            case "upload_file" -> getDriver().element().typeFileLocationForUpload(
                    getLocator(strategy, locatorValue), parameters.get("value"));
            case "drag_and_drop" -> getDriver().element().dragAndDrop(
                    getLocator(strategy, locatorValue),
                    getLocator(locatorStrategy.valueOf(parameters.get("targetStrategy")), parameters.get("targetValue")));
            default -> throw new IllegalArgumentException("Unsupported Playwright recording action: " + action.action());
        }
    }

    private McpMobileActionResult recordLocator(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            String javaCode,
            boolean sensitive) {
        return record(action, locatorStrategy, locatorValue, Map.of(), javaCode, sensitive);
    }

    private McpMobileActionResult recordLocator(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            String javaCode,
            boolean sensitive,
            Map<String, String> parameters) {
        return record(action, locatorStrategy, locatorValue, parameters, javaCode, sensitive);
    }

    private McpMobileActionResult recordLocator(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            String javaCode,
            String redactedJavaCode,
            boolean sensitive,
            Map<String, String> parameters) {
        return record(action, locatorStrategy, locatorValue, parameters, javaCode, redactedJavaCode, sensitive);
    }

    private McpMobileActionResult record(
            String action,
            locatorStrategy locatorStrategy,
            String locatorValue,
            Map<String, String> parameters,
            String javaCode,
            boolean sensitive) {
        return record(action, locatorStrategy, locatorValue, parameters, javaCode, javaCode, sensitive);
    }

    private McpMobileActionResult record(
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
                recorded == null ? List.of("Action was not recorded; call playwright_record_start to capture it.")
                        : recorded.warnings());
    }

    private McpCodeBlock actionBlock(String action, String javaCode) {
        return new McpCodeBlock(
                "playwright-action-" + action,
                "SHAFT Playwright action",
                McpCodeBlock.Kind.TEST_METHOD,
                "java",
                List.of("com.shaft.driver.SHAFT", "org.openqa.selenium.By", "org.openqa.selenium.WindowType"),
                javaCode,
                "Paste inside a method that owns an initialized SHAFT.GUI.Playwright named driver.",
                true,
                List.of(),
                List.of());
    }

    private void withLocator(locatorStrategy locatorStrategy, String locatorValue, LocatorOperation operation) {
        operation.execute(getLocator(locatorStrategy, locatorValue));
        logger.info("Playwright element operation completed (strategy: {}, locator length: {})",
                locatorStrategy, locatorValue == null ? 0 : locatorValue.length());
    }

    private com.microsoft.playwright.Locator locator(locatorStrategy locatorStrategy, String locatorValue) {
        return ShaftLocator.from(getLocator(locatorStrategy, locatorValue)).toPlaywrightLocator(getDriver().getDriver());
    }

    private Path writeScreenshot(String outputPath, byte[] png) {
        if (outputPath == null || outputPath.isBlank()) {
            return null;
        }
        Path resolved = workspacePolicy.output(outputPath, "Playwright screenshot output path");
        try {
            Path parent = resolved.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            return Files.write(resolved, png);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Playwright screenshot output path cannot be written inside the MCP workspace.",
                    exception);
        }
    }

    private static String locatorCode(locatorStrategy strategy, String value) {
        return switch (strategy) {
            case ID -> "SHAFT.GUI.Locator.hasAnyTagName().hasId(\"" + javaString(value) + "\").build()";
            case CSS, CSSSELECTOR, SELECTOR -> "By.cssSelector(\"" + javaString(value) + "\")";
            case XPATH -> "By.xpath(\"" + javaString(value) + "\")";
            case NAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"name\", \""
                    + javaString(value) + "\").build()";
            case TAGNAME -> "SHAFT.GUI.Locator.hasTagName(\"" + javaString(value) + "\").build()";
            case CLASSNAME -> "SHAFT.GUI.Locator.hasAnyTagName().hasAttribute(\"class\", \""
                    + javaString(value) + "\").build()";
            default -> throw new IllegalArgumentException("Locator strategy is not supported by Playwright tools: " + strategy);
        };
    }

    private static String text(String value) {
        return value == null ? "" : value;
    }

    private static String javaString(String value) {
        return text(value).replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t");
    }

    @FunctionalInterface
    private interface LocatorOperation {
        void execute(By locator);
    }
}
