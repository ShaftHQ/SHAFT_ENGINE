package com.shaft.mcp;

import com.deque.html.axecore.results.CheckedNode;
import com.deque.html.axecore.results.Rule;
import com.shaft.capture.generate.LocatorRanker;
import com.shaft.capture.model.ElementSnapshot;
import com.shaft.capture.model.EventContext;
import com.shaft.capture.model.LocatorCandidate;
import com.shaft.capture.model.PageContext;
import com.shaft.driver.SHAFT;
import com.shaft.validation.accessibility.AccessibilityHelper;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static com.shaft.mcp.EngineService.getDriver;
import static com.shaft.mcp.EngineService.getLocator;

@Service
public class BrowserService {
    private static final int DEFAULT_DOM_CHARACTER_LIMIT = 200_000;
    private static final int DEFAULT_ELEMENT_LIMIT = 10;
    private static final int MAX_ELEMENT_LIMIT = 25;
    private static final int MAX_VIOLATION_NODES = 5;
    private static final int MAX_VIOLATION_NODE_HTML_LENGTH = 200;
    private static final Logger logger = LoggerFactory.getLogger(BrowserService.class);

    private final McpWorkspacePolicy workspacePolicy;

    public BrowserService() {
        this(McpWorkspacePolicy.current());
    }

    BrowserService(McpWorkspacePolicy workspacePolicy) {
        this.workspacePolicy = workspacePolicy;
    }

    /**
     * Navigates the browser to the specified URL.
     *
     * @param targetUrl The URL to navigate to.
     */
    @Tool(name = "browser_navigate", description = "opens a URL in the active SHAFT WebDriver browser session")
    public void navigate(String targetUrl) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().navigateToURL(targetUrl);
            logger.info("Navigated to URL (value redacted)");
        } catch (Exception e) {
            logger.error("Failed to navigate to URL (value redacted)", e);
            throw e;
        }
    }

    /**
     * Navigates the browser to the specified URL using Basic Authentication.
     *
     * @param targetUrl          The URL to navigate to.
     * @param username           The username for Basic Authentication.
     * @param password           The password for Basic Authentication.
     * @param targetUrlAfterAuth The URL to navigate to after authentication.
     */
    public void navigateWithBasicAuth(String targetUrl, String username, String password, String targetUrlAfterAuth) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().navigateToURLWithBasicAuthentication(targetUrl, username, password, targetUrlAfterAuth);
            logger.info("Navigated with Basic Authentication (values redacted)");
        } catch (Exception e) {
            logger.error("Failed to navigate with Basic Authentication (values redacted)", e);
            throw e;
        }
    }

    /**
     * Refreshes the current page in the browser.
     */
    @Tool(name = "browser_refresh", description = "refreshes the current page")
    public void refreshPage() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().refreshCurrentPage();
            logger.info("Page refreshed successfully.");
        } catch (Exception e) {
            logger.error("Failed to refresh the page.", e);
            throw e;
        }
    }

    /**
     * Navigates back to the previous page in the browser's history.
     */
    @Tool(name = "browser_navigate_back", description = "navigates back to the previous page")
    public void navigateBack() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().navigateBack();
            logger.info("Navigated back to the previous page.");
        } catch (Exception e) {
            logger.error("Failed to navigate back.", e);
            throw e;
        }
    }

    /**
     * Navigates forward to the next page in the browser's history.
     */
    @Tool(name = "browser_navigate_forward", description = "navigates forward to the next page")
    public void navigateForward() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().navigateForward();
            logger.info("Navigated forward to the next page.");
        } catch (Exception e) {
            logger.error("Failed to navigate forward.", e);
            throw e;
        }
    }

    /**
     * Maximizes the browser window.
     */
    @Tool(name = "browser_maximize_window", description = "maximizes the browser window")
    public void maximizeWindow() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().maximizeWindow();
            logger.info("Browser window maximized.");
        } catch (Exception e) {
            logger.error("Failed to maximize browser window.", e);
            throw e;
        }
    }

    /**
     * Sets the browser window to a specific size.
     *
     * @param width  The desired width of the browser window.
     * @param height The desired height of the browser window.
     */
    @Tool(name = "browser_set_window_size", description = "sets the browser window to a specific size")
    public void setWindowSize(int width, int height) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().setWindowSize(width, height);
            logger.info("Browser window size set to {}x{}.", width, height);
        } catch (Exception e) {
            logger.error("Failed to set browser window size to {}x{}.", width, height, e);
            throw e;
        }
    }

    /**
     * Sets the browser window to fullscreen mode.
     */
    @Tool(name = "browser_fullscreen_window", description = "sets the browser window to fullscreen mode")
    public void fullscreenWindow() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().fullScreenWindow();
            logger.info("Browser window set to fullscreen mode.");
        } catch (Exception e) {
            logger.error("Failed to set browser window to fullscreen mode.", e);
            throw e;
        }
    }

    /**
     * Deletes all cookies in the current browser session.
     */
    @Tool(name = "browser_delete_all_cookies", description = "deletes all cookies")
    public void deleteAllCookies() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().deleteAllCookies();
            logger.info("All cookies deleted.");
        } catch (Exception e) {
            logger.error("Failed to delete all cookies.", e);
            throw e;
        }
    }

    /**
     * Deletes a specific cookie by name in the current browser session.
     *
     * @param cookieName The name of the cookie to delete.
     */
    @Tool(name = "browser_delete_cookie", description = "deletes a specific cookie by name")
    public void deleteCookie(String cookieName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().deleteCookie(cookieName);
            logger.info("Cookie deleted (name length: {})", cookieName == null ? 0 : cookieName.length());
        } catch (Exception e) {
            logger.error("Failed to delete cookie (name redacted)", e);
            throw e;
        }
    }

    /**
     * Adds a cookie to the current browser session.
     *
     * @param name  The name of the cookie.
     * @param value The value of the cookie.
     */
    public void addCookie(String name, String value) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().addCookie(name, value);
            logger.info("Cookie added (name length: {}, value length: {})",
                    name == null ? 0 : name.length(), value == null ? 0 : value.length());
        } catch (Exception e) {
            logger.error("Failed to add cookie (values redacted)", e);
            throw e;
        }
    }

    /**
     * Retrieves a cookie by name from the current browser session.
     *
     * @param cookieName The name of the cookie to retrieve.
     * @return The cookie value as a string, or null if not found.
     */
    public String getCookie(String cookieName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String cookieValue = driver.browser().getCookie(cookieName).getValue();
            logger.info("Retrieved cookie (name and value redacted)");
            return cookieValue;
        } catch (Exception e) {
            logger.error("Failed to retrieve cookie (name redacted)", e);
            throw e;
        }
    }

    /**
     * Retrieves all cookies from the current browser session.
     *
     * @return A string representation of all cookies.
     */
    public String getAllCookies() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String allCookies = driver.browser().getAllCookies().toString();
            logger.info("Retrieved all cookies (values redacted)");
            return allCookies;
        } catch (Exception e) {
            logger.error("Failed to retrieve all cookies.", e);
            throw e;
        }
    }

    /**
     * Retrieves the current URL of the browser.
     *
     * @return The current URL as a string.
     */
    @Tool(name = "browser_get_current_url", description = "gets current URL")
    public String getCurrentUrl() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String currentUrl = driver.browser().getCurrentURL();
            logger.info("Current URL retrieved (value redacted)");
            return currentUrl;
        } catch (Exception e) {
            logger.error("Failed to retrieve current URL.", e);
            throw e;
        }
    }

    /**
     * Retrieves the title of the current page in the browser.
     *
     * @return The page title as a string.
     */
    @Tool(name = "browser_get_title", description = "gets current page title")
    public String getTitle() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String title = driver.browser().getCurrentWindowTitle();
            logger.info("Page title retrieved (length: {})", title == null ? 0 : title.length());
            return title;
        } catch (Exception e) {
            logger.error("Failed to retrieve page title.", e);
            throw e;
        }
    }

    /**
     * Captures the current page DOM for MCP browser automation inspection.
     *
     * @param maxCharacters maximum DOM characters to return; uses a safe default when unset or non-positive
     * @return page DOM snapshot and browser context metadata
     */
    @Tool(name = "browser_get_page_dom",
            description = "returns bounded current-page DOM for locator inspection before element_* or natural_act")
    public McpPageDomSnapshot getPageDom(int maxCharacters) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            WebDriver seleniumDriver = driver.getDriver();
            int limit = maxCharacters <= 0 ? DEFAULT_DOM_CHARACTER_LIMIT : maxCharacters;
            String dom = seleniumDriver.getPageSource();
            String safeDom = dom == null ? "" : dom;
            boolean truncated = safeDom.length() > limit;
            String returnedDom = truncated ? safeDom.substring(0, limit) : safeDom;
            logger.info("Page DOM retrieved (characters: {}, returned: {}, truncated: {})",
                    safeDom.length(), returnedDom.length(), truncated);
            return new McpPageDomSnapshot(
                    seleniumDriver.getCurrentUrl(),
                    seleniumDriver.getTitle(),
                    returnedDom,
                    safeDom.length(),
                    truncated,
                    truncated ? List.of("DOM was truncated; increase maxCharacters for more context.") : List.of());
        } catch (Exception e) {
            logger.error("Failed to retrieve page DOM.", e);
            throw e;
        }
    }

    /**
     * Opens a URL and returns bounded DOM plus capture-ranked locator candidates for the user's intent.
     *
     * @param targetUrl URL to open in the active WebDriver session
     * @param userIntent natural-language action goal, such as click sign in or type email
     * @param maxCharacters maximum DOM characters to return
     * @param maxElements maximum element candidates to return
     * @return JSON-shaped DOM orientation and locator candidates
     */
    @Tool(name = "browser_open_intent",
            description = "opens a URL and returns bounded DOM plus capture-ranked locator candidates for the user intent")
    public Map<String, Object> openForIntent(
            String targetUrl,
            String userIntent,
            int maxCharacters,
            int maxElements) {
        navigate(targetUrl);
        WebDriver seleniumDriver = getDriver().getDriver();
        return orientPage(
                seleniumDriver.getCurrentUrl(),
                seleniumDriver.getTitle(),
                seleniumDriver.getPageSource(),
                userIntent,
                maxCharacters,
                maxElements);
    }

    /**
     * Takes a PNG screenshot of the current browser viewport for MCP browser automation inspection.
     *
     * @param outputPath    optional workspace-relative or workspace-contained output file path
     * @param includeBase64 whether to include the PNG bytes as base64 in the response
     * @return screenshot metadata and optional base64 payload
     */
    @Tool(name = "browser_take_screenshot", description = "takes a PNG screenshot of the current browser viewport")
    public McpScreenshotResult takeScreenshot(String outputPath, boolean includeBase64) {
        try {
            SHAFT.GUI.WebDriver shaftDriver = getDriver();
            WebDriver seleniumDriver = shaftDriver.getDriver();
            if (!(seleniumDriver instanceof TakesScreenshot takesScreenshot)) {
                throw new IllegalStateException("The active browser driver does not support screenshots.");
            }
            byte[] png = takesScreenshot.getScreenshotAs(OutputType.BYTES);
            Path writtenPath = writeScreenshot(outputPath, png);
            logger.info("Browser screenshot captured (bytes: {}, persisted: {}, base64Included: {})",
                    png.length, writtenPath != null, includeBase64);
            return new McpScreenshotResult(
                    "image/png",
                    png.length,
                    includeBase64 ? Base64.getEncoder().encodeToString(png) : null,
                    writtenPath == null ? null : writtenPath.toString(),
                    includeBase64 ? List.of() : List.of("Base64 omitted; set includeBase64=true to return inline PNG bytes."));
        } catch (Exception e) {
            logger.error("Failed to take browser screenshot.", e);
            throw e;
        }
    }

    /**
     * Saves the active browser session's cookies, {@code localStorage}, and {@code sessionStorage} to a
     * JSON file.
     *
     * @param filePath workspace-relative or workspace-contained output file path
     * @return the absolute path the storage state was written to
     */
    @Tool(name = "browser_storage_state_save",
            description = "saves the active browser session's cookies, localStorage, and sessionStorage to a JSON file")
    public String saveStorageState(String filePath) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            Path resolved = workspacePolicy.output(filePath, "Storage state output path");
            driver.browser().saveStorageState(resolved.toString());
            logger.info("Browser storage state saved (path length: {}).", resolved.toString().length());
            return resolved.toString();
        } catch (Exception e) {
            logger.error("Failed to save browser storage state.", e);
            throw e;
        }
    }

    /**
     * Loads cookies, {@code localStorage}, and {@code sessionStorage} from a JSON file into the active
     * browser session.
     *
     * <p>Navigate to the target origin before loading storage state so browser cookie domain rules can
     * apply.
     *
     * @param filePath workspace-contained source JSON file path
     * @return a short confirmation including the resolved path and restored cookie count
     */
    @Tool(name = "browser_storage_state_load",
            description = "loads cookies, localStorage, and sessionStorage from a JSON file into the active "
                    + "browser session; navigate to the target origin first")
    public String loadStorageState(String filePath) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            Path resolved = workspacePolicy.existing(filePath, "Storage state input path");
            driver.browser().loadStorageState(resolved.toString());
            int cookieCount = driver.browser().getAllCookies().size();
            logger.info("Browser storage state loaded (path length: {}, cookies: {}).",
                    resolved.toString().length(), cookieCount);
            return "Loaded browser storage state from " + resolved + " (cookies restored: " + cookieCount + ")";
        } catch (Exception e) {
            logger.error("Failed to load browser storage state.", e);
            throw e;
        }
    }

    /**
     * Captures an accessible-name-tree aria snapshot (SHAFT's YAML subset of Playwright's aria snapshot
     * DSL) of the whole page or a single element for MCP browser automation inspection.
     *
     * @param locatorStrategy locator strategy; leave unset together with locatorValue to snapshot the
     *                        whole page
     * @param locatorValue    locator value; leave blank to snapshot the whole page
     * @return the aria snapshot serialized as YAML
     */
    @Tool(name = "browser_aria_snapshot",
            description = "captures an accessible-name-tree aria snapshot (YAML) of the whole page, or of one "
                    + "element when a locator is supplied")
    public String ariaSnapshot(locatorStrategy locatorStrategy, String locatorValue) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            boolean wholePage = locatorStrategy == null || locatorValue == null || locatorValue.isBlank();
            By locator = wholePage ? By.tagName("html") : getLocator(locatorStrategy, locatorValue);
            String snapshot = driver.element().ariaSnapshot(locator);
            logger.info("Aria snapshot captured (wholePage: {}, characters: {}).",
                    wholePage, snapshot == null ? 0 : snapshot.length());
            return snapshot;
        } catch (Exception e) {
            logger.error("Failed to capture aria snapshot.", e);
            throw e;
        }
    }

    /**
     * Runs a non-asserting axe-core WCAG accessibility audit on the current page and returns the
     * violations found. Unlike {@code AccessibilityActions.assertIsAccessible}, this tool never fails
     * the call when violations exist; it reports them so the caller can decide what to do.
     *
     * @param wcagTags optional axe-core WCAG tags (e.g. {@code wcag2a}, {@code wcag21aa}) to scope the
     *                 audit to; when empty, SHAFT's default tag set is used
     * @return the audit result, including a violation count and a per-violation summary
     */
    @Tool(name = "browser_accessibility_audit",
            description = "runs a non-asserting axe-core WCAG accessibility audit on the current page and "
                    + "returns the violations found; never fails the call when violations exist")
    public McpAccessibilityAuditResult accessibilityAudit(String... wcagTags) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            AccessibilityHelper.AccessibilityConfig config = new AccessibilityHelper.AccessibilityConfig();
            List<String> tags = wcagTags == null || wcagTags.length == 0
                    ? List.copyOf(config.getTags())
                    : List.of(wcagTags);
            if (wcagTags != null && wcagTags.length > 0) {
                config.setTags(Arrays.asList(wcagTags));
            }
            AccessibilityHelper.AccessibilityResult result =
                    driver.browser().accessibility().analyzeAndReturn("mcp-accessibility-audit", config, false);
            List<McpAccessibilityViolation> violations = result.getViolations().stream()
                    .map(BrowserService::toAccessibilityViolation)
                    .toList();
            logger.info("Accessibility audit completed (violations: {}, score: {}).",
                    result.getViolationsCount(), result.getAccessibilityScore());
            return new McpAccessibilityAuditResult(
                    result.getAccessibilityScore(),
                    result.getViolationsCount(),
                    result.getPassCount(),
                    tags,
                    violations,
                    violations.isEmpty()
                            ? List.of()
                            : List.of("Accessibility violations were found; this tool reports them without "
                                    + "failing the call."));
        } catch (Exception e) {
            logger.error("Failed to run accessibility audit.", e);
            throw e;
        }
    }

    private static McpAccessibilityViolation toAccessibilityViolation(Rule rule) {
        List<CheckedNode> nodes = rule.getNodes() == null ? List.of() : rule.getNodes();
        List<McpAccessibilityViolationNode> nodeSummaries = nodes.stream()
                .limit(MAX_VIOLATION_NODES)
                .map(node -> new McpAccessibilityViolationNode(
                        String.valueOf(node.getTarget()),
                        trimTo(node.getHtml(), MAX_VIOLATION_NODE_HTML_LENGTH)))
                .toList();
        return new McpAccessibilityViolation(
                rule.getId(),
                rule.getImpact(),
                rule.getDescription(),
                rule.getHelp(),
                rule.getHelpUrl(),
                rule.getTags() == null ? List.of() : List.copyOf(rule.getTags()),
                nodes.size(),
                nodeSummaries);
    }

    private Path writeScreenshot(String outputPath, byte[] png) {
        if (outputPath == null || outputPath.isBlank()) {
            return null;
        }
        Path resolved = workspacePolicy.output(outputPath, "Screenshot output path");
        try {
            Path parent = resolved.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            return Files.write(resolved, png);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Screenshot output path cannot be written inside the MCP workspace.", exception);
        }
    }

    static Map<String, Object> orientPage(
            String currentUrl,
            String title,
            String dom,
            String userIntent,
            int maxCharacters,
            int maxElements) {
        String safeDom = dom == null ? "" : dom;
        int characterLimit = maxCharacters <= 0 ? DEFAULT_DOM_CHARACTER_LIMIT : maxCharacters;
        int elementLimit = maxElements <= 0 ? DEFAULT_ELEMENT_LIMIT : Math.min(maxElements, MAX_ELEMENT_LIMIT);
        boolean truncated = safeDom.length() > characterLimit;
        List<Map<String, Object>> elements = elements(currentUrl, title, safeDom, userIntent, elementLimit);
        List<String> warnings = new ArrayList<>();
        if (truncated) {
            warnings.add("DOM was truncated; increase maxCharacters for more context.");
        }
        if (elements.isEmpty()) {
            warnings.add("No actionable element candidates matched the DOM and intent.");
        }
        return Map.of(
                "schemaVersion", "1.0",
                "currentUrl", text(currentUrl),
                "title", text(title),
                "userIntent", text(userIntent),
                "dom", truncated ? safeDom.substring(0, characterLimit) : safeDom,
                "characterCount", safeDom.length(),
                "truncated", truncated,
                "elements", elements,
                "nextTools", nextTools(userIntent),
                "warnings", warnings);
    }

    private static List<Map<String, Object>> elements(
            String currentUrl,
            String title,
            String dom,
            String userIntent,
            int maxElements) {
        Document document = Jsoup.parse(dom, currentUrl == null ? "" : currentUrl);
        Set<String> tokens = tokens(userIntent);
        LocatorRanker ranker = new LocatorRanker();
        EventContext context = new EventContext(
                1,
                Instant.EPOCH,
                new PageContext(currentUrl, title, "window-1", List.of(), 0, 0),
                EventContext.ReplayStatus.NOT_REPLAYED,
                List.of(),
                Map.of());
        List<ScoredElement> scored = new ArrayList<>();
        for (Element element : document.select("button, a[href], input, textarea, select, [role], [aria-label],"
                + " [data-testid], [data-test], [data-qa]")) {
            ElementSnapshot snapshot = snapshot(document, element);
            if (snapshot.locatorCandidates().isEmpty()) {
                continue;
            }
            int intentScore = intentScore(tokens, snapshot, element);
            if (!tokens.isEmpty() && intentScore == 0) {
                continue;
            }
            scored.add(new ScoredElement(
                    snapshot,
                    trimTo(element.text(), 120),
                    intentScore,
                    ranker.select(snapshot, context, true)));
        }
        return scored.stream()
                .sorted(Comparator.comparingInt(ScoredElement::intentScore).reversed()
                        .thenComparing(item -> item.selection().selected().score(), Comparator.reverseOrder()))
                .limit(maxElements)
                .map(BrowserService::elementCandidate)
                .toList();
    }

    private static ElementSnapshot snapshot(Document document, Element element) {
        String tag = element.tagName().toLowerCase(Locale.ROOT);
        String role = role(element);
        String label = label(document, element);
        String accessibleName = firstText(
                element.attr("aria-label"), element.attr("title"), element.attr("alt"),
                label, element.attr("placeholder"), element.text());
        Map<String, String> attributes = attributes(element);
        List<LocatorCandidate> locators = locators(document, element, tag, role, accessibleName, label);
        return new ElementSnapshot(
                logicalId(tag, accessibleName, attributes),
                tag,
                role,
                accessibleName,
                label,
                attributes,
                locators,
                true,
                !element.hasAttr("disabled"),
                element.hasAttr("selected") || element.hasAttr("checked"));
    }

    private static List<LocatorCandidate> locators(
            Document document,
            Element element,
            String tag,
            String role,
            String accessibleName,
            String label) {
        List<LocatorCandidate> candidates = new ArrayList<>();
        if (!role.isBlank() && !accessibleName.isBlank()) {
            candidates.add(locator(LocatorCandidate.LocatorStrategy.ROLE, role + ":" + accessibleName,
                    countName(document, role, accessibleName), true, LocatorCandidate.LocatorSignal.ACCESSIBLE));
        }
        if (!accessibleName.isBlank()) {
            candidates.add(locator(LocatorCandidate.LocatorStrategy.ACCESSIBLE_NAME, accessibleName,
                    countAccessibleName(document, accessibleName), true, LocatorCandidate.LocatorSignal.ACCESSIBLE));
        }
        if (!label.isBlank()) {
            candidates.add(locator(LocatorCandidate.LocatorStrategy.LABEL, label,
                    countLabel(document, label), true, LocatorCandidate.LocatorSignal.LABEL_ASSOCIATED));
        }
        for (String attribute : List.of("data-testid", "data-test", "data-qa")) {
            String value = element.attr(attribute);
            if (!value.isBlank()) {
                candidates.add(locator(LocatorCandidate.LocatorStrategy.TEST_ID, cssAttribute(attribute, value),
                        count(document, cssAttribute(attribute, value)), true,
                        LocatorCandidate.LocatorSignal.TEST_ATTRIBUTE));
                break;
            }
        }
        if (!element.id().isBlank()) {
            candidates.add(locator(LocatorCandidate.LocatorStrategy.ID, element.id(),
                    count(document, cssAttribute("id", element.id())), !dynamic(element.id()),
                    LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE));
        }
        if (!element.attr("name").isBlank()) {
            candidates.add(locator(LocatorCandidate.LocatorStrategy.NAME, element.attr("name"),
                    count(document, cssAttribute("name", element.attr("name"))), !dynamic(element.attr("name")),
                    LocatorCandidate.LocatorSignal.STABLE_ATTRIBUTE));
        }
        if (!accessibleName.isBlank()) {
            candidates.add(locator(LocatorCandidate.LocatorStrategy.XPATH,
                    "//" + tag + "[normalize-space(.)=" + xpathLiteral(accessibleName)
                            + " or @aria-label=" + xpathLiteral(accessibleName) + "]",
                    1, false, LocatorCandidate.LocatorSignal.GENERATED));
        }
        return candidates;
    }

    private static LocatorCandidate locator(
            LocatorCandidate.LocatorStrategy strategy,
            String expression,
            int uniquenessCount,
            boolean stable,
            LocatorCandidate.LocatorSignal signal) {
        EnumSet<LocatorCandidate.LocatorSignal> signals = EnumSet.of(signal);
        if (!stable) {
            signals.add(LocatorCandidate.LocatorSignal.DYNAMIC_VALUE);
        }
        return new LocatorCandidate(strategy, expression, uniquenessCount, true, stable, signals);
    }

    private static Map<String, Object> elementCandidate(ScoredElement item) {
        ElementSnapshot snapshot = item.snapshot();
        Map<String, Object> best = locatorCandidate(item.selection().selected(), snapshot);
        return Map.of(
                "tagName", snapshot.tagName(),
                "role", snapshot.role(),
                "accessibleName", snapshot.accessibleName(),
                "label", snapshot.label(),
                "text", item.text(),
                "intentScore", item.intentScore(),
                "bestLocator", best,
                "alternativeLocators", item.selection().alternatives().stream()
                        .map(alternative -> locatorCandidate(alternative, snapshot))
                        .toList(),
                "shaftLocatorCode", best.get("shaftLocatorCode"));
    }

    private static Map<String, Object> locatorCandidate(LocatorRanker.ScoredLocator scored, ElementSnapshot target) {
        LocatorCandidate locator = scored.candidate();
        return Map.of(
                "strategy", locator.strategy().name(),
                "expression", locator.expression(),
                "score", scored.score(),
                "scoreBreakdown", scored.breakdown(),
                "shaftLocatorCode", locatorCode(locator, target));
    }

    private static String locatorCode(LocatorCandidate candidate, ElementSnapshot target) {
        String expression = candidate.expression();
        return switch (candidate.strategy()) {
            case ROLE, ACCESSIBLE_NAME, LABEL -> {
                String method = inputLike(target) ? "inputField" : "clickableField";
                String name = target.accessibleName().isBlank() ? expression : target.accessibleName();
                yield "SHAFT.GUI.Locator." + method + "(\"" + javaString(name) + "\")";
            }
            case TEST_ID, CSS -> "SHAFT.GUI.Locator.cssSelector(\"" + javaString(expression) + "\")";
            case ID -> "SHAFT.GUI.Locator.id(\"" + javaString(expression) + "\")";
            case NAME -> "SHAFT.GUI.Locator.name(\"" + javaString(expression) + "\")";
            case XPATH -> "By.xpath(\"" + javaString(expression) + "\")";
        };
    }

    private static boolean inputLike(ElementSnapshot target) {
        return switch (target.tagName()) {
            case "input", "textarea", "select" -> true;
            default -> target.role().equals("textbox") || target.role().equals("combobox")
                    || target.role().equals("searchbox");
        };
    }

    private static int intentScore(Set<String> tokens, ElementSnapshot snapshot, Element element) {
        if (tokens.isEmpty()) {
            return 1;
        }
        String searchable = String.join(" ", snapshot.tagName(), snapshot.role(), snapshot.accessibleName(),
                snapshot.label(), element.text(), String.join(" ", snapshot.normalizedAttributes().values()))
                .toLowerCase(Locale.ROOT);
        int score = 0;
        for (String token : tokens) {
            if (searchable.contains(token)) {
                score++;
            }
        }
        return score;
    }

    private static List<String> nextTools(String userIntent) {
        LinkedHashSet<String> tools = new LinkedHashSet<>();
        String intent = text(userIntent).toLowerCase(Locale.ROOT);
        tools.add("browser_get_page_dom");
        tools.add("browser_take_screenshot");
        tools.add("shaft_guide_search");
        if (intent.contains("type") || intent.contains("enter") || intent.contains("fill")) {
            tools.add("element_type");
        }
        if (intent.isBlank() || intent.contains("click") || intent.contains("tap") || intent.contains("press")
                || intent.contains("open") || intent.contains("select")) {
            tools.add("element_click");
        }
        tools.add("natural_act");
        tools.add("capture_start");
        tools.add("capture_code_blocks");
        tools.add("test_code_guardrails_check");
        return List.copyOf(tools);
    }

    private static Set<String> tokens(String value) {
        LinkedHashSet<String> tokens = new LinkedHashSet<>();
        for (String token : text(value).toLowerCase(Locale.ROOT).split("[^a-z0-9]+")) {
            if (token.length() > 2) {
                tokens.add(token);
            }
        }
        return tokens;
    }

    private static Map<String, String> attributes(Element element) {
        Map<String, String> values = new LinkedHashMap<>();
        for (String name : List.of("id", "name", "type", "role", "aria-label", "title",
                "placeholder", "data-testid", "data-test", "data-qa")) {
            String value = element.attr(name);
            if (!value.isBlank()) {
                values.put(name, value.trim());
            }
        }
        return values;
    }

    private static String role(Element element) {
        String explicit = element.attr("role").trim();
        if (!explicit.isBlank()) {
            return explicit.toLowerCase(Locale.ROOT);
        }
        return switch (element.tagName().toLowerCase(Locale.ROOT)) {
            case "button" -> "button";
            case "a" -> element.hasAttr("href") ? "link" : "";
            case "textarea" -> "textbox";
            case "select" -> "combobox";
            case "input" -> inputRole(element.attr("type"));
            default -> "";
        };
    }

    private static String inputRole(String type) {
        return switch (type == null ? "" : type.toLowerCase(Locale.ROOT)) {
            case "button", "submit", "reset", "image" -> "button";
            case "checkbox" -> "checkbox";
            case "radio" -> "radio";
            case "search" -> "searchbox";
            default -> "textbox";
        };
    }

    private static String label(Document document, Element element) {
        String id = element.id();
        if (!id.isBlank()) {
            for (Element label : document.select("label[for]")) {
                if (id.equals(label.attr("for"))) {
                    return label.text().trim();
                }
            }
        }
        Element parentLabel = element.closest("label");
        return parentLabel == null ? "" : parentLabel.text().trim();
    }

    private static int count(Document document, String cssSelector) {
        try {
            return document.select(cssSelector).size();
        } catch (RuntimeException exception) {
            return 0;
        }
    }

    private static int countName(Document document, String role, String accessibleName) {
        int count = 0;
        for (Element element : document.select("button, a[href], input, textarea, select, [role], [aria-label]")) {
            if (role(element).equals(role) && firstText(element.attr("aria-label"), element.attr("title"),
                    element.attr("alt"), label(document, element), element.attr("placeholder"), element.text())
                    .equals(accessibleName)) {
                count++;
            }
        }
        return count;
    }

    private static int countAccessibleName(Document document, String accessibleName) {
        int count = 0;
        for (Element element : document.select("button, a[href], input, textarea, select, [role], [aria-label]")) {
            if (firstText(element.attr("aria-label"), element.attr("title"), element.attr("alt"),
                    label(document, element), element.attr("placeholder"), element.text()).equals(accessibleName)) {
                count++;
            }
        }
        return count;
    }

    private static int countLabel(Document document, String label) {
        int count = 0;
        for (Element element : document.select("input, textarea, select")) {
            if (label(document, element).equals(label)) {
                count++;
            }
        }
        return count;
    }

    private static boolean dynamic(String value) {
        String text = text(value).toLowerCase(Locale.ROOT);
        return text.matches(".*[0-9a-f]{8,}.*") || text.matches(".*\\d{5,}.*");
    }

    private static String logicalId(String tag, String accessibleName, Map<String, String> attributes) {
        String seed = firstText(attributes.get("id"), attributes.get("name"), accessibleName, tag);
        return seed.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "-").replaceAll("(^-|-$)", "");
    }

    private static String firstText(String... values) {
        for (String value : values) {
            String text = text(value);
            if (!text.isBlank()) {
                return text;
            }
        }
        return "";
    }

    private static String trimTo(String value, int maxLength) {
        String text = text(value);
        return text.length() <= maxLength ? text : text.substring(0, maxLength);
    }

    private static String text(String value) {
        return value == null ? "" : value.trim();
    }

    private static String cssAttribute(String name, String value) {
        return "[" + name + "=\"" + text(value).replace("\\", "\\\\").replace("\"", "\\\"") + "\"]";
    }

    private static String javaString(String value) {
        return text(value).replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\r", "\\r")
                .replace("\n", "\\n")
                .replace("\t", "\\t");
    }

    private static String xpathLiteral(String value) {
        String text = text(value);
        if (!text.contains("'")) {
            return "'" + text + "'";
        }
        if (!text.contains("\"")) {
            return "\"" + text + "\"";
        }
        return "concat('" + text.replace("'", "',\"'\",'") + "')";
    }

    private record ScoredElement(
            ElementSnapshot snapshot,
            String text,
            int intentScore,
            LocatorRanker.LocatorSelection selection) {
    }
}
