package com.shaft.mcp;

import com.shaft.driver.SHAFT;
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
import java.util.Base64;
import java.util.List;

import static com.shaft.mcp.EngineService.getDriver;

@Service
public class BrowserService {
    private static final int DEFAULT_DOM_CHARACTER_LIMIT = 200_000;
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
    @Tool(name = "browser_navigate", description = "navigates to a URL")
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
    @Tool(name = "browser_get_page_dom", description = "gets the current page DOM/page source for browser automation inspection")
    public McpPageDomSnapshot getPageDom(int maxCharacters) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            int limit = maxCharacters <= 0 ? DEFAULT_DOM_CHARACTER_LIMIT : maxCharacters;
            String dom = driver.browser().getPageSource();
            String safeDom = dom == null ? "" : dom;
            boolean truncated = safeDom.length() > limit;
            String returnedDom = truncated ? safeDom.substring(0, limit) : safeDom;
            logger.info("Page DOM retrieved (characters: {}, returned: {}, truncated: {})",
                    safeDom.length(), returnedDom.length(), truncated);
            return new McpPageDomSnapshot(
                    driver.browser().getCurrentURL(),
                    driver.browser().getCurrentWindowTitle(),
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
}
