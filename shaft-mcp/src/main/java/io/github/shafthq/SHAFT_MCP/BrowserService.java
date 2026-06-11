package io.github.shafthq.SHAFT_MCP;

import com.shaft.driver.SHAFT;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.stereotype.Service;

import static io.github.shafthq.SHAFT_MCP.EngineService.getDriver;

@Service
public class BrowserService {
    private static final Logger logger = LoggerFactory.getLogger(BrowserService.class);

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
            logger.info("Navigated to URL: {}", targetUrl);
        } catch (Exception e) {
            logger.error("Failed to navigate to URL: {}", targetUrl, e);
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
    @Tool(name = "browser_navigate_with_basic_auth", description = "navigates to a URL with Basic Authentication")
    public void navigateWithBasicAuth(String targetUrl, String username, String password, String targetUrlAfterAuth) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().navigateToURLWithBasicAuthentication(targetUrl, username, password, targetUrlAfterAuth);
            logger.info("Navigated to URL with Basic Authentication: {}", targetUrl);
        } catch (Exception e) {
            logger.error("Failed to navigate to URL with Basic Authentication: {}", targetUrl, e);
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
            logger.info("Cookie '{}' deleted.", cookieName);
        } catch (Exception e) {
            logger.error("Failed to delete cookie '{}'.", cookieName, e);
            throw e;
        }
    }

    /**
     * Adds a cookie to the current browser session.
     *
     * @param name  The name of the cookie.
     * @param value The value of the cookie.
     */
    @Tool(name = "browser_add_cookie", description = "adds a cookie")
    public void addCookie(String name, String value) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            driver.browser().addCookie(name, value);
            logger.info("Cookie added: {}={}", name, value);
        } catch (Exception e) {
            logger.error("Failed to add cookie: {}={}", name, value, e);
            throw e;
        }
    }

    /**
     * Retrieves a cookie by name from the current browser session.
     *
     * @param cookieName The name of the cookie to retrieve.
     * @return The cookie value as a string, or null if not found.
     */
    @Tool(name = "browser_get_cookie", description = "gets a cookie by name")
    public String getCookie(String cookieName) {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String cookieValue = driver.browser().getCookie(cookieName).getValue();
            logger.info("Retrieved cookie: {}={}", cookieName, cookieValue);
            return cookieValue;
        } catch (Exception e) {
            logger.error("Failed to retrieve cookie '{}'.", cookieName, e);
            throw e;
        }
    }

    /**
     * Retrieves all cookies from the current browser session.
     *
     * @return A string representation of all cookies.
     */
    @Tool(name = "browser_get_all_cookies", description = "gets all cookies")
    public String getAllCookies() {
        try {
            SHAFT.GUI.WebDriver driver = getDriver();
            String allCookies = driver.browser().getAllCookies().toString();
            logger.info("Retrieved all cookies: {}", allCookies);
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
            logger.info("Current URL retrieved: {}", currentUrl);
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
            logger.info("Page title retrieved: {}", title);
            return title;
        } catch (Exception e) {
            logger.error("Failed to retrieve page title.", e);
            throw e;
        }
    }
}