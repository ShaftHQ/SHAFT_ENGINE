package com.shaft.gui.browser.internal;

import com.google.common.net.InternetDomainName;
import com.shaft.db.DatabaseActions;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.SynchronizationManager;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import lombok.SneakyThrows;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.bidi.BiDiException;
import org.openqa.selenium.bidi.browsingcontext.BrowsingContext;
import org.openqa.selenium.bidi.browsingcontext.ReadinessState;
import org.openqa.selenium.chromium.ChromiumDriver;
import org.openqa.selenium.devtools.DevToolsException;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;

import java.awt.*;
import java.io.File;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.*;
import java.util.List;

/**
 * Internal helper class for browser actions within the SHAFT framework.
 * Provides utility methods for navigating URLs, reporting pass/fail results,
 * maximizing browser windows, capturing page snapshots, and formatting URLs
 * for basic authentication.
 *
 * <p>Instances are typically created per action with a {@code isSilent} flag
 * that suppresses Allure report entries when set to {@code true}.
 *
 * <p>Example usage:
 * <pre>{@code
 * BrowserActionsHelper helper = new BrowserActionsHelper(false);
 * helper.navigateToNewUrl(driver, currentUrl, "https://example.com", "");
 * }</pre>
 */
public class BrowserActionsHelper {
    /**
     * The navigation timeout in seconds, sourced from {@code SHAFT.Properties.timeouts.browserNavigationTimeout()}.
     * Used as the maximum wait duration for URL-change conditions.
     */
    public static final int NAVIGATION_TIMEOUT_INTEGER = SHAFT.Properties.timeouts.browserNavigationTimeout();
    private final boolean isSilent;
    private final Boolean HEADLESS_EXECUTION = SHAFT.Properties.web.headlessExecution();

    /**
     * Creates a new {@code BrowserActionsHelper}.
     *
     * @param isSilent when {@code true}, action results are not written to the Allure report;
     *                 useful for internal/utility navigations that should not appear in test output
     */
    public BrowserActionsHelper(boolean isSilent) {
        this.isSilent = isSilent;
    }

    /**
     * Reports a successful browser action without a WebDriver instance.
     * The action name is derived from the caller's method name.
     *
     * <p>Example:
     * <pre>{@code
     * passAction("https://example.com");
     * }</pre>
     *
     * @param testData descriptive data associated with the action (e.g. the URL navigated to)
     */
    public void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(null, actionName, testData);
    }

    /**
     * Reports a successful browser action, attaching a screenshot when a driver is provided.
     * The action name is derived from the caller's method name.
     *
     * <p>Example:
     * <pre>{@code
     * passAction(driver, "Page title was correct");
     * }</pre>
     *
     * @param driver   the active {@link WebDriver} instance used to capture a screenshot, or {@code null}
     * @param testData descriptive data associated with the action
     */
    public void passAction(WebDriver driver, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, actionName, testData);
    }

    /**
     * Reports a successful browser action with an explicit action name.
     *
     * <p>Example:
     * <pre>{@code
     * passAction(driver, "navigateToURL", "https://example.com");
     * }</pre>
     *
     * @param driver     the active {@link WebDriver} instance used to capture a screenshot, or {@code null}
     * @param actionName the display name of the action as it should appear in the report
     * @param testData   descriptive data associated with the action
     */
    public void passAction(WebDriver driver, String actionName, String testData) {
        reportActionResult(driver, actionName, testData, true);
    }

    /**
     * Reports a failed browser action without a WebDriver instance or test data.
     * The action name is derived from the caller's method name.
     *
     * <p>Example:
     * <pre>{@code
     * failAction(rootCauseException);
     * }</pre>
     *
     * @param rootCauseException optional root-cause exception(s) to include in the report
     */
    public void failAction(Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(null, actionName, "", rootCauseException);
    }

    /**
     * Reports a failed browser action, capturing a screenshot when a driver is provided.
     * The action name is derived from the caller's method name.
     *
     * <p>Example:
     * <pre>{@code
     * failAction(driver, "Navigation failed", rootCauseException);
     * }</pre>
     *
     * @param driver             the active {@link WebDriver} instance used to capture a screenshot, or {@code null}
     * @param testData           descriptive data associated with the failure
     * @param rootCauseException optional root-cause exception(s) to include in the report
     */
    public void failAction(WebDriver driver, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, rootCauseException);
    }

    /**
     * Reports a failed browser action with an explicit action name, then throws a test failure.
     *
     * <p>Example:
     * <pre>{@code
     * failAction(driver, "navigateToURL", "https://example.com", rootCauseException);
     * }</pre>
     *
     * @param driver             the active {@link WebDriver} instance used to capture a screenshot, or {@code null}
     * @param actionName         the display name of the action as it should appear in the report
     * @param testData           descriptive data associated with the failure
     * @param rootCauseException optional root-cause exception(s) to include in the report
     */
    public void failAction(WebDriver driver, String actionName, String testData,
                           Exception... rootCauseException) {
        String message = reportActionResult(driver, actionName, testData, false, rootCauseException);
        if (rootCauseException != null && rootCauseException.length > 0) {
            FailureReporter.fail(DatabaseActions.class, message, rootCauseException[0]);
        } else {
            FailureReporter.fail(message);
        }
    }

    private String reportActionResult(WebDriver driver, String actionName, String testData,
                                      Boolean passFailStatus,
                                      Exception... rootCauseException) {
        actionName = JavaHelper.convertToSentenceCase(actionName);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Browser Action: " + actionName;
        } else {
            message = "Browser Action: " + actionName + " failed";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty()) {
            if (testData.length() >= 500 || testData.contains("</iframe>") || testData.contains("</html>") || testData.startsWith("From: <Saved by Blink>")) {
                List<Object> actualValueAttachment = Arrays.asList("Browser Action Test Data - " + actionName,
                        "Actual Value", testData);
                attachments.add(actualValueAttachment);
            } else {
                message = message + " \"" + testData.trim() + "\"";
            }
        }

        if (rootCauseException != null && rootCauseException.length >= 1) {
            List<Object> actualValueAttachment = Arrays.asList("Browser Action Exception - " + actionName,
                    "Stacktrace", ReportManagerHelper.formatStackTraceToLogEntry(rootCauseException[0]));
            attachments.add(actualValueAttachment);
        }

        message = message + ".";

        message = message.replace("Browser Action: ", "");
        if (!isSilent) {
            if (driver != null && !message.equals("Capture page snapshot.")) {
                attachments.add(new ScreenshotManager().takeScreenshot(driver, null, actionName, passFailStatus));
                ReportManagerHelper.log(message, attachments);
            } else if (!attachments.isEmpty()) {
                ReportManagerHelper.log(message, attachments);
            } else {
                ReportManager.log(message);
            }
        }
        return message;
    }

    /**
     * Verifies that the page currently loaded in the browser is not a network-error page.
     * Waits up to {@link #NAVIGATION_TIMEOUT_INTEGER} seconds using a fluent wait, checking
     * well-known browser error message strings (e.g. "This site can't be reached").
     *
     * <p>Example:
     * <pre>{@code
     * helper.confirmThatWebsiteIsNotDown(driver, "https://example.com");
     * }</pre>
     *
     * @param driver    the active {@link WebDriver} instance
     * @param targetUrl the URL that was navigated to, used in the failure message if an error page is detected
     */
    public void confirmThatWebsiteIsNotDown(WebDriver driver, String targetUrl) {
        var navigationErrorMessages = Arrays.asList("This site can’t be reached", "Unable to connect",
                "Safari Can’t Connect to the Server", "This page can't be displayed", "Invalid URL",
                "<head></head><body></body>");
        new SynchronizationManager(driver).fluentWait().withTimeout(Duration.ofSeconds(SHAFT.Properties.timeouts.browserNavigationTimeout()))
                .until(d -> {
                    JavaScriptWaitManager.waitForLazyLoading(driver);
                    var pageSource = driver.getPageSource();
                    navigationErrorMessages.forEach(errorMessage -> {
                        if (pageSource != null && pageSource.contains(errorMessage)) {
                            failAction(driver, "Error message: \"" + errorMessage + "\", Target URL: \"" + targetUrl + "\"");
                        }
                    });
                    return true;
                });
    }

    /**
     * Navigates the browser to a new URL. Handles local file paths by prepending the {@code file://} scheme,
     * and prefers the W3C BiDi {@link BrowsingContext} API for navigation when available. Falls back to
     * the classic {@link WebDriver#navigate()} API for non-BiDi drivers or on timeout.
     *
     * <p>Example:
     * <pre>{@code
     * helper.navigateToNewUrl(driver, "about:blank", "https://example.com", "https://example.com/home");
     * }</pre>
     *
     * @param driver                    the active {@link WebDriver} instance
     * @param initialURL                the URL the browser was at before navigation (used for wait conditions)
     * @param targetUrl                 the URL to navigate to
     * @param targetUrlAfterRedirection the expected final URL after any server-side redirects;
     *                                  pass the same value as {@code targetUrl} if no redirect is expected
     */
    public void navigateToNewUrl(WebDriver driver, String initialURL, String targetUrl, String targetUrlAfterRedirection) {
        var internalURL = targetUrl;
        try {
            if (targetUrl.startsWith(SHAFT.Properties.paths.testData())) {
                internalURL = "file://" + new File(targetUrl).getAbsolutePath();
            }
        } catch (Exception exception) {
            ReportManagerHelper.logDiscrete(exception, Level.DEBUG);
        }

        try {
            // upgrading to w3c compliant browsing context for navigation
            new BrowsingContext(driver, driver.getWindowHandle()).navigate(internalURL, ReadinessState.valueOf(SHAFT.Properties.web.readinessState().trim().toUpperCase()));
        } catch (TimeoutException | java.lang.IllegalArgumentException |
                 org.openqa.selenium.bidi.BiDiException illegalArgumentException) {
            // Caused by: java.lang.IllegalArgumentException: WebDriver instance must support BiDi protocol
            // Caused by: org.openqa.selenium.bidi.BiDiException: Unable to create a BiDi connection
            // TimeoutException: Happens sometimes with some proxy environments
            driver.navigate().to(internalURL);
        } catch (WebDriverException rootCauseException) {
            failAction(driver, targetUrl, rootCauseException);
        }
    }

    /**
     * Waits until the browser URL reflects a successful navigation away from the initial URL,
     * and optionally waits until it contains the expected target URL.
     *
     * <p>Example:
     * <pre>{@code
     * helper.checkNavigationWasSuccessful(driver, "about:blank", "https://example.com", "https://example.com/home");
     * }</pre>
     *
     * @param driver                    the active {@link WebDriver} instance
     * @param initialURL                the URL before navigation began
     * @param targetUrl                 the URL that was requested
     * @param targetUrlAfterRedirection the expected final URL after any server-side redirect;
     *                                  when different from {@code targetUrl}, the method also waits
     *                                  for the URL to contain this value
     */
    public void checkNavigationWasSuccessful(WebDriver driver, String initialURL, String targetUrl, String targetUrlAfterRedirection) {
        if (!targetUrl.equals(targetUrlAfterRedirection)) {
            waitUntilUrlIsNot(driver, initialURL);
        } else {
            waitUntilUrlIsNot(driver, initialURL);
            var modifiedTargetUrlAfterRedirection = (targetUrlAfterRedirection.startsWith("./")) ? targetUrl : targetUrlAfterRedirection;
            waitUntilUrlContains(driver, modifiedTargetUrlAfterRedirection);
        }
    }

    /**
     * Waits until the browser URL is no longer equal to the given initial URL.
     * Times out after {@link #NAVIGATION_TIMEOUT_INTEGER} seconds, failing the test if the URL does not change.
     *
     * <p>Example:
     * <pre>{@code
     * helper.waitUntilUrlIsNot(driver, "about:blank");
     * }</pre>
     *
     * @param driver     the active {@link WebDriver} instance
     * @param initialURL the URL that the browser should navigate away from
     */
    public void waitUntilUrlIsNot(WebDriver driver, String initialURL) {
        try {
            (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER))).until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
        } catch (TimeoutException rootCauseException) {
            failAction(driver, "Waited for " + NAVIGATION_TIMEOUT_INTEGER + " seconds to navigate away from \"" + initialURL + "\" but didn't.", rootCauseException);
        }
    }

    private void waitUntilUrlContains(WebDriver driver, String targetURL) {
        try {
            (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER))).until(ExpectedConditions.urlContains(targetURL));
        } catch (TimeoutException rootCauseException) {
            failAction(driver, "Waited for " + NAVIGATION_TIMEOUT_INTEGER + " seconds to navigate to \"" + targetURL + "\" but didn't.", rootCauseException);
        }
    }

    /**
     * Attempts to maximize the browser window using the Selenium WebDriver
     * {@link org.openqa.selenium.WebDriver.Window#maximize()} method.
     * This is the preferred approach for most browser/OS combinations, but is skipped for
     * Chrome on macOS in local execution (known limitation).
     *
     * <p>Example:
     * <pre>{@code
     * Dimension size = helper.attemptMaximizeUsingSeleniumWebDriver(driver, "local", "GoogleChrome", "Windows");
     * }</pre>
     *
     * @param driver                 the active {@link WebDriver} instance
     * @param executionAddress       the grid/cloud address, or {@code "local"} for local execution
     * @param targetBrowserName      the browser name (e.g. {@code "GoogleChrome"})
     * @param targetOperatingSystem  the OS name (e.g. {@code "Windows"}, {@code "Mac"})
     * @return the window {@link Dimension} after the maximize attempt
     */
    public Dimension attemptMaximizeUsingSeleniumWebDriver(WebDriver driver, String executionAddress,
                                                           String targetBrowserName, String targetOperatingSystem) {
        if ((!"local".equals(executionAddress) && !"GoogleChrome".equals(targetBrowserName))
                || ("local".equals(executionAddress)
                && !("GoogleChrome".equals(targetBrowserName) && "Mac".equals(targetOperatingSystem)))) {
            try {
                driver.manage().window().maximize();
                Dimension currentWindowSize = driver.manage().window().getSize();
                ReportManager.logDiscrete(
                        "Window size after SWD Maximize: " + currentWindowSize);
                return currentWindowSize;
            } catch (WebDriverException rootCauseException) {
                // org.openqa.selenium.WebDriverException: unknown error: failed to change
                // window state to maximized, current state is normal
                ReportManagerHelper.logDiscrete(rootCauseException);
            }
        }
        return driver.manage().window().getSize();
    }

    /**
     * Attempts to maximize the browser window by reading the physical screen dimensions from
     * {@link java.awt.Toolkit} and resizing the window accordingly. Falls back to JavaScript
     * execution when running in a headless environment where {@code Toolkit} throws
     * {@link java.awt.HeadlessException}.
     *
     * <p>Example:
     * <pre>{@code
     * Dimension size = helper.attemptMaximizeUsingToolkitAndJavascript(driver, 1920, 1080);
     * }</pre>
     *
     * @param driver the active {@link WebDriver} instance
     * @param width  the fallback width to use when the screen size cannot be determined
     * @param height the fallback height to use when the screen size cannot be determined
     * @return the window {@link Dimension} after the resize attempt
     */
    public Dimension attemptMaximizeUsingToolkitAndJavascript(WebDriver driver, int width, int height) {
        int targetWidth = width;
        int targetHeight = height;
        try {
            var toolkit = Toolkit.getDefaultToolkit();
            if (Boolean.FALSE.equals(HEADLESS_EXECUTION)) {
                targetWidth = (int) toolkit.getScreenSize().getWidth();
                targetHeight = (int) toolkit.getScreenSize().getHeight();
            }
            driver.manage().window().setPosition(new org.openqa.selenium.Point(0, 0));
            driver.manage().window().setSize(new Dimension(targetWidth, targetHeight));

            ReportManager.logDiscrete("Window size after Toolkit: " + driver.manage().window().getSize());
            return driver.manage().window().getSize();
        } catch (HeadlessException e) {
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_FOCUS.getValue());
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(targetWidth)).replace("$HEIGHT", String.valueOf(targetHeight)));

            ReportManager.logDiscrete(
                    "Window size after JavascriptExecutor: " + driver.manage().window().getSize());
            return driver.manage().window().getSize();
        }
    }

    /**
     * Maximizes the browser window by explicitly setting its position to {@code (0, 0)} and
     * its size to the provided dimensions using the {@link org.openqa.selenium.WebDriver.Window} API.
     *
     * <p>Example:
     * <pre>{@code
     * Dimension size = helper.attemptMaximizeUsingSeleniumWebDriverManageWindow(driver, 1920, 1080);
     * }</pre>
     *
     * @param driver the active {@link WebDriver} instance
     * @param width  the desired window width in pixels
     * @param height the desired window height in pixels
     * @return the window {@link Dimension} after the resize
     */
    public Dimension attemptMaximizeUsingSeleniumWebDriverManageWindow(WebDriver driver, int width,
                                                                       int height) {
        driver.manage().window().setPosition(new Point(0, 0));
        driver.manage().window().setSize(new Dimension(width, height));

        ReportManager.logDiscrete(
                "Window size after WebDriver.Manage.Window: " + driver.manage().window().getSize());
        return driver.manage().window().getSize();
    }

    /**
     * Captures a snapshot of the current page. For Chromium-based browsers the Chrome DevTools
     * Protocol {@code Page.captureSnapshot} command is used to produce a full MHTML snapshot;
     * for all other browsers the standard {@link WebDriver#getPageSource()} is used as a fallback.
     *
     * <p>Example:
     * <pre>{@code
     * String snapshot = helper.capturePageSnapshot(driver);
     * }</pre>
     *
     * @param driver the active {@link WebDriver} instance, or {@code null} to fall back directly
     *               to a page-source capture
     * @return the serialised page data as a {@link String} (MHTML or HTML source)
     */
    public String capturePageSnapshot(WebDriver driver) {
        var serializedPageData = "";
        try {
            if (driver instanceof ChromiumDriver chromiumDriver) {
                var result = chromiumDriver.executeCdpCommand("Page.captureSnapshot", new HashMap<>());
                serializedPageData = (String) ((Map<String, ?>) result).get("data");
            } else {
                // get page source
                serializedPageData = driver.getPageSource();
            }
            return serializedPageData;
        } catch (BiDiException | DevToolsException exception) {
            ReportManagerHelper.logDiscrete(exception);
            return capturePageSnapshot(driver);
        } catch (WebDriverException webDriverException) {
            // unknown error: unhandled inspector error: {"code":-32000,"message":"Failed to generate MHTML"
            // try again but just get the regular page source this time
            return capturePageSnapshot(null);
        } catch (Exception rootCauseException) {
            failAction(driver, serializedPageData, rootCauseException);
            return serializedPageData;
        }
    }


    /**
     * Embeds Basic-Authentication credentials into a URL so that the browser submits them
     * automatically on navigation without requiring an interactive prompt.
     * Credentials are percent-encoded via {@link java.net.URLEncoder} to handle special characters.
     *
     * <p>Example:
     * <pre>{@code
     * String authUrl = helper.formatUrlForBasicAuthentication("admin", "p@ssw0rd", "https://example.com/secure");
     * // returns "https://admin:p%40ssw0rd@example.com/secure"
     * }</pre>
     *
     * @param username  the username for Basic Authentication
     * @param password  the password for Basic Authentication
     * @param targetUrl the target URL (must start with {@code http://} or {@code https://})
     * @return the URL with embedded Basic-Auth credentials
     */
    @SneakyThrows
    public String formatUrlForBasicAuthentication(String username, String password, String targetUrl) {
        if (targetUrl.startsWith("https://")) {
            return new URI("https://" + URLEncoder.encode(username, StandardCharsets.UTF_8) + ":" + URLEncoder.encode(password, StandardCharsets.UTF_8) + "@" + targetUrl.substring("https://".length())).toString();
        } else {
            return new URI("http://" + URLEncoder.encode(username, StandardCharsets.UTF_8) + ":" + URLEncoder.encode(password, StandardCharsets.UTF_8) + "@" + targetUrl.substring("http://".length())).toString();
        }
    }

    /**
     * Extracts the registrable (top-private) domain name from a full URL.
     * Uses the <a href="https://guava.dev/releases/snapshot/api/docs/com/google/common/net/InternetDomainName.html">
     * Guava {@code InternetDomainName}</a> class to determine the public suffix and top-level domain.
     *
     * <p>Example:
     * <pre>{@code
     * String domain = helper.getDomainNameFromUrl("https://www.example.co.uk/page");
     * // returns "example.co.uk"
     * }</pre>
     *
     * @param url the full URL string to extract the domain from
     * @return the registrable domain name (e.g. {@code "example.co.uk"})
     */
    @SneakyThrows
    public String getDomainNameFromUrl(String url) {
        // https://www.baeldung.com/java-domain-name-from-url#using-the-internetdomainname-class-from-guava-library
        URI uri = new URI(url);
        String host = uri.getHost();
        InternetDomainName internetDomainName = InternetDomainName.from(host).topPrivateDomain();
        return internetDomainName.toString();
    }
}
