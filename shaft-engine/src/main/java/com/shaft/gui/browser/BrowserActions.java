package com.shaft.gui.browser;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.driver.internal.FluentWebDriverAction;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.enums.internal.NavigationAction;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.browser.internal.BrowserNetworkProfileManager;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import com.shaft.gui.browser.internal.BrowserStorageStateManager;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.performance.internal.LightHouseGenerateReport;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FlakeProfiler;
import com.shaft.tools.io.internal.HttpContractRecorder;
import com.shaft.tools.io.internal.MobileTraceMetadata;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.io.internal.TraceEventRecorder;
import com.shaft.validation.accessibility.AccessibilityActions;
import com.shaft.validation.internal.WebDriverBrowserValidationsBuilder;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import io.qameta.allure.Step;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.*;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Provides a fluent API for performing browser-level actions such as navigation, window management,
 * cookie handling, network interception, and accessibility validation. Instances are typically
 * obtained via {@code driver.browser()} on a {@link com.shaft.driver.SHAFT.GUI.WebDriver} instance.
 *
 * <p>Example usage:
 * <pre>{@code
 * SHAFT.GUI.WebDriver driver = new SHAFT.GUI.WebDriver();
 * driver.browser()
 *       .navigateToURL("https://example.com")
 *       .and().maximizeWindow();
 * }</pre>
 */
@SuppressWarnings({"unused", "UnusedReturnValue"})
public class BrowserActions extends FluentWebDriverAction implements com.shaft.gui.driver.BrowserActionsContract {
    // Matches embedded credentials in URLs (e.g. protocol://user:password@host); compiled once for reuse
    private static final Pattern EMBEDDED_PASSWORD_PATTERN = Pattern.compile(":\\/\\/.*:(.*)@");

    /**
     * Creates a new {@code BrowserActions} instance and initializes the underlying driver
     * using the framework's default driver factory configuration.
     */
    public BrowserActions() {
        initialize();
    }

    /**
     * Creates a new {@code BrowserActions} instance wrapping the provided {@link WebDriver}.
     *
     * @param driver the {@link WebDriver} instance to use for browser actions
     */
    public BrowserActions(WebDriver driver) {
        initialize(driver);
    }

    /**
     * Creates a new {@code BrowserActions} instance wrapping the provided {@link WebDriver},
     * with control over whether reporting output is suppressed.
     *
     * @param driver   the {@link WebDriver} instance to use for browser actions
     * @param isSilent when {@code true}, step reporting is suppressed for this instance
     */
    public BrowserActions(WebDriver driver, boolean isSilent) {
        initialize(driver, isSilent);
    }

    /**
     * Creates a new {@code BrowserActions} instance backed by an existing {@link DriverFactoryHelper}.
     * Use this constructor when integrating with the internal driver lifecycle management.
     *
     * @param helper the {@link DriverFactoryHelper} that manages the underlying driver instance
     */
    public BrowserActions(DriverFactoryHelper helper) {
        initialize(helper);
    }

    @Override
    public BrowserActions and() {
        return this;
    }

    /**
     * Opens a hard-assertion builder scoped to the current browser state. Any assertion failure
     * immediately stops the test.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().assertThat().title().contains("Dashboard");
     * }</pre>
     *
     * @return a {@link WebDriverBrowserValidationsBuilder} for chaining browser assertions
     */
    @Override
    public WebDriverBrowserValidationsBuilder assertThat() {
        return new WizardHelpers.WebDriverAssertions(driverFactoryHelper).browser();
    }

    /**
     * Opens a soft-assertion (verification) builder scoped to the current browser state.
     * Failures are collected and reported at the end of the test rather than stopping it immediately.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().verifyThat().title().contains("Dashboard");
     * }</pre>
     *
     * @return a {@link WebDriverBrowserValidationsBuilder} for chaining browser verifications
     */
    @Override
    public WebDriverBrowserValidationsBuilder verifyThat() {
        return new WizardHelpers.WebDriverVerifications(driverFactoryHelper).browser();
    }

    /**
     * Attempts to capture a page snapshot archive in the format of a .mht file
     * Works only for Chromium based driver instances
     * For other driver types attempts to attach the current page source (for web)
     * or accessibility tree (for mobile)
     *
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public BrowserActions capturePageSnapshot() {
        var serializedPageData = browserActionsHelper.capturePageSnapshot(driverFactoryHelper.getDriver());
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), serializedPageData);
        return this;
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @return the URL that's currently open in the current page
     */
    @Override
    public String getCurrentURL() {
        var currentURL = "";
        try {
            currentURL = driverFactoryHelper.getDriver().getCurrentUrl();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), currentURL);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), currentURL, rootCauseException);
        }
        return currentURL;
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @return the title of the current window
     */
    @Override
    public String getCurrentWindowTitle() {
        var currentWindowTitle = "";
        try {
            currentWindowTitle = driverFactoryHelper.getDriver().getTitle();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), currentWindowTitle);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), currentWindowTitle, rootCauseException);
        }
        return currentWindowTitle;
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @return the source of the current page
     */
    @Override
    public String getPageSource() {
        var pageSource = "";
        try {
            pageSource = driverFactoryHelper.getDriver().getPageSource();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), pageSource);
        } catch (JavascriptException javascriptException) {
            //try again
            JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
            return getPageSource();
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), pageSource, rootCauseException);
        }
        return pageSource;
    }

    /**
     * Gets the current window handle and returns it as a string
     *
     * @return the window handle for the current window
     */
    @Override
    public String getWindowHandle() {
        var windowHandle = "";
        try {
            windowHandle = driverFactoryHelper.getDriver().getWindowHandle();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), windowHandle);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), windowHandle, rootCauseException);
        }
        return windowHandle;
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @return the position of the current window
     */
    @Override
    public String getWindowPosition() {
        var windowPosition = "";
        try {
            windowPosition = driverFactoryHelper.getDriver().manage().window().getPosition().toString();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), windowPosition);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), windowPosition, rootCauseException);
        }
        return windowPosition;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the size of the current window
     */
    @Override
    public String getWindowSize() {
        var windowSize = "";
        try {
            windowSize = driverFactoryHelper.getDriver().manage().window().getSize().toString();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), windowSize);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), windowSize, rootCauseException);
        }
        return windowSize;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the height of the current window
     */
    @Override
    public String getWindowHeight() {
        var windowHeight = "";
        try {
            windowHeight = String.valueOf(driverFactoryHelper.getDriver().manage().window().getSize().getHeight());
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), windowHeight, rootCauseException);
        }
        return windowHeight;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the width of the current window
     */
    @Override
    public String getWindowWidth() {
        var windowWidth = "";
        try {
            windowWidth = String.valueOf(driverFactoryHelper.getDriver().manage().window().getSize().getWidth());
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), windowWidth, rootCauseException);
        }
        return windowWidth;
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions navigateToURL(String targetUrl) {
        return navigateToURL(targetUrl, targetUrl);
    }

    /**
     * Navigates to the specified URL by opening it in a new browser tab or a new browser window,
     * depending on the provided {@link WindowType}. The driver focus switches to the newly opened context.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().navigateToURL("https://example.com", WindowType.TAB);
     * }</pre>
     *
     * @param targetUrl  a string representing the URL to navigate to
     * @param windowType the type of new context to open — either {@link WindowType#TAB} or {@link WindowType#WINDOW}
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions navigateToURL(String targetUrl, WindowType windowType) {
        var handleBeforeNavigation = driverFactoryHelper.getDriver().getWindowHandle();
        try {
            switch (windowType) {
                case TAB -> driverFactoryHelper.getDriver().switchTo().newWindow(WindowType.TAB).navigate().to(targetUrl);
                case WINDOW -> driverFactoryHelper.getDriver().switchTo().newWindow(WindowType.WINDOW).navigate().to(targetUrl);
            }
            JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
            var handleAfterNavigation = driverFactoryHelper.getDriver().getWindowHandle();
            if (!handleBeforeNavigation.equals(handleAfterNavigation)) {
                ReportManager.logDiscrete("Old Tab Handle: \"" + handleBeforeNavigation + "\", New Tab handle : \"" + handleAfterNavigation + "\"");
                browserActionsHelper.passAction(driverFactoryHelper.getDriver(), targetUrl);
            } else {
                browserActionsHelper.failAction(driverFactoryHelper.getDriver(), targetUrl);
            }
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), targetUrl, rootCauseException);
        }
        return this;
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page. Waits for successfully navigating to the final url after
     * redirection.
     *
     * @param targetUrl                 a string that represents the URL that you
     *                                  wish to navigate to
     * @param targetUrlAfterRedirection a string that represents a part of the url
     *                                  that should be present after redirection,
     *                                  this string is used to confirm successful
     *                                  navigation
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        //reset scope in case user was stuck inside an iFrame
        LocatorBuilder.getIFrameLocator().remove();
        ShadowLocatorBuilder.shadowDomLocator.remove();

        String modifiedTargetUrl = targetUrl;
        var baseUrl = SHAFT.Properties.web.baseURL();

        if (!baseUrl.isBlank() && targetUrl.startsWith("./")) {
            // valid use case for baseURL property ==> property is not blank && the target url starts with ./
            modifiedTargetUrl = (baseUrl.endsWith("/")) ? baseUrl + targetUrl.replace("./", "") : baseUrl + targetUrl.replace("./", "/");
        }

        String modifiedTargetUrlForLogging = modifiedTargetUrl;
        //obfuscate embedded passwords
        Matcher matcher = EMBEDDED_PASSWORD_PATTERN.matcher(modifiedTargetUrl);
        if (matcher.find()) {
            modifiedTargetUrlForLogging = modifiedTargetUrl.replaceAll(matcher.group(1), "•".repeat(matcher.group(1).length()));
        }

        String targetUrlMessage;
        if (targetUrl.equals(targetUrlAfterRedirection)) {
            targetUrlMessage = "Target URL: \"" + modifiedTargetUrlForLogging + "\"";
        } else {
            targetUrlMessage = "Target URL: \"" + modifiedTargetUrlForLogging + "\", and after redirection: \"" + targetUrlAfterRedirection + "\"";
        }
        ReportManager.logDiscrete(targetUrlMessage);

        forceStopCurrentNavigation();
        String initialURL = null;
        try {
            if (driverFactoryHelper.getDriver() == null) {
                browserActionsHelper.failAction(null, modifiedTargetUrl, new NullPointerException("WebDriver instance is null; driver initialization may have failed."));
                return this;
            }
            initialURL = driverFactoryHelper.getDriver().getCurrentUrl();
        } catch (UnsupportedCommandException exception) {
            ReportManager.logDiscrete("Could not read the current URL. Navigating to the target URL.", Level.WARN);
        }

        try {
            // remove trailing slash which may cause comparing the current and target urls
            // to fail
            if (initialURL != null) {
                if (initialURL.endsWith("/"))
                    initialURL = initialURL.substring(0, initialURL.length() - 1);
                ReportManager.logDiscrete("Initial URL: \"" + initialURL + "\"");
                if (!initialURL.equals(modifiedTargetUrl))
                    // navigate to new url
                    browserActionsHelper.navigateToNewUrl(driverFactoryHelper.getDriver(), initialURL, modifiedTargetUrl, targetUrlAfterRedirection);
                else
                    // already on the same page
                    driverFactoryHelper.getDriver().navigate().refresh();
                JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
            } else
                // navigate to new url
                browserActionsHelper.navigateToNewUrl(driverFactoryHelper.getDriver(), null, modifiedTargetUrl, targetUrlAfterRedirection);

            // validate successful navigation
            if (!targetUrl.contains("\n")) {
                // it can contain line breaks for mocked HTML pages that are used for internal testing only
                if (SHAFT.Properties.flags.forceCheckNavigationWasSuccessful()) {
                    browserActionsHelper.confirmThatWebsiteIsNotDown(driverFactoryHelper.getDriver(), modifiedTargetUrl);
                    browserActionsHelper.checkNavigationWasSuccessful(driverFactoryHelper.getDriver(), initialURL, targetUrl, targetUrlAfterRedirection);
                }
            }
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "navigateToUrl", modifiedTargetUrlForLogging);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), modifiedTargetUrlForLogging, rootCauseException);
        }
        return this;
    }

    private void forceStopCurrentNavigation() {
        try {
            JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
            ((JavascriptExecutor) driverFactoryHelper.getDriver()).executeScript("return window.stop;");
        } catch (Exception rootCauseException) {
            ReportManagerHelper.logDiscrete(rootCauseException);
            /*
             * org.openqa.selenium.NoSuchSessionException: Session ID is null. Using
             * WebDriver after calling quit()? Build info: version: '3.141.59', revision:
             * 'e82be7d358', time: '2018-11-14T08:17:03' System info: host:
             * 'gcp-test-automation-sys-187-jenkins-fullaccess', ip: '10.128.0.11', os.name:
             * 'Linux', os.arch: 'amd64', os.version: '4.15.0-1027-gcp', java.version:
             * '1.8.0_202' Driver info: driver.version: RemoteWebDriver
             */
        }
    }

    /**
     * Navigates to targetUrl using basic authentication in case the current URL
     * is different, else refreshes the current page. Waits for successfully
     * navigating to the final url after redirection.
     * <p>
     * Note: Basic authentication is supported only on Chrome and Firefox browsers,
     * for other browsers the username and password will be embedded in the URL as
     * follows: http://username:password@the_rest_of_the_url and a warning will be
     * logged
     *
     * @param targetUrl                    a string that represents the URL that you
     *                                     wish to navigate to
     * @param username                     the username to be used for basic
     *                                     authentication
     * @param password                     the password to be used for basic
     *                                     authentication
     * @param targetUrlAfterAuthentication a string that represents a part of the
     *                                     url that should be present after
     *                                     redirection, this string is used to confirm successful
     *                                     navigation
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public BrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        try {
            String domainName = browserActionsHelper.getDomainNameFromUrl(targetUrl);
            if (SHAFT.Properties.platform.executionAddress().equals("local")) {
                Predicate<URI> uriPredicate = uri -> uri.getHost().contains(domainName);
                ((HasAuthentication) driverFactoryHelper.getDriver()).register(uriPredicate, UsernameAndPassword.of(username, password));
            } else {
                AtomicReference<DevTools> devToolsAtomicReference = new AtomicReference<>();
                driverFactoryHelper.setDriver(new Augmenter().addDriverAugmentation("chrome",
                        HasAuthentication.class,
                        (caps, exec) -> (whenThisMatches, useTheseCredentials) -> {
                            devToolsAtomicReference.get()
                                    .createSessionIfThereIsNotOne();
                            devToolsAtomicReference.get().getDomains()
                                    .network()
                                    .addAuthHandler(whenThisMatches,
                                            useTheseCredentials);
                        }).augment(driverFactoryHelper.getDriver()));
                DevTools devTools = ((HasDevTools) driverFactoryHelper.getDriver()).getDevTools();
                devTools.createSession();
                devToolsAtomicReference.set(devTools);
                ((HasAuthentication) driverFactoryHelper.getDriver()).register(UsernameAndPassword.of(username, password));
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            targetUrl = browserActionsHelper.formatUrlForBasicAuthentication(username, password, targetUrl);
        }
        return navigateToURL(targetUrl, targetUrlAfterAuthentication);
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions navigateBack() {
        return performNavigationAction(NavigationAction.BACK);
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions navigateForward() {
        return performNavigationAction(NavigationAction.FORWARD);
    }

    /**
     * Attempts to refresh the current page
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions refreshCurrentPage() {
        return performNavigationAction(NavigationAction.REFRESH);
    }

    private BrowserActions performNavigationAction(NavigationAction navigationAction) {
        String initialURL;
        var newURL = "";
        try {
            initialURL = driverFactoryHelper.getDriver().getCurrentUrl();
            forceStopCurrentNavigation();
            switch (navigationAction) {
                case FORWARD -> driverFactoryHelper.getDriver().navigate().forward();
                case BACK -> driverFactoryHelper.getDriver().navigate().back();
                case REFRESH -> driverFactoryHelper.getDriver().navigate().refresh();
            }
            JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
            if (!navigationAction.equals(NavigationAction.REFRESH)) {
                browserActionsHelper.waitUntilUrlIsNot(driverFactoryHelper.getDriver(), initialURL);
                newURL = driverFactoryHelper.getDriver().getCurrentUrl();
                if (initialURL != null && !initialURL.equals(newURL)) {
                    browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Navigate " + navigationAction + " to " + newURL);
                } else {
                    browserActionsHelper.failAction(driverFactoryHelper.getDriver(), newURL);
                }
            } else {
                browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Navigate " + navigationAction + " to " + newURL);
            }
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), newURL, rootCauseException);
        }
        return this;
    }


    /**
     * Closes the current browser window
     */
    @Override
    public void closeCurrentWindow() {
        driverFactoryHelper.closeDriver();
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions maximizeWindow() {
        Dimension initialWindowSize;
        Dimension currentWindowSize;
        var targetWidth = 1920;
        var targetHeight = 1080;

        initialWindowSize = driverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize);

        String targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        String targetOperatingSystem = SHAFT.Properties.platform.targetPlatform();
        String executionAddress = SHAFT.Properties.platform.executionAddress();

        // try selenium WebDriver maximize
        currentWindowSize = browserActionsHelper.attemptMaximizeUsingSeleniumWebDriver(driverFactoryHelper.getDriver(), executionAddress, targetBrowserName,
                targetOperatingSystem);
        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            // attempt resize using toolkit
            currentWindowSize = browserActionsHelper.attemptMaximizeUsingToolkitAndJavascript(driverFactoryHelper.getDriver(), targetWidth, targetHeight);

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // happens with headless firefox browsers // remote // linux and windows
                // also happens with chrome/windows

                // attempt resize using WebDriver manage window
                currentWindowSize = browserActionsHelper.attemptMaximizeUsingSeleniumWebDriverManageWindow(driverFactoryHelper.getDriver(), targetWidth, targetHeight);
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // attempt setting window to fullscreen
                fullScreenWindow();

                currentWindowSize = driverFactoryHelper.getDriver().manage().window().getSize();
                ReportManager.logDiscrete("Window size after fullScreenWindow: " + currentWindowSize);
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                ReportManager.logDiscrete("skipping window maximization due to unknown error, marking step as passed.");
            }
        }
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "New screen size is now: " + currentWindowSize);
        return this;
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions setWindowSize(int width, int height) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;

        initialWindowSize = driverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize);

        driverFactoryHelper.getDriver().manage().window().setPosition(new Point(0, 0));
        driverFactoryHelper.getDriver().manage().window().setSize(new Dimension(width, height));
        // apparently we need to add +1 here to ensure that the new window size matches
        // the expected window size

        currentWindowSize = driverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Window size after SWD: " + currentWindowSize);

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ((JavascriptExecutor) driverFactoryHelper.getDriver()).executeScript(JavaScriptHelper.WINDOW_FOCUS.getValue());
            ((JavascriptExecutor) driverFactoryHelper.getDriver()).executeScript(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            ((JavascriptExecutor) driverFactoryHelper.getDriver()).executeScript(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(width)).replace("$HEIGHT", String.valueOf(height)));

            currentWindowSize = driverFactoryHelper.getDriver().manage().window().getSize();
            ReportManager.logDiscrete("Window size after JavascriptExecutor: " + currentWindowSize);
        }

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ReportManager.logDiscrete("skipping window resizing due to unknown error, marking step as passed.");
        }

        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "New screen size is now: " + currentWindowSize);
        return this;
    }

    /**
     * Intercepts outgoing HTTP requests matching the given predicate and replaces the actual
     * network response with the provided {@link HttpResponse}. Only supported for drivers that
     * implement {@link org.openqa.selenium.devtools.HasDevTools}.
     *
     * <p>Example:
     * <pre>{@code
     * HttpResponse mocked = new HttpResponse();
     * mocked.setStatus(200);
     * driver.browser().mock(req -> req.getUri().contains("/api/user"), mocked);
     * }</pre>
     *
     * @param requestPredicate a {@link Predicate} that identifies which requests should be intercepted
     * @param mockedResponse   the {@link HttpResponse} to return instead of the real network response
     * @return a self-reference to be used to chain actions
     */
    @Step("Mock HTTP Request")
    @Override
    public BrowserActions mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        return internalIntercept(requestPredicate, mockedResponse);
    }

    /**
     * Starts a fluent browser network interception builder.
     *
     * <p>The configured rule is scoped to the current WebDriver session and is cleared automatically
     * when the driver closes. Call {@link #clearNetworkInterceptors()} to remove all active rules
     * earlier. Only drivers that implement {@link org.openqa.selenium.devtools.HasDevTools} support
     * network interception.</p>
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser()
     *       .interceptRequest()
     *       .get()
     *       .urlContains("/api/users")
     *       .respond()
     *       .statusCode(200)
     *       .jsonBody("{\"ok\":true}")
     *       .perform();
     * }</pre>
     *
     * @return a browser network interception request builder
     */
    @Override
    public NetworkInterceptionRequestBuilder<BrowserActions> interceptRequest() {
        return new NetworkInterceptionRequestBuilder<>(this, this::registerNetworkInterceptionRule);
    }

    /**
     * Starts recording selected browser traffic into an HTTP contract.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().startContractRecording("src/test/resources/contracts/checkout.json", "/api/checkout");
     * driver.browser().navigateToURL("https://shop.example/checkout");
     * SHAFT.Contracts.stopRecording();
     * }</pre>
     *
     * @param contractFilePath destination JSON contract path
     * @param urlContains optional URL fragments used to select recorded traffic
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions startContractRecording(String contractFilePath, String... urlContains) {
        HttpContractRecorder.startRecording(contractFilePath, urlContains);
        driverFactoryHelper.startBrowserNetworkObservation();
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Started HTTP contract recording.");
        return this;
    }

    /**
     * Starts hard-assert validation of live browser responses against an HTTP contract.
     *
     * @param contractFilePath source JSON contract path
     * @param urlContains optional URL fragments used to select validated traffic
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions assertContract(String contractFilePath, String... urlContains) {
        HttpContractRecorder.startAssertMode(contractFilePath, urlContains);
        driverFactoryHelper.startBrowserNetworkObservation();
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Started HTTP contract assertion mode.");
        return this;
    }

    /**
     * Starts soft-verify validation of live browser responses against an HTTP contract.
     *
     * @param contractFilePath source JSON contract path
     * @param urlContains optional URL fragments used to select validated traffic
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions verifyContract(String contractFilePath, String... urlContains) {
        HttpContractRecorder.startVerifyMode(contractFilePath, urlContains);
        driverFactoryHelper.startBrowserNetworkObservation();
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Started HTTP contract verification mode.");
        return this;
    }

    /**
     * Replays recorded contract responses through the browser network interceptor.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().replayContract("src/test/resources/contracts/checkout.json");
     * driver.browser().navigateToURL("https://shop.example/checkout");
     * }</pre>
     *
     * @param contractFilePath source JSON contract path
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions replayContract(String contractFilePath) {
        HttpContractRecorder.browserReplayRules(contractFilePath)
                .forEach(rule -> driverFactoryHelper.registerBrowserNetworkInterceptionRule(rule));
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Loaded HTTP contract replay rules.");
        return this;
    }

    /**
     * Intercepts outgoing HTTP requests matching the given predicate and substitutes the real
     * network response with the provided {@link HttpResponse}. Functionally equivalent to
     * {@link #mock(Predicate, HttpResponse)} but semantically named for interception use-cases.
     * Only supported for drivers that implement {@link org.openqa.selenium.devtools.HasDevTools}.
     *
     * <p>Example:
     * <pre>{@code
     * HttpResponse stubbed = new HttpResponse();
     * stubbed.setStatus(503);
     * driver.browser().intercept(req -> req.getUri().contains("/api/search"), stubbed);
     * }</pre>
     *
     * @param requestPredicate a {@link Predicate} that identifies which requests should be intercepted
     * @param mockedResponse   the {@link HttpResponse} to return instead of the real network response
     * @return a self-reference to be used to chain actions
     */
    @Step("Intercept HTTP Request")
    @Override
    public BrowserActions intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        return internalIntercept(requestPredicate, mockedResponse);
    }

    /**
     * Removes all browser network interception rules registered for the current WebDriver session.
     *
     * @return a self-reference to be used to chain actions
     */
    @Step("Clear Network Interceptors")
    @Override
    public BrowserActions clearNetworkInterceptors() {
        try {
            driverFactoryHelper.clearBrowserNetworkInterceptors();
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Cleared network interceptors.");
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(rootCauseException);
        }
        return this;
    }

    private BrowserActions internalIntercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        ReportManager.logDiscrete("Configuring network interceptor for \"" + requestPredicate + "\" with a mocked response.");
        ReportManagerHelper.attach("HTTP Response", "Mocked HTTP Response", String.valueOf(mockedResponse));
        return registerNetworkInterceptionRule(
                BrowserNetworkInterceptionRule.mock(requestPredicate, request -> mockedResponse),
                "Configured network interceptor.");
    }

    BrowserActions registerNetworkInterceptionRule(BrowserNetworkInterceptionRule rule, String successMessage) {
        try {
            driverFactoryHelper.registerBrowserNetworkInterceptionRule(rule);
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), successMessage);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(rootCauseException);
        }
        return this;
    }

    /**
     * Resize the window to fill the current screen
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions fullScreenWindow() {
        Dimension initialWindowSize = driverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.width + "x" + initialWindowSize.height + ".");

        if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local")
                && SHAFT.Properties.web.headlessExecution()) {
            maximizeWindow();
        } else {
            driverFactoryHelper.getDriver().manage().window().fullscreen();
        }

        ReportManager.logDiscrete("Current Windows Size after fullScreen: " + driverFactoryHelper.getDriver().manage().window().getSize().width + "x" + driverFactoryHelper.getDriver().manage().window().getSize().height);
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), driverFactoryHelper.getDriver().getPageSource());
        return this;
    }

    /**
     * Switches focus to another window
     *
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions switchToWindow(String nameOrHandle) {
        if (driverFactoryHelper.getDriver().getWindowHandles().contains(nameOrHandle)) {
            driverFactoryHelper.getDriver().switchTo().window(nameOrHandle);
            browserActionsHelper.passAction(driverFactoryHelper.getDriver(), nameOrHandle);
        } else {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), nameOrHandle);
        }
        return this;
    }

    /**
     * Adds a cookie to the current browsing context.
     *
     * @param key   The cookie's name
     * @param value The cookie's name
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions addCookie(String key, String value) {
        driverFactoryHelper.getDriver().manage().addCookie(new Cookie(key, value));
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Add Cookie", "Key: " + key + " | Value: ********");
        return this;
    }

    /**
     * Imports API session cookies into the current browser context.
     *
     * @param api the API session to read cookies from
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions importCookiesFrom(SHAFT.API api) {
        return importCookiesFrom(api, null, null);
    }

    /**
     * Imports matching API session cookies into the current browser context.
     *
     * @param api          the API session to read cookies from
     * @param domainFilter optional exact cookie domain filter; pass {@code null} or blank to import every domain
     * @param pathFilter   optional exact cookie path filter; pass {@code null} or blank to import every path
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions importCookiesFrom(SHAFT.API api, String domainFilter, String pathFilter) {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            Objects.requireNonNull(api, "api");
            URI currentUri = requireHttpBrowserUri(driver.getCurrentUrl());
            List<Cookie> cookiesToImport = new ArrayList<>();
            for (io.restassured.http.Cookie apiCookie : api.getCookies().values()) {
                if (matchesApiCookieFilter(apiCookie, domainFilter, pathFilter)) {
                    cookiesToImport.add(toSeleniumCookie(apiCookie, currentUri.getHost()));
                }
            }
            cookiesToImport.forEach(cookie -> driver.manage().addCookie(cookie));
            browserActionsHelper.passAction(driver, "Import Cookies From API", "Imported Cookies: " + cookiesToImport.size());
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Import Cookies From API",
                    "Could not import API cookies. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Copies a selected API session header into {@code localStorage}.
     *
     * @param api        the API session to read headers from
     * @param headerName the header name to copy
     * @param storageKey the localStorage key to write
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions importHeaderToLocalStorage(SHAFT.API api, String headerName, String storageKey) {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            Objects.requireNonNull(api, "api");
            requireNonBlank(headerName, "headerName");
            requireNonBlank(storageKey, "storageKey");
            requireHttpBrowserUri(driver.getCurrentUrl());
            String headerValue = api.getHeaders().get(headerName);
            if (headerValue == null) {
                throw new IllegalArgumentException("API session does not contain header \"" + headerName + "\".");
            }
            if (!(driver instanceof JavascriptExecutor javascriptExecutor)) {
                throw new IllegalStateException("The active WebDriver does not support JavaScript execution.");
            }
            javascriptExecutor.executeScript("window.localStorage.setItem(arguments[0], arguments[1]);",
                    storageKey, headerValue);
            browserActionsHelper.passAction(driver, "Import Header To Local Storage",
                    "Header: " + headerName + " | Storage Key: " + storageKey);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Import Header To Local Storage",
                    "Could not import API header to localStorage. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Saves the current browser cookies, {@code localStorage}, and {@code sessionStorage} to a JSON file.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().saveStorageState("target/auth-state.json");
     * }</pre>
     *
     * @param filePath target JSON file path
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions saveStorageState(String filePath) {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            BrowserStorageStateManager.save(driver, filePath);
            browserActionsHelper.passAction(driver, "Save Browser Storage State", filePath);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Save Browser Storage State",
                    "Could not save browser storage state. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Loads browser cookies, {@code localStorage}, and {@code sessionStorage} from a JSON file.
     *
     * <p>Navigate to the target origin before loading storage so browser cookie domain rules can apply.
     * Example:
     * <pre>{@code
     * driver.browser()
     *       .navigateToURL("https://example.com")
     *       .and().loadStorageState("target/auth-state.json");
     * }</pre>
     *
     * @param filePath source JSON file path
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions loadStorageState(String filePath) {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            BrowserStorageStateManager.load(driver, filePath);
            browserActionsHelper.passAction(driver, "Load Browser Storage State", filePath);
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Load Browser Storage State",
                    "Could not load browser storage state. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Enables offline network mode for DevTools-capable browsers.
     *
     * <p>Unsupported drivers keep the test running and add deterministic trace metadata.
     * Example:
     * <pre>{@code
     * driver.browser().goOffline();
     * }</pre>
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions goOffline() {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            boolean applied = BrowserNetworkProfileManager.goOffline(driver);
            browserActionsHelper.passAction(driver, "Go Offline",
                    applied ? "Offline network profile applied." : "Offline network profile is unsupported.");
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Go Offline",
                    "Could not apply offline network mode. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Restores default network mode and clears blocked URL patterns for DevTools-capable browsers.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().restoreNetwork();
     * }</pre>
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions restoreNetwork() {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            boolean applied = BrowserNetworkProfileManager.restore(driver);
            browserActionsHelper.passAction(driver, "Restore Network",
                    applied ? "Network profile restored." : "Network profile restore is unsupported.");
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Restore Network",
                    "Could not restore network mode. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Applies fixed latency and throughput limits for DevTools-capable browsers.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().throttleNetwork(250, 64, 32);
     * }</pre>
     *
     * @param latencyMs    network latency in milliseconds
     * @param downloadKbps download throughput in kilobits per second
     * @param uploadKbps   upload throughput in kilobits per second
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions throttleNetwork(long latencyMs, long downloadKbps, long uploadKbps) {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            boolean applied = BrowserNetworkProfileManager.throttle(driver, latencyMs, downloadKbps, uploadKbps);
            browserActionsHelper.passAction(driver, "Throttle Network",
                    applied ? "Latency: " + latencyMs + "ms | Download: " + downloadKbps + "kbps | Upload: " + uploadKbps + "kbps"
                            : "Network throttling is unsupported.");
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Throttle Network",
                    "Could not apply network throttling. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Blocks browser requests matching DevTools URL patterns.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().blockNetworkResources("*.png", "*.jpg");
     * }</pre>
     *
     * @param urlPatterns DevTools URL patterns to block
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions blockNetworkResources(String... urlPatterns) {
        WebDriver driver = driverFactoryHelper.getDriver();
        try {
            boolean applied = BrowserNetworkProfileManager.blockResources(driver, urlPatterns);
            browserActionsHelper.passAction(driver, "Block Network Resources",
                    applied ? "Blocked URL patterns: " + String.join(", ", urlPatterns == null ? new String[0] : urlPatterns)
                            : "Network resource blocking is unsupported.");
        } catch (Exception rootCauseException) {
            browserActionsHelper.failAction(driver, "Block Network Resources",
                    "Could not block network resources. Cause: " + rootCauseException.getClass().getSimpleName());
        }
        return this;
    }

    /**
     * Gets a cookie with a given name.
     *
     * @param cookieName The cookie's name.
     * @return the cookie.
     */
    @Override
    public Cookie getCookie(String cookieName) {
        Cookie cookie = driverFactoryHelper.getDriver().manage().getCookieNamed(cookieName);
        if (cookie == null) {
            browserActionsHelper.failAction(driverFactoryHelper.getDriver(), "Get Cookie: " + cookieName);
        }
        return cookie;
    }

    /**
     * Gets all cookies for the current browsing context.
     *
     * @return A Set of cookies for the current browsing context.
     */
    @Override
    public Set<Cookie> getAllCookies() {
        Set<Cookie> cookies = driverFactoryHelper.getDriver().manage().getCookies();
        browserActionsHelper.passAction("");
        return cookies;
    }

    /**
     * Gets the cookie domain.
     *
     * @param cookieName The cookie's name.
     * @return te cookie domain;
     */
    @Override
    public String getCookieDomain(String cookieName) {
        String cookieDomain = getCookie(cookieName).getDomain();
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Get Cookie Domain with name: " + cookieName, cookieDomain);
        return cookieDomain;
    }

    /**
     * Gets the cookie value.
     *
     * @param cookieName The cookie's name.
     * @return the cookie value;
     */
    @Override
    public String getCookieValue(String cookieName) {
        String cookieValue = getCookie(cookieName).getValue();
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Get Cookie Value with name: " + cookieName, "********");
        return cookieValue;
    }

    /**
     * Gets the cookie path.
     *
     * @param cookieName The cookie's name.
     * @return the cookie path;
     */
    @Override
    public String getCookiePath(String cookieName) {
        String cookiePath = getCookie(cookieName).getPath();
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Get Cookie Path with name: " + cookieName, cookiePath);
        return cookiePath;
    }

    /**
     * Deletes the cookie data matching with the provided cookie name for the current browsing context.
     *
     * @param cookieName The name of the cookie to delete.
     * @return a self-reference to be used to chain actions.
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public BrowserActions deleteCookie(String cookieName) {
        driverFactoryHelper.getDriver().manage().deleteCookieNamed(cookieName);
        browserActionsHelper.passAction(driverFactoryHelper.getDriver(), "Delete Cookie", cookieName);
        return this;
    }

    /**
     * Deletes all the cookies of the current browsing context.
     *
     * @return a self-reference to be used to chain actions.
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public BrowserActions deleteAllCookies() {
        driverFactoryHelper.getDriver().manage().deleteAllCookies();
        browserActionsHelper.passAction("");
        return this;
    }

    /**
     * Use this action to return a full page screenshot. This is a synonym to {@link  BrowserActions#captureScreenshot(Screenshots type)} if you pass `Screenshots.FULL`
     *
     * @return a self-reference for chainable actions
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public BrowserActions captureScreenshot() {
        return this.captureScreenshot(Screenshots.FULL);
    }

    /**
     * Use this action to return a page screenshot. If you want to capture a screenshot then use this method instead {@see FluentBrowserActions#captureSnapshot()}
     *
     * @param type can either be `Screenshots.FULL`, or `Screenshots.VIEWPORT`
     * @return a self-reference for chainable actions
     */
    @Override
    public BrowserActions captureScreenshot(Screenshots type) {
        var logText = "Capture " + type.name().toLowerCase() + " screenshot";
        var screenshotManager = new ScreenshotManager();
        long profilerScreenshotStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
        byte[] screenshot = screenshotManager.takeScreenshot(driverFactoryHelper.getDriver(), null);
        if (profilerScreenshotStart != 0L) {
            FlakeProfiler.recordEvidenceCapture("screenshot", logText,
                    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerScreenshotStart));
        }
        long profilerAttachmentStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
        ReportManagerHelper.log(logText, Collections.singletonList(screenshotManager.prepareImageForReport(screenshot, "captureScreenshot")));
        if (profilerAttachmentStart != 0L) {
            FlakeProfiler.recordEvidenceCapture("report attachment", logText,
                    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerAttachmentStart));
        }
        return this;
    }

    /**
     * Use this action to return a page snapshot. A page snapshot is a single .mht file that contains the full page DOM and any related assets
     * to help you view the page as a whole. If you want to capture a screenshot then use this method instead @see FluentBrowserActions#captureScreenshot()
     *
     * @return a self-reference for chainable actions
     */
    @SuppressWarnings("UnusedReturnValue")
    @Override
    public BrowserActions captureSnapshot() {
        var logMessage = "";
        long profilerSnapshotStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
        var pageSnapshot = browserActionsHelper.capturePageSnapshot(driverFactoryHelper.getDriver());
        if (profilerSnapshotStart != 0L) {
            FlakeProfiler.recordEvidenceCapture("page snapshot", "Capture page snapshot",
                    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerSnapshotStart));
        }
        if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
            logMessage = "Capture page snapshot";
        } else if (pageSnapshot.startsWith("<html")) {
            logMessage = "Capture page HTML";
        }
        long profilerAttachmentStart = FlakeProfiler.isEnabled() ? System.nanoTime() : 0L;
        ReportManagerHelper.log(logMessage, List.of(Arrays.asList(logMessage, new ScreenshotManager().generateAttachmentFileName("captureSnapshot"), new ByteArrayInputStream(pageSnapshot.getBytes()))));
        if (profilerAttachmentStart != 0L) {
            FlakeProfiler.recordEvidenceCapture("report attachment", logMessage,
                    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - profilerAttachmentStart));
        }
        return this;
    }

    /**
     * Generates a Google Lighthouse performance report for the currently open page.
     * The report is attached to the Allure test report for review after the test run.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().navigateToURL("https://example.com")
     *       .and().generateLightHouseReport();
     * }</pre>
     */
    @Override
    public void generateLightHouseReport() {
        new LightHouseGenerateReport(driverFactoryHelper.getDriver()).generateLightHouseReport();
    }

    /**
     * Waits for the page to finish lazy-loading by polling JavaScript readiness conditions.
     * Use this after actions that trigger dynamic content loading (e.g., infinite scrolling
     * or deferred script execution).
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().navigateToURL("https://example.com")
     *       .and().waitForLazyLoading();
     * }</pre>
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions waitForLazyLoading() {
        JavaScriptWaitManager.waitForLazyLoading(driverFactoryHelper.getDriver());
        return this;
    }

    /**
     * Returns the handle for currently active context. This can be used to switch
     * to this context at a later time.
     *
     * @return The current context handle
     */
    @Override
    public String getContext() {
        String context = "";
        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            context = androidDriver.getContext();
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            context = iosDriver.getContext();
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
        return context;
    }

    /**
     * Switches focus to another context
     *
     * @param context The name of the context or the handle as returned by
     *                ElementActions.getContext(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions setContext(String context) {
        String contextBefore = currentMobileContext();
        try {
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                androidDriver.context(context);
            } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                iosDriver.context(context);
            } else {
                elementActionsHelper.failAction(driverFactoryHelper.getDriver(), context, null);
            }
        } catch (RuntimeException exception) {
            recordMobileContextSwitch(context, contextBefore, "", exception);
            throw exception;
        }
        recordMobileContextSwitch(context, contextBefore, currentMobileContext(), null);
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), context, null, null);
        return this;
    }

    /**
     * Returns a list of unique handles for all the currently open windows. This can
     * be used to switch to any of these windows at a later time.
     *
     * @return list of window handles
     */
    @Override
    public List<String> getWindowHandles() {
        List<String> windowHandles = new ArrayList<>(driverFactoryHelper.getDriver().getWindowHandles());
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
        return windowHandles;
    }

    /**
     * Returns a list of unique handles for all the currently open contexts. This
     * can be used to switch to any of these contexts at a later time.
     *
     * @return list of context handles
     */
    @Override
    public List<String> getContextHandles() {
        List<String> windowHandles = new ArrayList<>();
        if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
            windowHandles.addAll(androidDriver.getContextHandles());
        } else if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
            windowHandles.addAll(iosDriver.getContextHandles());
        } else {
            elementActionsHelper.failAction(driverFactoryHelper.getDriver(), null);
        }
        elementActionsHelper.passAction(driverFactoryHelper.getDriver(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(windowHandles), null, null);
        return windowHandles;
    }

    private String currentMobileContext() {
        try {
            if (driverFactoryHelper.getDriver() instanceof AndroidDriver androidDriver) {
                return androidDriver.getContext();
            }
            if (driverFactoryHelper.getDriver() instanceof IOSDriver iosDriver) {
                return iosDriver.getContext();
            }
            return "unsupported by active driver";
        } catch (RuntimeException ignored) {
            return "unsupported by active provider";
        }
    }

    private void recordMobileContextSwitch(String requestedContext, String contextBefore, String contextAfter,
                                           RuntimeException exception) {
        Map<String, String> metadata = new LinkedHashMap<>(MobileTraceMetadata.mobileMetadata(
                driverFactoryHelper.getDriver(), exception != null));
        metadata.put("contextBefore", contextBefore);
        metadata.put("contextAfter", contextAfter == null || contextAfter.isBlank() ? "unavailable" : contextAfter);
        metadata.put("requestedContext", requestedContext == null ? "" : requestedContext);
        TraceEventRecorder.record("mobile-context", "SET_CONTEXT", exception == null ? "passed" : "failed",
                requestedContext, driverFactoryHelper.getDriver(), "Switch mobile context to \"" + requestedContext + "\"",
                exception, metadata, List.of());
    }

    /**
     * Provides access to accessibility testing actions for the current browser session.
     *
     * <p>This method returns an {@link AccessibilityActions} instance that can be used
     * to perform automated accessibility checks and generate reports for the current page.</p>
     *
     * <p>Example usage:</p>
     * <pre>{@code
     * AccessibilityActions actions = browserActions.accessibility();
     * actions.analyzePageAccessibility("HomePage");
     * }</pre>
     *
     * @return an {@link AccessibilityActions} object tied to the current browser session
     */
    @Override
    public AccessibilityActions accessibility() {
        WebDriver rawDriver = driverFactoryHelper.getDriver();
        return new AccessibilityActions(rawDriver, this);
    }

    private static Cookie toSeleniumCookie(io.restassured.http.Cookie apiCookie, String currentHost) {
        Cookie.Builder builder = new Cookie.Builder(apiCookie.getName(), apiCookie.hasValue() ? apiCookie.getValue() : "");
        if (apiCookie.hasDomain()) {
            String cookieDomain = normalizeDomain(apiCookie.getDomain());
            if (!hostMatchesDomain(currentHost, cookieDomain)) {
                throw new IllegalArgumentException("Current browser host \"" + currentHost
                        + "\" is not compatible with cookie domain \"" + apiCookie.getDomain() + "\".");
            }
            builder.domain(cookieDomain);
        }
        builder.path(apiCookie.hasPath() ? apiCookie.getPath() : "/");
        if (apiCookie.hasExpiryDate()) {
            builder.expiresOn(apiCookie.getExpiryDate());
        }
        builder.isSecure(apiCookie.isSecured());
        builder.isHttpOnly(apiCookie.isHttpOnly());
        if (apiCookie.hasSameSite()) {
            builder.sameSite(apiCookie.getSameSite());
        }
        return builder.build();
    }

    private static URI requireHttpBrowserUri(String currentUrl) {
        if (currentUrl == null || currentUrl.isBlank()) {
            throw new IllegalStateException("Navigate to an HTTP or HTTPS page before importing auth state.");
        }
        URI currentUri = URI.create(currentUrl);
        String scheme = currentUri.getScheme();
        if (currentUri.getHost() == null
                || scheme == null
                || (!scheme.equalsIgnoreCase("http") && !scheme.equalsIgnoreCase("https"))) {
            throw new IllegalStateException("Navigate to an HTTP or HTTPS page before importing auth state.");
        }
        return currentUri;
    }

    private static void requireNonBlank(String value, String fieldName) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(fieldName + " must not be blank.");
        }
    }

    private static boolean matchesApiCookieFilter(io.restassured.http.Cookie cookie, String domainFilter, String pathFilter) {
        return matchesFilter(cookie.hasDomain() ? cookie.getDomain() : null, domainFilter, true)
                && matchesFilter(cookie.hasPath() ? cookie.getPath() : null, pathFilter, false);
    }

    private static boolean matchesFilter(String actual, String filter, boolean domain) {
        if (filter == null || filter.isBlank()) {
            return true;
        }
        if (actual == null || actual.isBlank()) {
            return false;
        }
        String normalizedActual = domain ? normalizeDomain(actual) : actual;
        String normalizedFilter = domain ? normalizeDomain(filter) : filter;
        return normalizedActual.equals(normalizedFilter);
    }

    private static boolean hostMatchesDomain(String host, String domain) {
        String normalizedHost = normalizeDomain(host);
        return normalizedHost.equals(domain) || normalizedHost.endsWith("." + domain);
    }

    private static String normalizeDomain(String domain) {
        String normalized = domain.toLowerCase(Locale.ROOT);
        while (normalized.startsWith(".")) {
            normalized = normalized.substring(1);
        }
        return normalized;
    }

}
