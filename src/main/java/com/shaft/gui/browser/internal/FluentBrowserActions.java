package com.shaft.gui.browser.internal;

import com.google.common.base.Throwables;
import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.enums.internal.NavigationAction;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.element.internal.ElementActionsHelper;
import com.shaft.gui.element.internal.FluentElementActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.performance.internal.LightHouseGenerateReport;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.WebDriverBrowserValidationsBuilder;
import org.openqa.selenium.*;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

@SuppressWarnings("unused")
public class FluentBrowserActions {
    private static final ThreadLocal<FluentBrowserActions> INSTANCE = new ThreadLocal<>();

    protected FluentBrowserActions() {
    }

    public synchronized static FluentBrowserActions getInstance() {
        JavaScriptWaitManager.waitForLazyLoading();
        if (INSTANCE.get() == null) {
            INSTANCE.set(new FluentBrowserActions());
        }
        return INSTANCE.get();
    }

    public TouchActions performTouchAction() {
        return new TouchActions();
    }

    public AlertActions performAlertAction() {
        return new AlertActions();
    }

    public FluentElementActions performElementAction() {
        return FluentElementActions.getInstance();
    }

    public TouchActions touch() {
        return new TouchActions();
    }

    public AlertActions alert() {
        return new AlertActions();
    }

    public FluentElementActions element() {
        return FluentElementActions.getInstance();
    }

    public FluentBrowserActions and() {
        return this;
    }

    public WebDriverBrowserValidationsBuilder assertThat() {
        return new WizardHelpers.WebDriverAssertions().browser();
    }

    public WebDriverBrowserValidationsBuilder verifyThat() {
        return new WizardHelpers.WebDriverVerifications().browser();
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
    public FluentBrowserActions capturePageSnapshot() {
        var serializedPageData = BrowserActionsHelpers.capturePageSnapshot(DriverFactoryHelper.getDriver().get());
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), serializedPageData);
        return this;
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @return the URL that's currently open in the current page
     */
    public String getCurrentURL() {
        var currentURL = "";
        try {
            currentURL = DriverFactoryHelper.getDriver().get().getCurrentUrl();
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), currentURL);
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), currentURL, rootCauseException);
        }
        return currentURL;
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @return the title of the current window
     */
    public String getCurrentWindowTitle() {
        var currentWindowTitle = "";
        try {
            currentWindowTitle = DriverFactoryHelper.getDriver().get().getTitle();
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), currentWindowTitle);
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), currentWindowTitle, rootCauseException);
        }
        return currentWindowTitle;
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @return the source of the current page
     */
    public String getPageSource() {
        var pageSource = "";
        try {
            pageSource = DriverFactoryHelper.getDriver().get().getPageSource();
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), pageSource);
        } catch (org.openqa.selenium.JavascriptException javascriptException) {
            //try again
            JavaScriptWaitManager.waitForLazyLoading();
            return getPageSource();
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), pageSource, rootCauseException);
        }
        return pageSource;
    }

    /**
     * Gets the current window handle and returns it as a string
     *
     * @return the window handle for the current window
     */
    public String getWindowHandle() {
        var windowHandle = "";
        try {
            windowHandle = DriverFactoryHelper.getDriver().get().getWindowHandle();
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), windowHandle);
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), windowHandle, rootCauseException);
        }
        return windowHandle;
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @return the position of the current window
     */
    public String getWindowPosition() {
        var windowPosition = "";
        try {
            windowPosition = DriverFactoryHelper.getDriver().get().manage().window().getPosition().toString();
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), windowPosition);
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), windowPosition, rootCauseException);
        }
        return windowPosition;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the size of the current window
     */
    public String getWindowSize() {
        var windowSize = "";
        try {
            windowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize().toString();
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), windowSize);
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), windowSize, rootCauseException);
        }
        return windowSize;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the height of the current window
     */
    public String getWindowHeight() {
        var windowHeight = "";
        try {
            windowHeight = String.valueOf(DriverFactoryHelper.getDriver().get().manage().window().getSize().getHeight());
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), windowHeight, rootCauseException);
        }
        return windowHeight;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the width of the current window
     */
    public String getWindowWidth() {
        var windowWidth = "";
        try {
            windowWidth = String.valueOf(DriverFactoryHelper.getDriver().get().manage().window().getSize().getWidth());
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), windowWidth, rootCauseException);
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
    public FluentBrowserActions navigateToURL(String targetUrl) {
        return navigateToURL(targetUrl, targetUrl);
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
    public FluentBrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        String modifiedTargetUrl = targetUrl;
        var baseUrl = SHAFT.Properties.web.baseURL();

        if (!baseUrl.isBlank() && targetUrl.startsWith("./")) {
            // valid use case for baseURL property ==> property is not blank && the target url starts with ./
            modifiedTargetUrl = (baseUrl.endsWith("/")) ? baseUrl + targetUrl.replace("./", "") : baseUrl + targetUrl.replace("./", "/");
        }

        if (targetUrl.equals(targetUrlAfterRedirection)) {
            ReportManager.logDiscrete(
                    "Target URL: \"" + modifiedTargetUrl + "\"");
        } else {
            ReportManager.logDiscrete(
                    "Target URL: \"" + modifiedTargetUrl + "\", and after redirection: \"" + targetUrlAfterRedirection + "\"");
        }
//         force stop any current navigation
        //noinspection SpellCheckingInspection
        try {
            ((JavascriptExecutor) DriverFactoryHelper.getDriver().get()).executeScript("return window.stop;");
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
        try {
            JavaScriptWaitManager.waitForLazyLoading();
            String initialSource = DriverFactoryHelper.getDriver().get().getPageSource();
            String initialURL = DriverFactoryHelper.getDriver().get().getCurrentUrl();
            // remove trailing slash which may cause comparing the current and target urls
            // to fail
            if (initialURL.endsWith("/")) {
                initialURL = initialURL.substring(0, initialURL.length() - 1);
            }
            ReportManager.logDiscrete("Initial URL: \"" + initialURL + "\"");
            if (!initialURL.equals(modifiedTargetUrl)) {
                // navigate to new url
                BrowserActionsHelpers.navigateToNewURL(DriverFactoryHelper.getDriver().get(), initialURL, modifiedTargetUrl, targetUrlAfterRedirection);
                JavaScriptWaitManager.waitForLazyLoading();
                if ((ElementActionsHelper.getElementsCount(DriverFactoryHelper.getDriver().get(), By.tagName("html")) == 1)
                        && (!DriverFactoryHelper.getDriver().get().getPageSource().equalsIgnoreCase(initialSource))) {
                    BrowserActionsHelpers.confirmThatWebsiteIsNotDown(DriverFactoryHelper.getDriver().get(), modifiedTargetUrl);
                    BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), modifiedTargetUrl);
                } else {
                    BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), modifiedTargetUrl);
                }
            } else {
                // already on the same page
                DriverFactoryHelper.getDriver().get().navigate().refresh();
                JavaScriptWaitManager.waitForLazyLoading();
                if (ElementActionsHelper.getElementsCount(DriverFactoryHelper.getDriver().get(), By.tagName("html")) == 1) {
                    BrowserActionsHelpers.confirmThatWebsiteIsNotDown(DriverFactoryHelper.getDriver().get(), modifiedTargetUrl);
                    BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), modifiedTargetUrl);
                }
            }
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), modifiedTargetUrl, rootCauseException);
        }
        return this;
    }

    @SuppressWarnings("UnusedReturnValue")
    public FluentBrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        try {
            String domainName = BrowserActionsHelpers.getDomainNameFromURL(targetUrl);
            if (SHAFT.Properties.platform.executionAddress().equals("local")) {
                Predicate<URI> uriPredicate = uri -> uri.getHost().contains(domainName);
                ((HasAuthentication) DriverFactoryHelper.getDriver().get()).register(uriPredicate, UsernameAndPassword.of(username, password));
            } else {
                AtomicReference<DevTools> devToolsAtomicReference = new AtomicReference<>();
                DriverFactoryHelper.getDriver().set(new Augmenter().addDriverAugmentation("chrome",
                        HasAuthentication.class,
                        (caps, exec) -> (whenThisMatches, useTheseCredentials) -> {
                            devToolsAtomicReference.get()
                                    .createSessionIfThereIsNotOne();
                            devToolsAtomicReference.get().getDomains()
                                    .network()
                                    .addAuthHandler(whenThisMatches,
                                            useTheseCredentials);
                        }).augment(DriverFactoryHelper.getDriver().get()));
                DevTools devTools = ((HasDevTools) DriverFactoryHelper.getDriver().get()).getDevTools();
                devTools.createSession();
                devToolsAtomicReference.set(devTools);
                ((HasAuthentication) DriverFactoryHelper.getDriver().get()).register(UsernameAndPassword.of(username, password));
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            targetUrl = BrowserActionsHelpers.formatURLForBasicAuthentication(username, password, targetUrl);
        }
        return navigateToURL(targetUrl, targetUrlAfterAuthentication);
    }

    /**
     * Navigates one step back from the browsers history
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions navigateBack() {
        return performNavigationAction(NavigationAction.BACK);
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions navigateForward() {
        return performNavigationAction(NavigationAction.FORWARD);
    }

    /**
     * Attempts to refresh the current page
     *
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions refreshCurrentPage() {
        return performNavigationAction(NavigationAction.REFRESH);
    }

    private FluentBrowserActions performNavigationAction(NavigationAction navigationAction) {
        String initialURL;
        var newURL = "";
        try {
            initialURL = DriverFactoryHelper.getDriver().get().getCurrentUrl();
            switch (navigationAction) {
                case FORWARD -> DriverFactoryHelper.getDriver().get().navigate().forward();
                case BACK -> DriverFactoryHelper.getDriver().get().navigate().back();
                case REFRESH -> DriverFactoryHelper.getDriver().get().navigate().refresh();
            }
            JavaScriptWaitManager.waitForLazyLoading();
            if (!navigationAction.equals(NavigationAction.REFRESH)) {
                BrowserActionsHelpers.waitUntilURLIsNot(DriverFactoryHelper.getDriver().get(), initialURL);
                newURL = DriverFactoryHelper.getDriver().get().getCurrentUrl();
                if (!newURL.equals(initialURL)) {
                    BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), newURL);
                } else {
                    BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), newURL);
                }
            } else {
                BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), newURL);
            }
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), newURL, rootCauseException);
        }
        return this;
    }


    /**
     * Closes the current browser window
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions closeCurrentWindow() {
        if (DriverFactoryHelper.getDriver().get() != null) {
            try {
                // TODO: handle session timeout while attempting to close empty window
                String lastPageSource = DriverFactoryHelper.getDriver().get().getPageSource();
                DriverFactory.closeAllDrivers();
                BrowserActionsHelpers.passAction(lastPageSource);
            } catch (WebDriverException rootCauseException) {
                if (rootCauseException.getMessage() != null
                        && (rootCauseException.getMessage().contains("was terminated due to TIMEOUT") || rootCauseException.getMessage().contains("Session ID is null"))) {
                    BrowserActionsHelpers.passAction(null);
                } else {
                    BrowserActionsHelpers.failAction(rootCauseException);
                }
            } catch (Exception rootCauseException) {
                BrowserActionsHelpers.failAction(rootCauseException);
            }
        } else {
            ReportManager.logDiscrete("Window is already closed and driver object is null.");
            BrowserActionsHelpers.passAction(null);
        }
        return this;
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions maximizeWindow() {
        Dimension initialWindowSize;
        Dimension currentWindowSize;
        var targetWidth = 1920;
        var targetHeight = 1080;

        initialWindowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

        String targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        String targetOperatingSystem = SHAFT.Properties.platform.targetPlatform();
        String executionAddress = SHAFT.Properties.platform.executionAddress();

        // try selenium WebDriver maximize
        currentWindowSize = BrowserActionsHelpers.attemptMaximizeUsingSeleniumWebDriver(DriverFactoryHelper.getDriver().get(), executionAddress, targetBrowserName,
                targetOperatingSystem);
        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            // attempt resize using toolkit
            currentWindowSize = BrowserActionsHelpers.attemptMaximizeUsingToolkitAndJavascript(DriverFactoryHelper.getDriver().get(), targetWidth, targetHeight);

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // happens with headless firefox browsers // remote // linux and windows
                // also happens with chrome/windows

                // attempt resize using WebDriver manage window
                currentWindowSize = BrowserActionsHelpers.attemptMaximizeUsingSeleniumWebDriverManageWindow(DriverFactoryHelper.getDriver().get(), targetWidth, targetHeight);
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // attempt setting window to fullscreen
                fullScreenWindow();

                currentWindowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize();
                ReportManager.logDiscrete("Window size after fullScreenWindow: " + currentWindowSize.toString());
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                ReportManager.logDiscrete("skipping window maximization due to unknown error, marking step as passed.");
            }
        }
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "New screen size is now: " + currentWindowSize);
        return this;
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions setWindowSize(int width, int height) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;

        initialWindowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

        DriverFactoryHelper.getDriver().get().manage().window().setPosition(new Point(0, 0));
        DriverFactoryHelper.getDriver().get().manage().window().setSize(new Dimension(width, height));
        // apparently we need to add +1 here to ensure that the new window size matches
        // the expected window size

        currentWindowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize();
        ReportManager.logDiscrete("Window size after SWD: " + currentWindowSize.toString());

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ((JavascriptExecutor) DriverFactoryHelper.getDriver().get()).executeScript(JavaScriptHelper.WINDOW_FOCUS.getValue());
            ((JavascriptExecutor) DriverFactoryHelper.getDriver().get()).executeScript(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            ((JavascriptExecutor) DriverFactoryHelper.getDriver().get()).executeScript(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(width)).replace("$HEIGHT", String.valueOf(height)));

            currentWindowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize();
            ReportManager.logDiscrete("Window size after JavascriptExecutor: " + currentWindowSize.toString());
        }

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ReportManager.logDiscrete("skipping window resizing due to unknown error, marking step as passed.");
        }

        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "New screen size is now: " + currentWindowSize);
        return this;
    }

    /**
     * Resize the window to fill the current screen
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions fullScreenWindow() {
        Dimension initialWindowSize = DriverFactoryHelper.getDriver().get().manage().window().getSize();
        ReportManager.logDiscrete("Initial Windows Size: " + initialWindowSize.width + "x" + initialWindowSize.height);

        if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local")
                && SHAFT.Properties.web.headlessExecution()) {
            maximizeWindow();
        } else {
            DriverFactoryHelper.getDriver().get().manage().window().fullscreen();
        }

        ReportManager.logDiscrete("Current Windows Size after fullScreen: " + DriverFactoryHelper.getDriver().get().manage().window().getSize().width + "x" + DriverFactoryHelper.getDriver().get().manage().window().getSize().height);
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), DriverFactoryHelper.getDriver().get().getPageSource());
        return this;
    }


    /**
     * Switches focus to another Tab
     *
     * @param url The name of the URL you want to navigate to
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentBrowserActions switchToNewTab(String url) {
        try {
            var handleBeforeNavigation = DriverFactoryHelper.getDriver().get().getWindowHandle();
            DriverFactoryHelper.getDriver().get().switchTo().newWindow(WindowType.TAB).navigate().to(url);
            JavaScriptWaitManager.waitForLazyLoading();
            var handleAfterNavigation = DriverFactoryHelper.getDriver().get().getWindowHandle();
            if (!handleBeforeNavigation.equals(handleAfterNavigation)) {
                ReportManager.logDiscrete("Old Tab Handle: \"" + handleBeforeNavigation + "\", New Tab handle : \"" + handleAfterNavigation + "\"");
                BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), url);
            } else {
                BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), url);
            }
        } catch (Exception rootCauseException) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), url, rootCauseException);
        }
        return this;
    }


    /**
     * Switches focus to another window
     *
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentBrowserActions switchToWindow(String nameOrHandle) {
        if (DriverFactoryHelper.getDriver().get().getWindowHandles().contains(nameOrHandle)) {
            DriverFactoryHelper.getDriver().get().switchTo().window(nameOrHandle);
            BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), nameOrHandle);
        } else {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), nameOrHandle);
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
    public FluentBrowserActions addCookie(String key, String value) {
        DriverFactoryHelper.getDriver().get().manage().addCookie(new Cookie(key, value));
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "Add Cookie", "Key: " + key + " | Value: " + value);
        return this;
    }

    /**
     * Gets a cookie with a given name.
     *
     * @param cookieName The cookie's name.
     * @return the cookie.
     */
    public Cookie getCookie(String cookieName) {
        Cookie cookie = DriverFactoryHelper.getDriver().get().manage().getCookieNamed(cookieName);
        if (cookie == null) {
            BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), "Get Cookie: " + cookieName);
        }
        return cookie;
    }

    /**
     * Gets all cookies for the current browsing context.
     *
     * @return A Set of cookies for the current browsing context.
     */
    public Set<Cookie> getAllCookies() {
        Set<Cookie> cookies = DriverFactoryHelper.getDriver().get().manage().getCookies();
        BrowserActionsHelpers.passAction("");
        return cookies;
    }

    /**
     * Gets the cookie domain.
     *
     * @param cookieName The cookie's name.
     * @return te cookie domain;
     */
    public String getCookieDomain(String cookieName) {
        String cookieDomain = getCookie(cookieName).getDomain();
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "Get Cookie Domain with name: " + cookieName, cookieDomain);
        return cookieDomain;
    }

    /**
     * Gets the cookie value.
     *
     * @param cookieName The cookie's name.
     * @return the cookie value;
     */
    public String getCookieValue(String cookieName) {
        String cookieValue = getCookie(cookieName).getValue();
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "Get Cookie Value with name: " + cookieName, cookieValue);
        return cookieValue;
    }

    /**
     * Gets the cookie path.
     *
     * @param cookieName The cookie's name.
     * @return the cookie path;
     */
    public String getCookiePath(String cookieName) {
        String cookiePath = getCookie(cookieName).getPath();
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "Get Cookie Path with name: " + cookieName, cookiePath);
        return cookiePath;
    }

    /**
     * Deletes the cookie data matching with the provided cookie name for the current browsing context.
     *
     * @param cookieName The name of the cookie to delete.
     * @return a self-reference to be used to chain actions.
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentBrowserActions deleteCookie(String cookieName) {
        DriverFactoryHelper.getDriver().get().manage().deleteCookieNamed(cookieName);
        BrowserActionsHelpers.passAction(DriverFactoryHelper.getDriver().get(), "Delete Cookie", cookieName);
        return this;
    }

    /**
     * Deletes all the cookies of the current browsing context.
     *
     * @return a self-reference to be used to chain actions.
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentBrowserActions deleteAllCookies() {
        DriverFactoryHelper.getDriver().get().manage().deleteAllCookies();
        BrowserActionsHelpers.passAction("");
        return this;
    }

    /**
     * Use this action to return a full page screenshot. This is a synonym to {@link  FluentBrowserActions#captureScreenshot(Screenshots type)} if you pass `Screenshots.FULL`
     *
     * @return a self-reference for chainable actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public FluentBrowserActions captureScreenshot() {
        return this.captureScreenshot(Screenshots.FULL);
    }

    /**
     * Use this action to return a page screenshot. If you want to capture a screenshot then use this method instead {@see FluentBrowserActions#captureSnapshot()}
     *
     * @param type can either be `Screenshots.FULL`, or `Screenshots.VIEWPORT`
     * @return a self-reference for chainable actions
     */
    public FluentBrowserActions captureScreenshot(Screenshots type) {
        var logText = "Capture " + type.getValue().toLowerCase() + " screenshot";
        switch (type) {
            case FULL ->
                    ReportManagerHelper.log(logText, Collections.singletonList(ScreenshotManager.prepareImageForReport(ScreenshotManager.takeFullPageScreenshot(DriverFactoryHelper.getDriver().get()), "captureScreenshot")));
            case VIEWPORT ->
                    ReportManagerHelper.log(logText, Collections.singletonList(ScreenshotManager.prepareImageForReport(ScreenshotManager.takeViewportScreenshot(DriverFactoryHelper.getDriver().get()), "captureScreenshot")));
            case ELEMENT ->
                    BrowserActionsHelpers.failAction(DriverFactoryHelper.getDriver().get(), "Were you trying to use driver.element().captureScreenshot() instead?");
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
    public FluentBrowserActions captureSnapshot() {
        var logMessage = "";
        var pageSnapshot = BrowserActionsHelpers.capturePageSnapshot(DriverFactoryHelper.getDriver().get());
        if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
            logMessage = "Capture page snapshot";
        } else if (pageSnapshot.startsWith("<html")) {
            logMessage = "Capture page HTML";
        }
        ReportManagerHelper.log(logMessage, List.of(Arrays.asList(logMessage, ScreenshotManager.generateAttachmentFileName("captureSnapshot"), new ByteArrayInputStream(pageSnapshot.getBytes()))));
        return this;
    }

    public void generateLightHouseReport() {
        new LightHouseGenerateReport(DriverFactoryHelper.getDriver().get()).generateLightHouseReport();
    }

    /**
     * Switches focus to a certain iFrame, is mainly used in coordination with
     * {@link #switchToDefaultContent()} to navigate inside any iFrame
     * layer and go back to the main page
     *
     * @param elementLocator the locator of the iFrame webElement under test (By
     *                       xpath, id, selector, name ...etc)
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions switchToIframe(By elementLocator) {
        try {
            DriverFactoryHelper.getDriver().get().switchTo().frame(((WebElement) ElementActionsHelper.identifyUniqueElement(DriverFactoryHelper.getDriver().get(), elementLocator).get(1)));
            // note to self: remove elementLocator in case of bug in screenshot manager
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), String.valueOf(elementLocator), null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Throwable throwable) {
            // has to be throwable to catch assertion errors in case element was not found
            if (Throwables.getRootCause(throwable).getClass().getName().equals(org.openqa.selenium.NoSuchElementException.class.getName())) {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), null, throwable);
            } else {
                ElementActionsHelper.failAction(DriverFactoryHelper.getDriver().get(), elementLocator, throwable);
            }
        }
        return this;
    }

    /**
     * Switches focus to default content, is mainly used in coordination with
     * {@link #switchToIframe(By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions switchToDefaultContent() {
        try {
            DriverFactoryHelper.getDriver().get().switchTo().defaultContent();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
//            failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
//        return new FluentElementActions(Objects.requireNonNull(DriverFactoryHelper.getDriver()).get());
        return this;
    }

    /**
     * Switches focus to parent frame, is mainly used in coordination with
     * {@link #switchToIframe(By)} to exit any iFrame layer and go back
     * to the main page
     *
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions switchToParentFrame() {
        try {
            DriverFactoryHelper.getDriver().get().switchTo().parentFrame();
            boolean discreetLoggingState = ReportManagerHelper.getDiscreteLogging();
            ReportManagerHelper.setDiscreteLogging(true);
            ElementActionsHelper.passAction(DriverFactoryHelper.getDriver().get(), null, Thread.currentThread().getStackTrace()[1].getMethodName(), null, null, null);
            ReportManagerHelper.setDiscreteLogging(discreetLoggingState);
        } catch (Exception rootCauseException) {
//            failAction(DriverFactoryHelper.getDriver().get(), null, rootCauseException);
        }
        // if there is no last used driver or no drivers in the drivers list, do
        // nothing...
//        return new FluentElementActions(Objects.requireNonNull(DriverFactoryHelper.getDriver()).get());
        return this;
    }

    /**
     * gets the current frame
     *
     * @return currentFrame the current frame name
     */
    public String getCurrentFrame() {
        String currentFrame = null;
        try {
            JavascriptExecutor jsExecutor = (JavascriptExecutor) DriverFactoryHelper.getDriver().get();
            currentFrame = (String) jsExecutor.executeScript("return self.name");
            ReportManager.logDiscrete("The current frame is :- " + currentFrame);
        } catch (Exception rootCauseException) {
            ReportManager.logDiscrete(String.valueOf(rootCauseException));
        }
        return currentFrame;
    }

}