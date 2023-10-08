package com.shaft.gui.browser;

import com.shaft.driver.DriverFactory;
import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactoryHelper;
import com.shaft.driver.internal.WizardHelpers;
import com.shaft.enums.internal.NavigationAction;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.internal.BrowserActionsHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.ElementActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.performance.internal.LightHouseGenerateReport;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.internal.WebDriverBrowserValidationsBuilder;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.*;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.devtools.NetworkInterceptor;
import org.openqa.selenium.html5.LocalStorage;
import org.openqa.selenium.html5.SessionStorage;
import org.openqa.selenium.html5.WebStorage;
import org.openqa.selenium.remote.Augmenter;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;
import org.openqa.selenium.remote.http.Route;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

@SuppressWarnings("unused")
public class BrowserActions {
    private static final ThreadLocal<BrowserActions> INSTANCE = new ThreadLocal<>();

    public BrowserActions() {
    }

    @Deprecated
    public BrowserActions(WebDriver driver) {
    }

    public static BrowserActions getInstance() {
        JavaScriptWaitManager.waitForLazyLoading();
        if (INSTANCE.get() == null) {
            INSTANCE.set(new BrowserActions());
        }
        return INSTANCE.get();
    }

    public TouchActions performTouchAction() {
        return new TouchActions();
    }

    public AlertActions performAlertAction() {
        return new AlertActions();
    }

    public ElementActions performElementAction() {
        return ElementActions.getInstance();
    }

    public TouchActions touch() {
        return new TouchActions();
    }

    public AlertActions alert() {
        return new AlertActions();
    }

    public ElementActions element() {
        return ElementActions.getInstance();
    }

    public BrowserActions and() {
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
    public BrowserActions capturePageSnapshot() {
        var serializedPageData = BrowserActionsHelper.capturePageSnapshot(DriverFactoryHelper.getDriver());
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), serializedPageData);
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
            currentURL = DriverFactoryHelper.getDriver().getCurrentUrl();
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), currentURL);
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), currentURL, rootCauseException);
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
            currentWindowTitle = DriverFactoryHelper.getDriver().getTitle();
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), currentWindowTitle);
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), currentWindowTitle, rootCauseException);
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
            pageSource = DriverFactoryHelper.getDriver().getPageSource();
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), pageSource);
        } catch (org.openqa.selenium.JavascriptException javascriptException) {
            //try again
            JavaScriptWaitManager.waitForLazyLoading();
            return getPageSource();
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), pageSource, rootCauseException);
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
            windowHandle = DriverFactoryHelper.getDriver().getWindowHandle();
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), windowHandle);
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), windowHandle, rootCauseException);
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
            windowPosition = DriverFactoryHelper.getDriver().manage().window().getPosition().toString();
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), windowPosition);
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), windowPosition, rootCauseException);
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
            windowSize = DriverFactoryHelper.getDriver().manage().window().getSize().toString();
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), windowSize);
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), windowSize, rootCauseException);
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
            windowHeight = String.valueOf(DriverFactoryHelper.getDriver().manage().window().getSize().getHeight());
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), windowHeight, rootCauseException);
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
            windowWidth = String.valueOf(DriverFactoryHelper.getDriver().manage().window().getSize().getWidth());
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), windowWidth, rootCauseException);
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
    public BrowserActions navigateToURL(String targetUrl) {
        return navigateToURL(targetUrl, targetUrl);
    }

    public BrowserActions navigateToURL(String targetUrl, WindowType windowType) {
        var handleBeforeNavigation = DriverFactoryHelper.getDriver().getWindowHandle();
        try {
            switch (windowType) {
                case TAB ->
                        DriverFactoryHelper.getDriver().switchTo().newWindow(WindowType.TAB).navigate().to(targetUrl);
                case WINDOW ->
                        DriverFactoryHelper.getDriver().switchTo().newWindow(WindowType.WINDOW).navigate().to(targetUrl);
            }
            JavaScriptWaitManager.waitForLazyLoading();
            var handleAfterNavigation = DriverFactoryHelper.getDriver().getWindowHandle();
            if (!handleBeforeNavigation.equals(handleAfterNavigation)) {
                ReportManager.logDiscrete("Old Tab Handle: \"" + handleBeforeNavigation + "\", New Tab handle : \"" + handleAfterNavigation + "\"");
                BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), targetUrl);
            } else {
                BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), targetUrl);
            }
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), targetUrl, rootCauseException);
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
    public BrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        //reset scope in case user was stuck inside an iFrame
        LocatorBuilder.setIFrameLocator(null);
        ShadowLocatorBuilder.shadowDomLocator = null;

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
            ((JavascriptExecutor) DriverFactoryHelper.getDriver()).executeScript("return window.stop;");
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
            String initialURL = "";

            if (DriverFactoryHelper.getDriver() instanceof AppiumDriver appiumDriver) {
                initialURL = appiumDriver.getCurrentUrl();
            } else {
                initialURL = DriverFactoryHelper.getDriver().getCurrentUrl();
            }

            // remove trailing slash which may cause comparing the current and target urls
            // to fail
            if (initialURL.endsWith("/")) {
                initialURL = initialURL.substring(0, initialURL.length() - 1);
            }
            ReportManager.logDiscrete("Initial URL: \"" + initialURL + "\"");
            if (!initialURL.equals(modifiedTargetUrl)) {
                // navigate to new url
                BrowserActionsHelper.navigateToNewUrl(DriverFactoryHelper.getDriver(), initialURL, modifiedTargetUrl, targetUrlAfterRedirection);
            } else {
                // already on the same page
                DriverFactoryHelper.getDriver().navigate().refresh();
            }
            JavaScriptWaitManager.waitForLazyLoading();
            if (!targetUrl.contains("\n")) {
                // it can contain line breaks for mocked HTML pages that are used for internal testing only
                BrowserActionsHelper.confirmThatWebsiteIsNotDown(DriverFactoryHelper.getDriver(), modifiedTargetUrl);
            }
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), modifiedTargetUrl);
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), modifiedTargetUrl, rootCauseException);
        }
        return this;
    }

    @SuppressWarnings("UnusedReturnValue")
    public BrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        try {
            String domainName = BrowserActionsHelper.getDomainNameFromUrl(targetUrl);
            if (SHAFT.Properties.platform.executionAddress().equals("local")) {
                Predicate<URI> uriPredicate = uri -> uri.getHost().contains(domainName);
                ((HasAuthentication) DriverFactoryHelper.getDriver()).register(uriPredicate, UsernameAndPassword.of(username, password));
            } else {
                AtomicReference<DevTools> devToolsAtomicReference = new AtomicReference<>();
                DriverFactoryHelper.setDriver(new Augmenter().addDriverAugmentation("chrome",
                        HasAuthentication.class,
                        (caps, exec) -> (whenThisMatches, useTheseCredentials) -> {
                            devToolsAtomicReference.get()
                                    .createSessionIfThereIsNotOne();
                            devToolsAtomicReference.get().getDomains()
                                    .network()
                                    .addAuthHandler(whenThisMatches,
                                            useTheseCredentials);
                        }).augment(DriverFactoryHelper.getDriver()));
                DevTools devTools = ((HasDevTools) DriverFactoryHelper.getDriver()).getDevTools();
                devTools.createSession();
                devToolsAtomicReference.set(devTools);
                ((HasAuthentication) DriverFactoryHelper.getDriver()).register(UsernameAndPassword.of(username, password));
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            targetUrl = BrowserActionsHelper.formatUrlForBasicAuthentication(username, password, targetUrl);
        }
        return navigateToURL(targetUrl, targetUrlAfterAuthentication);
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions navigateBack() {
        return performNavigationAction(NavigationAction.BACK);
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions navigateForward() {
        return performNavigationAction(NavigationAction.FORWARD);
    }

    /**
     * Attempts to refresh the current page
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions refreshCurrentPage() {
        return performNavigationAction(NavigationAction.REFRESH);
    }

    private BrowserActions performNavigationAction(NavigationAction navigationAction) {
        String initialURL;
        var newURL = "";
        try {
            initialURL = DriverFactoryHelper.getDriver().getCurrentUrl();
            switch (navigationAction) {
                case FORWARD -> DriverFactoryHelper.getDriver().navigate().forward();
                case BACK -> DriverFactoryHelper.getDriver().navigate().back();
                case REFRESH -> DriverFactoryHelper.getDriver().navigate().refresh();
            }
            JavaScriptWaitManager.waitForLazyLoading();
            if (!navigationAction.equals(NavigationAction.REFRESH)) {
                BrowserActionsHelper.waitUntilUrlIsNot(DriverFactoryHelper.getDriver(), initialURL);
                newURL = DriverFactoryHelper.getDriver().getCurrentUrl();
                if (!newURL.equals(initialURL)) {
                    BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Navigate " + navigationAction + " to " + newURL);
                } else {
                    BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), newURL);
                }
            } else {
                BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Navigate " + navigationAction + " to " + newURL);
            }
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), newURL, rootCauseException);
        }
        return this;
    }


    /**
     * Closes the current browser window
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions closeCurrentWindow() {
        if (DriverFactoryHelper.getDriver() != null) {
            try {
                // TODO: handle session timeout while attempting to close empty window
                String lastPageSource = DriverFactoryHelper.getDriver().getPageSource();
                DriverFactory.closeAllDrivers();
                BrowserActionsHelper.passAction(lastPageSource);
            } catch (WebDriverException rootCauseException) {
                if (rootCauseException.getMessage() != null
                        && (rootCauseException.getMessage().contains("was terminated due to TIMEOUT") || rootCauseException.getMessage().contains("Session ID is null"))) {
                    BrowserActionsHelper.passAction(null);
                } else {
                    BrowserActionsHelper.failAction(rootCauseException);
                }
            } catch (Exception rootCauseException) {
                BrowserActionsHelper.failAction(rootCauseException);
            }
        } else {
            ReportManager.logDiscrete("Window is already closed and driver object is null.");
            BrowserActionsHelper.passAction(null);
        }
        return this;
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions maximizeWindow() {
        Dimension initialWindowSize;
        Dimension currentWindowSize;
        var targetWidth = 1920;
        var targetHeight = 1080;

        initialWindowSize = DriverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

        String targetBrowserName = SHAFT.Properties.web.targetBrowserName();
        String targetOperatingSystem = SHAFT.Properties.platform.targetPlatform();
        String executionAddress = SHAFT.Properties.platform.executionAddress();

        // try selenium WebDriver maximize
        currentWindowSize = BrowserActionsHelper.attemptMaximizeUsingSeleniumWebDriver(DriverFactoryHelper.getDriver(), executionAddress, targetBrowserName,
                targetOperatingSystem);
        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            // attempt resize using toolkit
            currentWindowSize = BrowserActionsHelper.attemptMaximizeUsingToolkitAndJavascript(DriverFactoryHelper.getDriver(), targetWidth, targetHeight);

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // happens with headless firefox browsers // remote // linux and windows
                // also happens with chrome/windows

                // attempt resize using WebDriver manage window
                currentWindowSize = BrowserActionsHelper.attemptMaximizeUsingSeleniumWebDriverManageWindow(DriverFactoryHelper.getDriver(), targetWidth, targetHeight);
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // attempt setting window to fullscreen
                fullScreenWindow();

                currentWindowSize = DriverFactoryHelper.getDriver().manage().window().getSize();
                ReportManager.logDiscrete("Window size after fullScreenWindow: " + currentWindowSize.toString());
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                ReportManager.logDiscrete("skipping window maximization due to unknown error, marking step as passed.");
            }
        }
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "New screen size is now: " + currentWindowSize);
        return this;
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions setWindowSize(int width, int height) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;

        initialWindowSize = DriverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

        DriverFactoryHelper.getDriver().manage().window().setPosition(new Point(0, 0));
        DriverFactoryHelper.getDriver().manage().window().setSize(new Dimension(width, height));
        // apparently we need to add +1 here to ensure that the new window size matches
        // the expected window size

        currentWindowSize = DriverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Window size after SWD: " + currentWindowSize.toString());

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ((JavascriptExecutor) DriverFactoryHelper.getDriver()).executeScript(JavaScriptHelper.WINDOW_FOCUS.getValue());
            ((JavascriptExecutor) DriverFactoryHelper.getDriver()).executeScript(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            ((JavascriptExecutor) DriverFactoryHelper.getDriver()).executeScript(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(width)).replace("$HEIGHT", String.valueOf(height)));

            currentWindowSize = DriverFactoryHelper.getDriver().manage().window().getSize();
            ReportManager.logDiscrete("Window size after JavascriptExecutor: " + currentWindowSize.toString());
        }

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ReportManager.logDiscrete("skipping window resizing due to unknown error, marking step as passed.");
        }

        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "New screen size is now: " + currentWindowSize);
        return this;
    }

    public LocalStorage getLocalStorage() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")) {
            return ((WebStorage) DriverFactoryHelper.getDriver()).getLocalStorage();
        } else {
            return (LocalStorage) ((JavascriptExecutor) DriverFactoryHelper.getDriver()).executeScript("return window.localStorage;");
        }
    }

    public SessionStorage getSessionStorage() {
        if (SHAFT.Properties.platform.executionAddress().equals("local")) {
            return ((WebStorage) DriverFactoryHelper.getDriver()).getSessionStorage();
        } else {
            return (SessionStorage) ((JavascriptExecutor) DriverFactoryHelper.getDriver()).executeScript("return window.sessionStorage;");
        }
    }

    public BrowserActions mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        return intercept(requestPredicate, mockedResponse);
    }

    public BrowserActions intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        ReportManager.logDiscrete("Attempting to configure network interceptor for \"" + requestPredicate + "\", will provide mocked response \"" + mockedResponse + "\"");
        try {
            NetworkInterceptor networkInterceptor = new NetworkInterceptor(
                    DriverFactoryHelper.getDriver(),
                    Route.matching(requestPredicate)
                            .to(() -> req -> mockedResponse));
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Successfully configured network interceptor.");
        } catch (Exception rootCauseException) {
            BrowserActionsHelper.failAction(rootCauseException);
        }
        return this;
    }

    /**
     * Resize the window to fill the current screen
     *
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions fullScreenWindow() {
        Dimension initialWindowSize = DriverFactoryHelper.getDriver().manage().window().getSize();
        ReportManager.logDiscrete("Initial Windows Size: " + initialWindowSize.width + "x" + initialWindowSize.height);

        if (!SHAFT.Properties.platform.executionAddress().equalsIgnoreCase("local")
                && SHAFT.Properties.web.headlessExecution()) {
            maximizeWindow();
        } else {
            DriverFactoryHelper.getDriver().manage().window().fullscreen();
        }

        ReportManager.logDiscrete("Current Windows Size after fullScreen: " + DriverFactoryHelper.getDriver().manage().window().getSize().width + "x" + DriverFactoryHelper.getDriver().manage().window().getSize().height);
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), DriverFactoryHelper.getDriver().getPageSource());
        return this;
    }


    /**
     * Switches focus to another Tab
     *
     * @param targetUrl The name of the URL you want to navigate to
     * @return a self-reference to be used to chain actions
     */
    @Deprecated
    public BrowserActions switchToNewTab(String targetUrl) {
        return navigateToURL(targetUrl, WindowType.TAB);
    }

    @Deprecated
    public BrowserActions switchToNewWindow(String targetUrl) {
        return navigateToURL(targetUrl, WindowType.WINDOW);
    }


    /**
     * Switches focus to another window
     *
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public BrowserActions switchToWindow(String nameOrHandle) {
        if (DriverFactoryHelper.getDriver().getWindowHandles().contains(nameOrHandle)) {
            DriverFactoryHelper.getDriver().switchTo().window(nameOrHandle);
            BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), nameOrHandle);
        } else {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), nameOrHandle);
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
    public BrowserActions addCookie(String key, String value) {
        DriverFactoryHelper.getDriver().manage().addCookie(new Cookie(key, value));
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Add Cookie", "Key: " + key + " | Value: " + value);
        return this;
    }

    /**
     * Gets a cookie with a given name.
     *
     * @param cookieName The cookie's name.
     * @return the cookie.
     */
    public Cookie getCookie(String cookieName) {
        Cookie cookie = DriverFactoryHelper.getDriver().manage().getCookieNamed(cookieName);
        if (cookie == null) {
            BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), "Get Cookie: " + cookieName);
        }
        return cookie;
    }

    /**
     * Gets all cookies for the current browsing context.
     *
     * @return A Set of cookies for the current browsing context.
     */
    public Set<Cookie> getAllCookies() {
        Set<Cookie> cookies = DriverFactoryHelper.getDriver().manage().getCookies();
        BrowserActionsHelper.passAction("");
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
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Get Cookie Domain with name: " + cookieName, cookieDomain);
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
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Get Cookie Value with name: " + cookieName, cookieValue);
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
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Get Cookie Path with name: " + cookieName, cookiePath);
        return cookiePath;
    }

    /**
     * Deletes the cookie data matching with the provided cookie name for the current browsing context.
     *
     * @param cookieName The name of the cookie to delete.
     * @return a self-reference to be used to chain actions.
     */
    @SuppressWarnings("UnusedReturnValue")
    public BrowserActions deleteCookie(String cookieName) {
        DriverFactoryHelper.getDriver().manage().deleteCookieNamed(cookieName);
        BrowserActionsHelper.passAction(DriverFactoryHelper.getDriver(), "Delete Cookie", cookieName);
        return this;
    }

    /**
     * Deletes all the cookies of the current browsing context.
     *
     * @return a self-reference to be used to chain actions.
     */
    @SuppressWarnings("UnusedReturnValue")
    public BrowserActions deleteAllCookies() {
        DriverFactoryHelper.getDriver().manage().deleteAllCookies();
        BrowserActionsHelper.passAction("");
        return this;
    }

    /**
     * Use this action to return a full page screenshot. This is a synonym to {@link  BrowserActions#captureScreenshot(Screenshots type)} if you pass `Screenshots.FULL`
     *
     * @return a self-reference for chainable actions
     */
    @SuppressWarnings("UnusedReturnValue")
    public BrowserActions captureScreenshot() {
        return this.captureScreenshot(Screenshots.FULL);
    }

    /**
     * Use this action to return a page screenshot. If you want to capture a screenshot then use this method instead {@see FluentBrowserActions#captureSnapshot()}
     *
     * @param type can either be `Screenshots.FULL`, or `Screenshots.VIEWPORT`
     * @return a self-reference for chainable actions
     */
    public BrowserActions captureScreenshot(Screenshots type) {
        var logText = "Capture " + type.getValue().toLowerCase() + " screenshot";
        switch (type) {
            case FULL ->
                    ReportManagerHelper.log(logText, Collections.singletonList(ScreenshotManager.prepareImageForReport(ScreenshotManager.takeFullPageScreenshot(DriverFactoryHelper.getDriver()), "captureScreenshot")));
            case VIEWPORT ->
                    ReportManagerHelper.log(logText, Collections.singletonList(ScreenshotManager.prepareImageForReport(ScreenshotManager.takeViewportScreenshot(DriverFactoryHelper.getDriver()), "captureScreenshot")));
            case ELEMENT ->
                    BrowserActionsHelper.failAction(DriverFactoryHelper.getDriver(), "Were you trying to use driver.element().captureScreenshot() instead?");
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
    public BrowserActions captureSnapshot() {
        var logMessage = "";
        var pageSnapshot = BrowserActionsHelper.capturePageSnapshot(DriverFactoryHelper.getDriver());
        if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
            logMessage = "Capture page snapshot";
        } else if (pageSnapshot.startsWith("<html")) {
            logMessage = "Capture page HTML";
        }
        ReportManagerHelper.log(logMessage, List.of(Arrays.asList(logMessage, ScreenshotManager.generateAttachmentFileName("captureSnapshot"), new ByteArrayInputStream(pageSnapshot.getBytes()))));
        return this;
    }

    public void generateLightHouseReport() {
        new LightHouseGenerateReport(DriverFactoryHelper.getDriver()).generateLightHouseReport();
    }
}
