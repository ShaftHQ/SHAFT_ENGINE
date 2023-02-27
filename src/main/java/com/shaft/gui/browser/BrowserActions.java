package com.shaft.gui.browser;

import com.shaft.driver.DriverFactory;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.gui.element.TouchActions;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.gui.browser.BrowserActionsHelpers;
import io.github.shafthq.shaft.gui.browser.FluentBrowserActions;
import io.github.shafthq.shaft.gui.browser.JavaScriptWaitManager;
import io.github.shafthq.shaft.gui.element.ElementActionsHelper;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.github.shafthq.shaft.tools.support.JavaScriptHelper;
import org.openqa.selenium.*;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;
import org.sikuli.script.App;

import java.net.URI;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

import static io.github.shafthq.shaft.gui.browser.BrowserActionsHelpers.*;

//TODO: Move body of implementation into the Fluent Actions class to fix internal "Deprecated member is still used" warnings
public class BrowserActions extends FluentBrowserActions {

    public BrowserActions() {
        new FluentBrowserActions();
    }

    public BrowserActions(WebDriver driver) {
        new FluentBrowserActions();
    }

    public static BrowserActions getInstance() {
        return new BrowserActions();
    }

    @Deprecated
    public static FluentBrowserActions performBrowserAction(WebDriver driver) {
        return new FluentBrowserActions();
    }

    @Deprecated
    public static FluentElementActions performElementAction(WebDriver driver) {
        return new FluentElementActions();
    }

    @Deprecated
    public static TouchActions performTouchAction(WebDriver driver) {
        return new TouchActions();
    }

    @Deprecated
    public static AlertActions performAlertAction(WebDriver driver) {
        return new AlertActions();
    }

    @Deprecated
    public static SikuliActions performSikuliAction() {
        return new SikuliActions();
    }

    @Deprecated
    public static SikuliActions performSikuliAction(App applicationWindow) {
        return new SikuliActions(applicationWindow);
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the URL that's currently open in the current page
     */
    @Deprecated
    public static String getCurrentURL(WebDriver driver) {
        var currentURL = "";
        try {
            currentURL = driver.getCurrentUrl();
            passAction(driver, currentURL);
        } catch (Exception rootCauseException) {
            failAction(driver, currentURL, rootCauseException);
        }
        return currentURL;
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the title of the current window
     */
    @Deprecated
    public static String getCurrentWindowTitle(WebDriver driver) {
        var currentWindowTitle = "";
        try {
            currentWindowTitle = driver.getTitle();
            passAction(driver, currentWindowTitle);
        } catch (Exception rootCauseException) {
            failAction(driver, currentWindowTitle, rootCauseException);
        }
        return currentWindowTitle;
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the source of the current page
     */
    @Deprecated
    public static String getPageSource(WebDriver driver) {
        var pageSource = "";
        try {
            pageSource = driver.getPageSource();
            passAction(driver, pageSource);
        } catch (Exception rootCauseException) {
            failAction(driver, pageSource, rootCauseException);
        }
        return pageSource;
    }

    /**
     * Attempts to capture a page snapshot archive in the format of a .mht file
     * Works only for Chromium based driver instances
     * For other driver types attempts to attach the current page source (for web)
     * or accessibility tree (for mobile)
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void capturePageSnapshot(WebDriver driver) {
        var serializedPageData = BrowserActionsHelpers.capturePageSnapshot(driver, true);
        passAction(driver, serializedPageData);
    }


    /**
     * Gets the current window handle and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the window handle for the current window
     */
    @Deprecated
    public static String getWindowHandle(WebDriver driver) {
        var windowHandle = "";
        try {
            windowHandle = driver.getWindowHandle();
            passAction(driver, windowHandle);
        } catch (Exception rootCauseException) {
            failAction(driver, windowHandle, rootCauseException);
        }
        return windowHandle;
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the position of the current window
     */
    @Deprecated
    public static String getWindowPosition(WebDriver driver) {
        var windowPosition = "";
        try {
            windowPosition = driver.manage().window().getPosition().toString();
            passAction(driver, windowPosition);
        } catch (Exception rootCauseException) {
            failAction(driver, windowPosition, rootCauseException);
        }
        return windowPosition;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the size of the current window
     */
    @Deprecated
    public static String getWindowSize(WebDriver driver) {
        var windowSize = "";
        try {
            windowSize = driver.manage().window().getSize().toString();
            passAction(driver, windowSize);
        } catch (Exception rootCauseException) {
            failAction(driver, windowSize, rootCauseException);
        }
        return windowSize;
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param driver    the current instance of Selenium WebDriver
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    @Deprecated
    public static void navigateToURL(WebDriver driver, String targetUrl) {
        navigateToURL(driver, targetUrl, targetUrl);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page. Waits for successfully navigating to the final url after
     * redirection.
     *
     * @param driver                    the current instance of Selenium WebDriver
     * @param targetUrl                 a string that represents the URL that you
     *                                  wish to navigate to
     * @param targetUrlAfterRedirection a string that represents a part of the url
     *                                  that should be present after redirection,
     *                                  this string is used to confirm successful
     *                                  navigation
     */
    @Deprecated
    public static void navigateToURL(WebDriver driver, String targetUrl, String targetUrlAfterRedirection) {
        String modifiedTargetUrl = targetUrl;
        var baseUrl = System.getProperty("baseURL").trim();

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
        try {
            ((JavascriptExecutor) driver).executeScript("return window.stop;");
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
            JavaScriptWaitManager.waitForLazyLoading(driver);
            String initialSource = driver.getPageSource();
            String initialURL = driver.getCurrentUrl();
            // remove trailing slash which may cause comparing the current and target urls
            // to fail
            if (initialURL.endsWith("/")) {
                initialURL = initialURL.substring(0, initialURL.length() - 1);
            }
            ReportManager.logDiscrete("Initial URL: \"" + initialURL + "\"");
            if (!initialURL.equals(modifiedTargetUrl)) {
                // navigate to new url
                navigateToNewURL(driver, initialURL, modifiedTargetUrl, targetUrlAfterRedirection);
                JavaScriptWaitManager.waitForLazyLoading(driver);
                if ((ElementActionsHelper.getElementsCount(driver, By.tagName("html")) == 1)
                        && (!driver.getPageSource().equalsIgnoreCase(initialSource))) {
                    confirmThatWebsiteIsNotDown(driver, modifiedTargetUrl);
                    passAction(driver, modifiedTargetUrl);
                } else {
                    failAction(driver, modifiedTargetUrl);
                }
            } else {
                // already on the same page
                driver.navigate().refresh();
                JavaScriptWaitManager.waitForLazyLoading(driver);
                if (ElementActionsHelper.getElementsCount(driver, By.tagName("html")) == 1) {
                    confirmThatWebsiteIsNotDown(driver, modifiedTargetUrl);
                    passAction(driver, modifiedTargetUrl);
                }
            }
        } catch (Exception rootCauseException) {
            failAction(driver, modifiedTargetUrl, rootCauseException);
        }
    }

    /**
     * Navigates to targetUrl and attempts to perform http authentication. Waits for successfully navigating to the final url after
     * authentication and redirection.
     *
     * @param driver                       the current instance of Selenium WebDriver
     * @param targetUrl                    a string that represents the URL that you
     *                                     wish to navigate to
     * @param username                     http credentials; username
     * @param password                     http credentials; password
     * @param targetUrlAfterAuthentication a string that represents a part of the url
     *                                     that should be present after redirection,
     *                                     this string is used to confirm successful
     *                                     authentication
     */
    @Deprecated
    public static void navigateToURLWithBasicAuthentication(WebDriver driver, String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        try {
            String domainName = getDomainNameFromURL(targetUrl);
            if (System.getProperty("executionAddress").equals("local")) {
                Predicate<URI> uriPredicate = uri -> uri.getHost().contains(domainName);
                ((HasAuthentication) driver).register(uriPredicate, UsernameAndPassword.of(username, password));
            } else {
                AtomicReference<DevTools> devToolsAtomicReference = new AtomicReference<>();
                driver = new Augmenter().addDriverAugmentation("chrome",
                        HasAuthentication.class,
                        (caps, exec) -> (whenThisMatches, useTheseCredentials) -> {
                            devToolsAtomicReference.get()
                                    .createSessionIfThereIsNotOne();
                            devToolsAtomicReference.get().getDomains()
                                    .network()
                                    .addAuthHandler(whenThisMatches,
                                            useTheseCredentials);
                        }).augment(driver);
                DevTools devTools = ((HasDevTools) driver).getDevTools();
                devTools.createSession();
                devToolsAtomicReference.set(devTools);
                ((HasAuthentication) driver).register(UsernameAndPassword.of(username, password));
            }
        } catch (Exception e) {
            ReportManagerHelper.logDiscrete(e);
            targetUrl = formatURLForBasicAuthentication(username, password, targetUrl);
        }
        navigateToURL(driver, targetUrl, targetUrlAfterAuthentication);
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void navigateBack(WebDriver driver) {
        String initialURL;
        var newURL = "";
        try {
            initialURL = driver.getCurrentUrl();
            driver.navigate().back();
            BrowserActionsHelpers.waitUntilURLIsNot(driver, initialURL);
            newURL = driver.getCurrentUrl();
            if (!newURL.equals(initialURL)) {
                passAction(driver, newURL);
            } else {
                failAction(driver, newURL);
            }
        } catch (Exception rootCauseException) {
            failAction(driver, newURL, rootCauseException);
        }
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void navigateForward(WebDriver driver) {
        String initialURL;
        var newURL = "";
        try {
            initialURL = driver.getCurrentUrl();
            driver.navigate().forward();
            JavaScriptWaitManager.waitForLazyLoading(driver);
            BrowserActionsHelpers.waitUntilURLIsNot(driver, initialURL);
            newURL = driver.getCurrentUrl();
            if (!newURL.equals(initialURL)) {
                passAction(driver, newURL);
            } else {
                failAction(driver, newURL);
            }
        } catch (Exception rootCauseException) {
            failAction(driver, newURL, rootCauseException);
        }
    }

    /**
     * Attempts to refresh the current page
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void refreshCurrentPage(WebDriver driver) {
        driver.navigate().refresh();
        passAction(driver, driver.getPageSource());
    }

    /**
     * Closes the current browser window
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void closeCurrentWindow(WebDriver driver) {
        if (driver != null) {
            try {
                // TODO: handle session timeout while attempting to close empty window
                String lastPageSource = driver.getPageSource();
                DriverFactory.closeAllDrivers();
                passAction(lastPageSource);
            } catch (WebDriverException rootCauseException) {
                if (rootCauseException.getMessage() != null
                        && (rootCauseException.getMessage().contains("was terminated due to TIMEOUT") || rootCauseException.getMessage().contains("Session ID is null"))) {
                    passAction(null);
                } else {
                    failAction(rootCauseException);
                }
            } catch (Exception rootCauseException) {
                failAction(rootCauseException);
            }
        } else {
            ReportManager.logDiscrete("Window is already closed and driver object is null.");
            passAction(null);
        }
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void maximizeWindow(WebDriver driver) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;
        var targetWidth = 1920;
        var targetHeight = 1080;

        initialWindowSize = driver.manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

        String targetBrowserName = System.getProperty("targetBrowserName").trim();
        String targetOperatingSystem = System.getProperty("targetOperatingSystem").trim();
        String executionAddress = System.getProperty("executionAddress").trim();

        // try selenium WebDriver maximize
        currentWindowSize = attemptMaximizeUsingSeleniumWebDriver(driver, executionAddress, targetBrowserName,
                targetOperatingSystem);
        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            // attempt resize using toolkit
            currentWindowSize = attemptMaximizeUsingToolkitAndJavascript(driver, targetWidth, targetHeight);

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // happens with headless firefox browsers // remote // linux and windows
                // also happens with chrome/windows

                // attempt resize using WebDriver manage window
                currentWindowSize = attemptMaximizeUsingSeleniumWebDriverManageWindow(driver, targetWidth, targetHeight);
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                // attempt setting window to fullscreen
                fullScreenWindow(driver);

                currentWindowSize = driver.manage().window().getSize();
                ReportManager.logDiscrete("Window size after fullScreenWindow: " + currentWindowSize.toString());
            }

            if ((currentWindowSize.height != targetHeight)
                    || (currentWindowSize.width != targetWidth)) {
                ReportManager.logDiscrete("skipping window maximization due to unknown error, marking step as passed.");
            }
        }
        passAction(driver, "New screen size is now: " + currentWindowSize);
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param driver the current instance of Selenium WebDriver
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    @Deprecated
    public static void setWindowSize(WebDriver driver, int width, int height) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;

        initialWindowSize = driver.manage().window().getSize();
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize.toString());

        driver.manage().window().setPosition(new Point(0, 0));
        driver.manage().window().setSize(new Dimension(width, height));
        // apparently we need to add +1 here to ensure that the new window size matches
        // the expected window size

        currentWindowSize = driver.manage().window().getSize();
        ReportManager.logDiscrete("Window size after SWD: " + currentWindowSize.toString());

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_FOCUS.getValue());
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(width)).replace("$HEIGHT", String.valueOf(height)));

            currentWindowSize = driver.manage().window().getSize();
            ReportManager.logDiscrete("Window size after JavascriptExecutor: " + currentWindowSize.toString());
        }

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ReportManager.logDiscrete("skipping window resizing due to unknown error, marking step as passed.");
        }

        passAction(driver, "New screen size is now: " + currentWindowSize);
    }

    /**
     * Resizes the current window to become full screen
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void fullScreenWindow(WebDriver driver) {
        Dimension initialWindowSize = driver.manage().window().getSize();
        ReportManager.logDiscrete("Initial Windows Size: " + initialWindowSize.width + "x" + initialWindowSize.height);

        if (!System.getProperty("executionAddress").trim().equalsIgnoreCase("local")
                && System.getProperty("headlessExecution").trim().equalsIgnoreCase("true")) {
            maximizeWindow(driver);
        } else {
            driver.manage().window().fullscreen();
        }

        ReportManager.logDiscrete("Current Windows Size after fullScreen: " + driver.manage().window().getSize().width + "x" + driver.manage().window().getSize().height);
        passAction(driver, driver.getPageSource());
    }

    /**
     * Switches focus to another Tab
     *
     * @param driver       the current instance of Selenium WebDriver
     * @param URL The name of the URL you want to navigate to
     */
    @Deprecated
    public static void switchToNewTab(WebDriver driver, String URL) {
        try {
            var handleBeforeNavigation = driver.getWindowHandle();
            driver.switchTo().newWindow(WindowType.TAB).navigate().to(URL);
            var handleAfterNavigation = driver.getWindowHandle();
            if (!handleBeforeNavigation.equals(handleAfterNavigation)) {
                ReportManager.logDiscrete("Old Tab Handle: \"" + handleBeforeNavigation + "\", New Tab handle : \"" + handleAfterNavigation + "\"");
                passAction(driver, URL);
            } else {
                failAction(driver, URL);
            }
        } catch (Exception rootCauseException) {
            failAction(driver, URL, rootCauseException);
        }
    }

    /**
     * Switches focus to another window
     *
     * @param driver       the current instance of Selenium WebDriver
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     */
    @Deprecated
    public static void switchToWindow(WebDriver driver, String nameOrHandle) {
        if (driver.getWindowHandles().contains(nameOrHandle)) {
            driver.switchTo().window(nameOrHandle);
            passAction(driver, nameOrHandle);
        } else {
            failAction(driver, nameOrHandle);
        }
    }

    @Deprecated
    public static void addCookie(WebDriver driver, String key, String value) {
        driver.manage().addCookie(new Cookie(key, value));
        passAction(driver, "Add Cookie", "Key: " + key + " | Value: " + value);
    }

    @Deprecated
    public static Cookie getCookie(WebDriver driver, String cookieName) {
        Cookie cookie = driver.manage().getCookieNamed(cookieName);
        if (cookie == null) {
            failAction(driver, "Get Cookie", cookieName);
        }
        return cookie;
    }

    @Deprecated
    public static Set<Cookie> getAllCookies(WebDriver driver) {
        Set<Cookie> cookies = driver.manage().getCookies();
        passAction("");
        return cookies;
    }

    @Deprecated
    public static String getCookieDomain(WebDriver driver, String cookieName) {
        String cookieDomain = getCookie(driver, cookieName).getDomain();
        passAction(driver, "Get Cookie Domain with name: " + cookieName, cookieDomain);
        return cookieDomain;
    }

    @Deprecated
    public static String getCookieValue(WebDriver driver, String cookieName) {
        String cookieValue = getCookie(driver, cookieName).getValue();
        passAction(driver, "Get Cookie Value with name: " + cookieName, cookieValue);
        return cookieValue;
    }

    @Deprecated
    public static String getCookiePath(WebDriver driver, String cookieName) {
        String cookiePath = getCookie(driver, cookieName).getPath();
        passAction(driver, "Get Cookie Path with name: " + cookieName, cookiePath);
        return cookiePath;
    }

    @Deprecated
    public static void deleteCookie(WebDriver driver, String cookieName) {
        driver.manage().deleteCookieNamed(cookieName);
        passAction(driver, "Delete Cookie", cookieName);
    }

    @Deprecated
    public static void deleteAllCookies(WebDriver driver) {
        driver.manage().deleteAllCookies();
        passAction("");
    }

}