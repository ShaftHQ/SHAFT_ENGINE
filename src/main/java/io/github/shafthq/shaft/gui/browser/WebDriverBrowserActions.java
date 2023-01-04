package io.github.shafthq.shaft.gui.browser;

import com.google.common.net.InternetDomainName;
import com.shaft.driver.DriverFactory;
import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.gui.element.JavaScriptWaitManager;
import io.github.shafthq.shaft.gui.element.WebDriverElementActions;
import io.github.shafthq.shaft.gui.image.ScreenshotManager;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.github.shafthq.shaft.tools.support.JavaHelper;
import io.github.shafthq.shaft.tools.support.JavaScriptHelper;
import lombok.SneakyThrows;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.*;
import org.openqa.selenium.chromium.ChromiumDriver;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.remote.Augmenter;
import org.openqa.selenium.support.ui.ExpectedConditions;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;

import java.awt.*;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;

public class WebDriverBrowserActions {
    private static final Boolean HEADLESS_EXECUTION = Boolean.valueOf(System.getProperty("headlessExecution").trim());
    private static final int NAVIGATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("browserNavigationTimeout").trim());

    private static final ThreadLocal<WebDriver> lastUsedDriver = new ThreadLocal<>();

    public WebDriverBrowserActions(WebDriver driver) {
        lastUsedDriver.set(driver);
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the URL that's currently open in the current page
     */
    public static String getCurrentURL(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
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
    public static String getCurrentWindowTitle(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
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
    public static String getPageSource(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
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
     * Gets the current window handle and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the window handle for the current window
     */
    public static String getWindowHandle(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
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
    public static String getWindowPosition(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
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
    public static String getWindowSize(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
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
    public static void navigateToURL(WebDriver driver, String targetUrl) {
        navigateToURL(driver, targetUrl, targetUrl);
    }

    public static void navigateToURLWithBasicAuthentication(WebDriver driver, String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        String domainName = getDomainNameFromURL(targetUrl);
        String driverName = System.getProperty("targetBrowserName");
        if (driverName.equals("GoogleChrome") || driverName.equals("MicrosoftEdge")){
            if (System.getProperty("executionAddress").equals("local")) {
                Predicate<URI> uriPredicate = uri -> uri.getHost().contains(domainName);
                ((HasAuthentication) driver).register(uriPredicate, UsernameAndPassword.of(username, password));
            } else {
                try {
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
                } catch (org.openqa.selenium.remote.http.ConnectionFailedException | java.lang.IllegalArgumentException e){
                    //in case of remote connection but Unable to establish websocket connection
                    targetUrl = formatURL(username, password, targetUrl);
                }
            }
        } else{
            //in case of ie, firefox, safari, ...etc
            targetUrl = formatURL(username, password, targetUrl);
        }
        navigateToURL(driver, targetUrl, targetUrlAfterAuthentication);
    }

    @SneakyThrows
    private static String formatURL(String username, String password, String targetUrl){
        if (targetUrl.startsWith("https://")){
            return new URI("https://" + URLEncoder.encode(username, StandardCharsets.UTF_8)+":"+URLEncoder.encode(password, StandardCharsets.UTF_8)+ "@"+ targetUrl.substring("https://".length())).toString();
        }else{
            return new URI("http://" + URLEncoder.encode(username, StandardCharsets.UTF_8)+":"+URLEncoder.encode(password, StandardCharsets.UTF_8)+ "@"+ targetUrl.substring("http://".length())).toString();
        }
    }

    @SneakyThrows
    private static String getDomainNameFromURL(String url) {
        // https://www.baeldung.com/java-domain-name-from-url#using-the-internetdomainname-class-from-guava-library
        URI uri = new URI(url);
        String host = uri.getHost();
        InternetDomainName internetDomainName = InternetDomainName.from(host).topPrivateDomain();
        return internetDomainName.toString();
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
            ReportManagerHelper.log(rootCauseException);
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
                if ((WebDriverElementActions.getElementsCount(driver, By.tagName("html")) == 1)
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
                if (WebDriverElementActions.getElementsCount(driver, By.tagName("html")) == 1) {
                    confirmThatWebsiteIsNotDown(driver, modifiedTargetUrl);
                    passAction(driver, modifiedTargetUrl);
                }
            }
        } catch (Exception rootCauseException) {
            failAction(driver, modifiedTargetUrl, rootCauseException);
        }
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @param driver the current instance of Selenium WebDriver
     */
    public static void navigateBack(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
        String initialURL;
        var newURL = "";
        try {
            initialURL = driver.getCurrentUrl();
            driver.navigate().back();
            JavaScriptWaitManager.waitForLazyLoading(driver);
            (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER)))
                    .until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
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
    public static void navigateForward(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
        String initialURL;
        var newURL = "";
        try {
            initialURL = driver.getCurrentUrl();
            driver.navigate().forward();
            JavaScriptWaitManager.waitForLazyLoading(driver);
            (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER)))
                    .until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
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
    public static void refreshCurrentPage(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
        driver.navigate().refresh();
        passAction(driver, driver.getPageSource());
        // removed all exception handling as there was no comments on when and why this
        // exception happens
    }

    /**
     * Closes the current browser window
     *
     * @param driver the current instance of Selenium WebDriver
     */
    public static void closeCurrentWindow(WebDriver driver) {
        if (driver != null) {
            JavaScriptWaitManager.waitForLazyLoading(driver);
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
     * Resize the window to fill the current screen
     *
     * @param driver the current instance of Selenium WebDriver
     */
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

    private static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(null, actionName, testData);
    }

    private static void passAction(WebDriver driver, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, actionName, testData);
    }

    private static void passAction(WebDriver driver, String actionName, String testData) {
        reportActionResult(driver, actionName, testData, true);
    }

    private static void failAction(Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(null, actionName, "", rootCauseException);
    }

    private static void failAction(WebDriver driver, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, rootCauseException);
    }

    private static void failAction(WebDriver driver, String actionName, String testData,
                                   Exception... rootCauseException) {
        String message = reportActionResult(driver, actionName, testData, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static String reportActionResult(WebDriver driver, String actionName, String testData,
                                             Boolean passFailStatus) {
        actionName = JavaHelper.convertToSentenceCase(actionName);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Browser Action: " + actionName;
        } else {
            message = "Browser Action: " + actionName + " failed";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && !testData.isEmpty()) {
            if (testData.length() >= 500 || testData.contains("</iframe>") || testData.contains("</html>")) {
                List<Object> actualValueAttachment = Arrays.asList("Browser Action Test Data - " + actionName,
                        "Actual Value", testData);
                attachments.add(actualValueAttachment);
            } else {
                message = message + " \"" + testData.trim() + "\"";
            }
        }
        message = message + ".";

        message = message.replace("Browser Action: ", "");
        if (driver != null) {
            attachments.add(ScreenshotManager.captureScreenShot(driver, actionName, passFailStatus));
            ReportManagerHelper.log(message, attachments);
        } else if (!attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    private static void confirmThatWebsiteIsNotDown(WebDriver driver, String targetUrl) {
        List<String> navigationErrorMessages = Arrays.asList("This site can’t be reached", "Unable to connect",
                "Safari Can’t Connect to the Server", "This page can't be displayed", "Invalid URL",
                "<head></head><body></body>");
        // TODO: get page loop outside the foreach loop
        navigationErrorMessages.forEach(errorMessage -> {
            if (driver.getPageSource().contains(errorMessage)) {
                failAction(driver, "Error message: \"" + errorMessage + "\", Target URL: \"" + targetUrl + "\"");
            }
        });
    }

    private static void navigateToNewURL(WebDriver driver, String initialURL, String targetUrl, String targetUrlAfterRedirection) {
        try {
            driver.navigate().to(targetUrl);
        } catch (WebDriverException rootCauseException) {
            failAction(driver, targetUrl, rootCauseException);
        }

        if (Boolean.TRUE.equals(Boolean.valueOf(System.getProperty("forceCheckNavigationWasSuccessful")))) {
            checkNavigationWasSuccessful(driver, initialURL, targetUrl, targetUrlAfterRedirection);
        }
    }

    private static void checkNavigationWasSuccessful(WebDriver driver, String initialURL, String targetUrl, String targetUrlAfterRedirection) {
        if (!targetUrl.equals(targetUrlAfterRedirection)) {
            try {
                (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER)))
                        .until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
            } catch (TimeoutException rootCauseException) {
                failAction(driver, "Waited for " + NAVIGATION_TIMEOUT_INTEGER + " seconds to navigate away from \"" + initialURL + "\" but didn't.", rootCauseException);
            }
        } else {
            try {
                (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER)))
                        .until(ExpectedConditions.not(ExpectedConditions.urlToBe(initialURL)));
                var modifiedTargetUrlAfterRedirection = (targetUrlAfterRedirection.startsWith("./")) ? targetUrl : targetUrlAfterRedirection;
                (new WebDriverWait(driver, Duration.ofSeconds(NAVIGATION_TIMEOUT_INTEGER)))
                        .until(ExpectedConditions.urlContains(modifiedTargetUrlAfterRedirection));
            } catch (TimeoutException rootCauseException) {
                failAction(driver, "Waited for " + NAVIGATION_TIMEOUT_INTEGER + " seconds to navigate to \"" + targetUrlAfterRedirection + "\" but ended up with \"" + driver.getCurrentUrl() + "\".", rootCauseException);
            }
        }
    }

    private static Dimension attemptMaximizeUsingSeleniumWebDriver(WebDriver driver, String executionAddress,
                                                                   String targetBrowserName, String targetOperatingSystem) {
        if ((!"local".equals(executionAddress) && !"GoogleChrome".equals(targetBrowserName))
                || ("local".equals(executionAddress)
                && !("GoogleChrome".equals(targetBrowserName) && "Mac-64".equals(targetOperatingSystem)))) {
            try {
                driver.manage().window().maximize();
                Dimension currentWindowSize = driver.manage().window().getSize();
                ReportManager.logDiscrete(
                        "Window size after SWD Maximize: " + currentWindowSize.toString());
                return currentWindowSize;
            } catch (WebDriverException rootCauseException) {
                // org.openqa.selenium.WebDriverException: unknown error: failed to change
                // window state to maximized, current state is normal
                ReportManagerHelper.log(rootCauseException);
            }
        }
        return driver.manage().window().getSize();
    }

    private static Dimension attemptMaximizeUsingToolkitAndJavascript(WebDriver driver, int width, int height) {
        int targetWidth = width;
        int targetHeight = height;
        try {
            var toolkit = Toolkit.getDefaultToolkit();
            if (Boolean.FALSE.equals(HEADLESS_EXECUTION)) {
                targetWidth = (int) toolkit.getScreenSize().getWidth();
                targetHeight = (int) toolkit.getScreenSize().getHeight();
            }
            driver.manage().window().setPosition(new Point(0, 0));
            driver.manage().window().setSize(new Dimension(targetWidth, targetHeight));

            ReportManager.logDiscrete("Window size after Toolkit: " + driver.manage().window().getSize().toString());
            return driver.manage().window().getSize();
        } catch (HeadlessException e) {
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_FOCUS.getValue());
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            ((JavascriptExecutor) driver).executeScript(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(targetWidth)).replace("$HEIGHT", String.valueOf(targetHeight)));

            ReportManager.logDiscrete(
                    "Window size after JavascriptExecutor: " + driver.manage().window().getSize().toString());
            return driver.manage().window().getSize();
        }
    }

    private static Dimension attemptMaximizeUsingSeleniumWebDriverManageWindow(WebDriver driver, int width,
                                                                               int height) {
        driver.manage().window().setPosition(new Point(0, 0));
        driver.manage().window().setSize(new Dimension(width, height));

        ReportManager.logDiscrete(
                "Window size after WebDriver.Manage.Window: " + driver.manage().window().getSize().toString());
        return driver.manage().window().getSize();
    }

    public static void capturePageSnapshot(WebDriver driver) {
        JavaScriptWaitManager.waitForLazyLoading(driver);
        var serializedPageData = "";
        try {
            if (driver instanceof ChromiumDriver chromiumDriver) {
                // capture snapshot
                var result = chromiumDriver.executeCdpCommand("Page.captureSnapshot", new HashMap<>());
                serializedPageData = (String) ((Map<String, ?>) result).get("data");
            } else {
                // get page source
                serializedPageData = driver.getPageSource();
            }
            passAction(driver, serializedPageData);
        } catch (Exception rootCauseException) {
            failAction(driver, serializedPageData, rootCauseException);
        }
    }

    public WebDriverBrowserActions capturePageSnapshot() {
        capturePageSnapshot(lastUsedDriver.get());
        return this;
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @return the URL that's currently open in the current page
     */
    public String getCurrentURL() {
        return getCurrentURL(lastUsedDriver.get());
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @return the title of the current window
     */
    public String getCurrentWindowTitle() {
        return getCurrentWindowTitle(lastUsedDriver.get());
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @return the source of the current page
     */
    public String getPageSource() {
        return getPageSource(lastUsedDriver.get());
    }

    /**
     * Gets the current window handle and returns it as a string
     *
     * @return the window handle for the current window
     */
    public String getWindowHandle() {
        return getWindowHandle(lastUsedDriver.get());
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @return the position of the current window
     */
    public String getWindowPosition() {
        return getWindowPosition(lastUsedDriver.get());
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the size of the current window
     */
    public String getWindowSize() {
        return getWindowSize(lastUsedDriver.get());
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    public WebDriverBrowserActions navigateToURL(String targetUrl) {
        navigateToURL(lastUsedDriver.get(), targetUrl);
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
     */
    public WebDriverBrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        navigateToURL(lastUsedDriver.get(), targetUrl, targetUrlAfterRedirection);
        return this;
    }

    public WebDriverBrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        navigateToURLWithBasicAuthentication(lastUsedDriver.get(), targetUrl, username, password, targetUrlAfterAuthentication);
        return this;
    }

    /**
     * Navigates one step back from the browsers history
     */
    public WebDriverBrowserActions navigateBack() {
        navigateBack(lastUsedDriver.get());
        return this;
    }

    /**
     * Navigates one step forward from the browsers history
     */
    public WebDriverBrowserActions navigateForward() {
        navigateForward(lastUsedDriver.get());
        return this;
    }

    /**
     * Attempts to refresh the current page
     */
    public WebDriverBrowserActions refreshCurrentPage() {
        refreshCurrentPage(lastUsedDriver.get());
        return this;
    }

    /**
     * Closes the current browser window
     */
    public WebDriverBrowserActions closeCurrentWindow() {
        closeCurrentWindow(lastUsedDriver.get());
        return this;
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     */
    public WebDriverBrowserActions maximizeWindow() {
        maximizeWindow(lastUsedDriver.get());
        return this;
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    public WebDriverBrowserActions setWindowSize(int width, int height) {
        setWindowSize(lastUsedDriver.get(), width, height);
        return this;
    }

    /**
     * Resize the window to fill the current screen
     */
    public WebDriverBrowserActions fullScreenWindow() {
        fullScreenWindow(lastUsedDriver.get());
        return this;
    }

    /**
     * Switches focus to another Tab
     *
     * @param driver the current instance of Selenium WebDriver
     * @param URL    The name of the URL you want to navigate to
     */
    public static void switchToNewTab(WebDriver driver, String URL) {
    	try {
    		var handleBeforeNavigation = driver.getWindowHandle();
        	driver.switchTo().newWindow(WindowType.TAB).navigate().to(URL);
        	var handleAfterNavigation = driver.getWindowHandle();
        	if (!handleBeforeNavigation.equals(handleAfterNavigation)) {
                ReportManager.logDiscrete("Old Tab Handle: \""+handleBeforeNavigation+"\", New Tab handle : \"" + handleAfterNavigation+"\"");
        		 passAction(driver, URL);
        	}
        	else {
        		failAction(driver, URL);
        	}
        	}
        catch (Exception rootCauseException) {
                failAction(driver, URL, rootCauseException);
            }
    }
    
    /**
     * Switches focus to another Tap
     *
     * @param URL The name of the URL you want to navigate to
     */
    public WebDriverBrowserActions switchToNewTab(String URL) {
    	switchToNewTab(lastUsedDriver.get(),URL);
		return this;
    }
    
    /**
     * Switches focus to another window
     *
     * @param driver       the current instance of Selenium WebDriver
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     */
    public static void switchToWindow(WebDriver driver, String nameOrHandle) {
        if (driver.getWindowHandles().contains(nameOrHandle)) {
            driver.switchTo().window(nameOrHandle);
            passAction(driver, nameOrHandle);
        } else {
            failAction(driver, nameOrHandle);
        }
    }
    
    /**
     * Switches focus to another window
     *
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    public WebDriverBrowserActions switchToWindow(String nameOrHandle) {
        switchToWindow(lastUsedDriver.get(), nameOrHandle);
        return this;
    }
}
