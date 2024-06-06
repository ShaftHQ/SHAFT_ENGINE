package com.shaft.gui.browser.internal;

import com.google.common.net.InternetDomainName;
import com.shaft.db.DatabaseActions;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.image.ScreenshotManager;
import com.shaft.tools.internal.support.JavaHelper;
import com.shaft.tools.internal.support.JavaScriptHelper;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.FailureReporter;
import com.shaft.tools.io.internal.ReportManagerHelper;
import lombok.SneakyThrows;
import org.apache.logging.log4j.Level;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Point;
import org.openqa.selenium.*;
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
import java.util.List;
import java.util.*;

public class BrowserActionsHelper {
    public static final int NAVIGATION_TIMEOUT_INTEGER = SHAFT.Properties.timeouts.browserNavigationTimeout();
    private final boolean isSilent;
    private final Boolean HEADLESS_EXECUTION = SHAFT.Properties.web.headlessExecution();

    public BrowserActionsHelper(boolean isSilent) {
        this.isSilent = isSilent;
    }

    public void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(null, actionName, testData);
    }

    public void passAction(WebDriver driver, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(driver, actionName, testData);
    }

    public void passAction(WebDriver driver, String actionName, String testData) {
        reportActionResult(driver, actionName, testData, true);
    }

    public void failAction(Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(null, actionName, "", rootCauseException);
    }

    public void failAction(WebDriver driver, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(driver, actionName, testData, rootCauseException);
    }

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
            } else if (!attachments.equals(new ArrayList<>())) {
                ReportManagerHelper.log(message, attachments);
            } else {
                ReportManager.log(message);
            }
        }
        return message;
    }

    public void confirmThatWebsiteIsNotDown(WebDriver driver, String targetUrl) {
        if (SHAFT.Properties.flags.forceCheckNavigationWasSuccessful()) {
            List<String> navigationErrorMessages = Arrays.asList("This site can’t be reached", "Unable to connect",
                    "Safari Can’t Connect to the Server", "This page can't be displayed", "Invalid URL",
                    "<head></head><body></body>");
            //TODO: get page loop outside the foreach loop
            try {
                navigationErrorMessages.forEach(errorMessage -> {
                    var pageSource = driver.getPageSource();
                    if (pageSource != null && pageSource.contains(errorMessage)) {
                        failAction(driver, "Error message: \"" + errorMessage + "\", Target URL: \"" + targetUrl + "\"");
                    }
                });
            } catch (org.openqa.selenium.JavascriptException javascriptException) {
                // this happens in some cases with local execution on windows
            /*
            Caused by: org.openqa.selenium.JavascriptException: javascript error: Cannot read properties of null (reading 'outerHTML')
            (Session info: chrome=111.0.5563.111)
            Build info: version: '4.8.2', revision: '826dbfc730'
            System info: os.name: 'Windows 11', os.arch: 'amd64', os.version: '10.0', java.version: '17.0.3.1'
            Driver info: org.openqa.selenium.chrome.ChromeDriver
            Command: [3650f46d33000b7ed76f29f53d7810b6, getPageSource {}]
            */
                // try again
                JavaScriptWaitManager.waitForLazyLoading(driver);
                confirmThatWebsiteIsNotDown(driver, targetUrl);
            }
        }
    }

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
            new BrowsingContext(driver, driver.getWindowHandle()).navigate(internalURL, ReadinessState.COMPLETE);
        } catch (TimeoutException | java.lang.IllegalArgumentException |
                 org.openqa.selenium.bidi.BiDiException illegalArgumentException) {
            // Caused by: java.lang.IllegalArgumentException: WebDriver instance must support BiDi protocol
            // Caused by: org.openqa.selenium.bidi.BiDiException: Unable to create a BiDi connection
            // TimeoutException: Happens sometimes with some proxy environments
            driver.navigate().to(internalURL);
        } catch (WebDriverException rootCauseException) {
            failAction(driver, targetUrl, rootCauseException);
        }

        if (SHAFT.Properties.flags.forceCheckNavigationWasSuccessful() && !targetUrl.contains("\n")) {
            checkNavigationWasSuccessful(driver, initialURL, targetUrl, targetUrlAfterRedirection);
        }
    }

    public void checkNavigationWasSuccessful(WebDriver driver, String initialURL, String targetUrl, String targetUrlAfterRedirection) {
        if (!targetUrl.equals(targetUrlAfterRedirection)) {
            waitUntilUrlIsNot(driver, initialURL);
        } else {
            waitUntilUrlIsNot(driver, initialURL);
            var modifiedTargetUrlAfterRedirection = (targetUrlAfterRedirection.startsWith("./")) ? targetUrl : targetUrlAfterRedirection;
            waitUntilUrlContains(driver, modifiedTargetUrlAfterRedirection);
        }
    }

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

    public Dimension attemptMaximizeUsingSeleniumWebDriver(WebDriver driver, String executionAddress,
                                                           String targetBrowserName, String targetOperatingSystem) {
        if ((!"local".equals(executionAddress) && !"GoogleChrome".equals(targetBrowserName))
                || ("local".equals(executionAddress)
                && !("GoogleChrome".equals(targetBrowserName) && "Mac".equals(targetOperatingSystem)))) {
            try {
                driver.manage().window().maximize();
                Dimension currentWindowSize = driver.manage().window().getSize();
                ReportManager.logDiscrete(
                        "Window size after SWD Maximize: " + currentWindowSize.toString());
                return currentWindowSize;
            } catch (WebDriverException rootCauseException) {
                // org.openqa.selenium.WebDriverException: unknown error: failed to change
                // window state to maximized, current state is normal
                ReportManagerHelper.logDiscrete(rootCauseException);
            }
        }
        return driver.manage().window().getSize();
    }

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

    public Dimension attemptMaximizeUsingSeleniumWebDriverManageWindow(WebDriver driver, int width,
                                                                       int height) {
        driver.manage().window().setPosition(new Point(0, 0));
        driver.manage().window().setSize(new Dimension(width, height));

        ReportManager.logDiscrete(
                "Window size after WebDriver.Manage.Window: " + driver.manage().window().getSize().toString());
        return driver.manage().window().getSize();
    }

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
            return capturePageSnapshot(driver);
        } catch (Exception rootCauseException) {
            failAction(driver, serializedPageData, rootCauseException);
            return serializedPageData;
        }
    }


    @SneakyThrows
    public String formatUrlForBasicAuthentication(String username, String password, String targetUrl) {
        if (targetUrl.startsWith("https://")) {
            return new URI("https://" + URLEncoder.encode(username, StandardCharsets.UTF_8) + ":" + URLEncoder.encode(password, StandardCharsets.UTF_8) + "@" + targetUrl.substring("https://".length())).toString();
        } else {
            return new URI("http://" + URLEncoder.encode(username, StandardCharsets.UTF_8) + ":" + URLEncoder.encode(password, StandardCharsets.UTF_8) + "@" + targetUrl.substring("http://".length())).toString();
        }
    }

    @SneakyThrows
    public String getDomainNameFromUrl(String url) {
        // https://www.baeldung.com/java-domain-name-from-url#using-the-internetdomainname-class-from-guava-library
        URI uri = new URI(url);
        String host = uri.getHost();
        InternetDomainName internetDomainName = InternetDomainName.from(host).topPrivateDomain();
        return internetDomainName.toString();
    }
}
