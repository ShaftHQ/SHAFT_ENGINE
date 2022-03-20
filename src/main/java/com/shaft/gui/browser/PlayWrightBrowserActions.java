package com.shaft.gui.browser;

import com.microsoft.playwright.Page;
import com.microsoft.playwright.Page.NavigateOptions;
import com.microsoft.playwright.options.LoadState;
import com.microsoft.playwright.options.ViewportSize;
import com.shaft.driver.DriverFactoryHelper;
import com.shaft.gui.element.PlayWrightElementActions;
import com.shaft.gui.image.ScreenshotManager;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.ReportManagerHelper;
import com.shaft.tools.support.JavaScriptHelper;
import org.openqa.selenium.Dimension;
import org.testng.Assert;

import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class PlayWrightBrowserActions {
    private static final Boolean HEADLESS_EXECUTION = Boolean.valueOf(System.getProperty("headlessExecution").trim());
    private static final int NAVIGATION_TIMEOUT_INTEGER = Integer
            .parseInt(System.getProperty("browserNavigationTimeout").trim());

    private static Page lastUsedPage;

    protected PlayWrightBrowserActions(Page page) {
        lastUsedPage = page;
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @param page the current instance of PlayWright page
     * @return the URL that's currently open in the current page
     */
    public static String getCurrentURL(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        var currentURL = "";
        try {
            currentURL = page.url();
            passAction(page, currentURL);
        } catch (Exception rootCauseException) {
            failAction(page, currentURL, rootCauseException);
        }
        return currentURL;
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @param page the current instance of PlayWright page
     * @return the title of the current window
     */
    public static String getCurrentWindowTitle(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        var currentWindowTitle = "";
        try {
            currentWindowTitle = page.title();
            passAction(page, currentWindowTitle);
        } catch (Exception rootCauseException) {
            failAction(page, currentWindowTitle, rootCauseException);
        }
        return currentWindowTitle;
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @param page the current instance of PlayWright page
     * @return the source of the current page
     */
    public static String getPageSource(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        var pageSource = "";
        try {
            pageSource = page.content();
            passAction(page, pageSource);
        } catch (Exception rootCauseException) {
            failAction(page, pageSource, rootCauseException);
        }
        return pageSource;
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @param page the current instance of PlayWright page
     * @return the size of the current window
     */
    public static String getWindowSize(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        var windowSize = "";
        try {
            windowSize = String.valueOf(page.viewportSize());
            passAction(page, windowSize);
        } catch (Exception rootCauseException) {
            failAction(page, windowSize, rootCauseException);
        }
        return windowSize;
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param page      the current instance of PlayWright page
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    public static void navigateToURL(Page page, String targetUrl) {
        navigateToURL(page, targetUrl, targetUrl);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page. Waits for successfully navigating to the final url after
     * redirection.
     *
     * @param page                      the current instance of PlayWright page
     * @param targetUrl                 a string that represents the URL that you
     *                                  wish to navigate to
     * @param targetUrlAfterRedirection a string that represents a part of the url
     *                                  that should be present after redirection,
     *                                  this string is used to confirm successful
     *                                  navigation
     */
    public static void navigateToURL(Page page, String targetUrl, String targetUrlAfterRedirection) {
        if (targetUrl.equals(targetUrlAfterRedirection)) {
            ReportManager.logDiscrete(
                    "Target URL: \"" + targetUrl + "\"");
        } else {
            ReportManager.logDiscrete(
                    "Target URL: \"" + targetUrl + "\", and after redirection: \"" + targetUrlAfterRedirection + "\"");
        }

        try {
            page.waitForLoadState(LoadState.NETWORKIDLE);
            String initialSource = page.content();
            String initialURL = page.url();
            // remove trailing slash which may cause comparing the current and target urls
            // to fail
            if (initialURL.startsWith("/", initialURL.length() - 1)) {
                initialURL = initialURL.substring(0, initialURL.length() - 1);
            }
            ReportManager.logDiscrete("Initial URL: \"" + initialURL + "\"");
            if (!initialURL.equals(targetUrl)) {
                // navigate to new url
                navigateToNewURL(page, targetUrl, targetUrlAfterRedirection);
                page.waitForLoadState(LoadState.NETWORKIDLE);
                if ((PlayWrightElementActions.getElementsCount(page, "xpath=//html") == 1)
                        && (!page.content().equalsIgnoreCase(initialSource))) {
                    confirmThatWebsiteIsNotDown(page, targetUrl);
                    passAction(page, targetUrl);
                } else {
                    failAction(page, targetUrl);
                }
            } else {
                // already on the same page
                page.reload();
                page.waitForLoadState(LoadState.NETWORKIDLE);
                if (PlayWrightElementActions.getElementsCount(page, "xpath=//html") == 1) {
                    confirmThatWebsiteIsNotDown(page, targetUrl);
                    passAction(page, targetUrl);
                }
            }
        } catch (Exception rootCauseException) {
            failAction(page, targetUrl, rootCauseException);
        }
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @param page the current instance of PlayWright page
     */
    public static void navigateBack(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        String initialURL;
        var newURL = "";
        try {
            initialURL = page.url();
            page.goBack();
            page.waitForLoadState(LoadState.NETWORKIDLE);
            newURL = page.url();
            if (!newURL.equals(initialURL)) {
                passAction(page, newURL);
            } else {
                failAction(page, newURL);
            }
        } catch (Exception rootCauseException) {
            failAction(page, newURL, rootCauseException);
        }
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @param page the current instance of PlayWright page
     */
    public static void navigateForward(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        String initialURL;
        var newURL = "";
        try {
            initialURL = page.url();
            page.goForward();
            page.waitForLoadState(LoadState.NETWORKIDLE);
            newURL = page.url();
            if (!newURL.equals(initialURL)) {
                passAction(page, newURL);
            } else {
                failAction(page, newURL);
            }
        } catch (Exception rootCauseException) {
            failAction(page, newURL, rootCauseException);
        }
    }

    /**
     * Attempts to refresh the current page
     *
     * @param page the current instance of PlayWright page
     */
    public static void refreshCurrentPage(Page page) {
        page.waitForLoadState(LoadState.NETWORKIDLE);
        page.reload();
        passAction(page, page.content());
        // removed all exception handling as there was no comments on when and why this
        // exception happens
    }

    /**
     * Closes the current browser window
     *
     * @param page the current instance of PlayWright page
     */
    public static synchronized void closeCurrentWindow(Page page) {
        if (page != null) {
            page.waitForLoadState(LoadState.NETWORKIDLE);
            String lastPageSource = page.content();
            DriverFactoryHelper.closePlayWrightDriver();
            passAction(lastPageSource);
            PlayWrightElementActions.setLastUsedPage(null);
        } else {
            ReportManager.logDiscrete("Window is already closed and page object is null.");
            passAction(null);
        }
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     *
     * @param page the current instance of PlayWright page
     */
    public static void maximizeWindow(Page page) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;
        var targetWidth = 1920;
        var targetHeight = 1080;

        var viewportSize = page.viewportSize();
        initialWindowSize = new Dimension(viewportSize.width, viewportSize.height);

        ReportManager.logDiscrete("Initial window size: " + initialWindowSize);

        // attempt resize using toolkit
        currentWindowSize = attemptMaximizeUsingToolkitAndJavascript(page, targetWidth, targetHeight);

        if ((currentWindowSize.height != targetHeight)
                || (currentWindowSize.width != targetWidth)) {
            ReportManager.logDiscrete("skipping window maximization due to unknown error, marking step as passed.");
        }
        passAction(page, "New screen size is now: " + currentWindowSize);
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param page   the current instance of PlayWright page
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    public static void setWindowSize(Page page, int width, int height) {
        Dimension initialWindowSize;
        Dimension currentWindowSize;

        var viewportSize = page.viewportSize();
        initialWindowSize = new Dimension(viewportSize.width, viewportSize.height);
        ReportManager.logDiscrete("Initial window size: " + initialWindowSize);

        page.setViewportSize(width, height);
        // apparently we need to add +1 here to ensure that the new window size matches
        // the expected window size

        viewportSize = page.viewportSize();
        currentWindowSize = new Dimension(viewportSize.width, viewportSize.height);
        ReportManager.logDiscrete("Window size after SWD: " + currentWindowSize);

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            page.evaluate(JavaScriptHelper.WINDOW_FOCUS.getValue());
            page.evaluate(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            page.evaluate(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(width)).replace("$HEIGHT", String.valueOf(height)));

            viewportSize = page.viewportSize();
            currentWindowSize = new Dimension(viewportSize.width, viewportSize.height);
            ReportManager.logDiscrete("Window size after JavascriptExecutor: " + currentWindowSize);
        }

        if ((initialWindowSize.height == currentWindowSize.height)
                && (initialWindowSize.width == currentWindowSize.width)) {
            ReportManager.logDiscrete("skipping window resizing due to unknown error, marking step as passed.");
        }

        passAction(page, "New screen size is now: " + currentWindowSize);
    }

    private static void passAction(String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(null, actionName, testData);
    }

    private static void passAction(Page page, String testData) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        passAction(page, actionName, testData);
    }

    private static void passAction(Page page, String actionName, String testData) {
        reportActionResult(page, actionName, testData, true);
    }

    private static void failAction(Page page, String testData, Exception... rootCauseException) {
        String actionName = Thread.currentThread().getStackTrace()[2].getMethodName();
        failAction(page, actionName, testData, rootCauseException);
    }

    private static void failAction(Page page, String actionName, String testData,
                                   Exception... rootCauseException) {
        String message = reportActionResult(page, actionName, testData, false);
        if (rootCauseException != null && rootCauseException.length >= 1) {
            Assert.fail(message, rootCauseException[0]);
        } else {
            Assert.fail(message);
        }
    }

    private static String reportActionResult(Page page, String actionName, String testData,
                                             Boolean passFailStatus) {
//        RecordManager.startVideoRecording(page);
        actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);
        String message;
        if (Boolean.TRUE.equals(passFailStatus)) {
            message = "Browser Action \"" + actionName + "\" successfully performed.";
        } else {
            message = "Browser Action \"" + actionName + "\" failed.";
        }

        List<List<Object>> attachments = new ArrayList<>();
        if (testData != null && testData.length() >= 500) {
            List<Object> actualValueAttachment = Arrays.asList("Browser Action Test Data - " + actionName,
                    "Actual Value", testData);
            attachments.add(actualValueAttachment);
        } else if (testData != null && !testData.isEmpty()) {
            message = message + " With the following test data \"" + testData + "\".";
        }

        if (page != null) {
            attachments.add(ScreenshotManager.captureScreenShot(page, "", actionName, true));
            ReportManagerHelper.log(message, attachments);
        } else if (!attachments.equals(new ArrayList<>())) {
            ReportManagerHelper.log(message, attachments);
        } else {
            ReportManager.log(message);
        }
        return message;
    }

    private static void confirmThatWebsiteIsNotDown(Page page, String targetUrl) {
        List<String> navigationErrorMessages = Arrays.asList("This site can’t be reached", "Unable to connect",
                "Safari Can’t Connect to the Server", "This page can't be displayed", "Invalid URL",
                "<head></head><body></body>");
        navigationErrorMessages.forEach(errorMessage -> {
            if (page.content().contains(errorMessage)) {
                failAction(page, "Error message: \"" + errorMessage + "\", Target URL: \"" + targetUrl + "\"");
            }
        });
    }

    private static void navigateToNewURL(Page page, String targetUrl, String targetUrlAfterRedirection) {
        try {
            page.navigate(targetUrl, new NavigateOptions().setTimeout(NAVIGATION_TIMEOUT_INTEGER * 1000));
        } catch (Exception rootCauseException) {
            failAction(page, targetUrl, rootCauseException);
        }

        page.waitForLoadState(LoadState.NETWORKIDLE);
        var currentUrl = page.url();

        if ("/".equals(String.valueOf(currentUrl.charAt(currentUrl.length() - 1)))) {
            currentUrl = currentUrl.substring(0, currentUrl.length() - 1);
        }

        if ("/".equals(String.valueOf(targetUrlAfterRedirection.charAt(targetUrlAfterRedirection.length() - 1)))) {
            targetUrlAfterRedirection = targetUrlAfterRedirection.substring(0, targetUrlAfterRedirection.length() - 1);
        }

        if (!currentUrl.equals(targetUrlAfterRedirection)) {
            failAction(page, "Failed to navigate to \"" + targetUrlAfterRedirection + "\" and ended up on \"" + currentUrl + "\".");
        }
    }

    private static Dimension attemptMaximizeUsingToolkitAndJavascript(Page page, int width, int height) {
        int targetWidth = width;
        int targetHeight = height;
        ViewportSize viewportSize;
        try {
            var toolkit = Toolkit.getDefaultToolkit();
            if (Boolean.FALSE.equals(HEADLESS_EXECUTION)) {
                targetWidth = (int) toolkit.getScreenSize().getWidth();
                targetHeight = (int) toolkit.getScreenSize().getHeight();
            }
            page.setViewportSize(targetWidth, targetHeight);
            viewportSize = page.viewportSize();
            ReportManager.logDiscrete("Window size after Toolkit: " + viewportSize);
        } catch (HeadlessException e) {
            page.evaluate(JavaScriptHelper.WINDOW_FOCUS.getValue());
            page.evaluate(JavaScriptHelper.WINDOW_RESET_LOCATION.getValue());
            page.evaluate(JavaScriptHelper.WINDOW_RESIZE.getValue()
                    .replace("$WIDTH", String.valueOf(targetWidth)).replace("$HEIGHT", String.valueOf(targetHeight)));
            viewportSize = page.viewportSize();
            ReportManager.logDiscrete(
                    "Window size after JavascriptExecutor: " + viewportSize);
        }
        return new Dimension(viewportSize.width, viewportSize.height);
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @return the URL that's currently open in the current page
     */
    public String getCurrentURL() {
        return getCurrentURL(lastUsedPage);
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @return the title of the current window
     */
    public String getCurrentWindowTitle() {
        return getCurrentWindowTitle(lastUsedPage);
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @return the source of the current page
     */
    public String getPageSource() {
        return getPageSource(lastUsedPage);
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the size of the current window
     */
    public String getWindowSize() {
        return getWindowSize(lastUsedPage);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    public PlayWrightBrowserActions navigateToURL(String targetUrl) {
        navigateToURL(lastUsedPage, targetUrl);
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
    public PlayWrightBrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        navigateToURL(lastUsedPage, targetUrl, targetUrlAfterRedirection);
        return this;
    }

    /**
     * Navigates one step back from the browsers history
     */
    public PlayWrightBrowserActions navigateBack() {
        navigateBack(lastUsedPage);
        return this;
    }

    /**
     * Navigates one step forward from the browsers history
     */
    public PlayWrightBrowserActions navigateForward() {
        navigateForward(lastUsedPage);
        return this;
    }

    /**
     * Attempts to refresh the current page
     */
    public PlayWrightBrowserActions refreshCurrentPage() {
        refreshCurrentPage(lastUsedPage);
        return this;
    }

    /**
     * Closes the current browser window
     */
    public synchronized PlayWrightBrowserActions closeCurrentWindow() {
        closeCurrentWindow(lastUsedPage);
        return this;
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     */
    @SuppressWarnings("UnusedReturnValue")
    public PlayWrightBrowserActions maximizeWindow() {
        maximizeWindow(lastUsedPage);
        return this;
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    public PlayWrightBrowserActions setWindowSize(int width, int height) {
        setWindowSize(lastUsedPage, width, height);
        return this;
    }
}
