package com.shaft.gui.browser;

import com.microsoft.playwright.Page;
import org.openqa.selenium.WebDriver;

public class BrowserActions {

    public BrowserActions() {
        throw new IllegalStateException("Utility class");
    }

    public static WebDriverBrowserActions performBrowserAction(WebDriver driver) {
        return new WebDriverBrowserActions(driver);
    }

    public static PlayWrightBrowserActions performBrowserAction(Page page) {
        return new PlayWrightBrowserActions(page);
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @param driver the current instance of Selenium webdriver
     * @return the URL that's currently open in the current page
     */
    public static String getCurrentURL(WebDriver driver) {
        return WebDriverBrowserActions.getCurrentURL(driver);
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @param driver the current instance of Selenium webdriver
     * @return the title of the current window
     */
    public static String getCurrentWindowTitle(WebDriver driver) {
        return WebDriverBrowserActions.getCurrentWindowTitle(driver);
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @param driver the current instance of Selenium webdriver
     * @return the source of the current page
     */
    public static String getPageSource(WebDriver driver) {
        return WebDriverBrowserActions.getPageSource(driver);
    }

    /**
     * Gets the current window handle and returns it as a string
     *
     * @param driver the current instance of Selenium webdriver
     * @return the window handle for the current window
     */
    public static String getWindowHandle(WebDriver driver) {
        return WebDriverBrowserActions.getWindowHandle(driver);
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @param driver the current instance of Selenium webdriver
     * @return the position of the current window
     */
    public static String getWindowPosition(WebDriver driver) {
        return WebDriverBrowserActions.getWindowPosition(driver);
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @param driver the current instance of Selenium webdriver
     * @return the size of the current window
     */
    public static String getWindowSize(WebDriver driver) {
        return WebDriverBrowserActions.getWindowSize(driver);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param driver    the current instance of Selenium webdriver
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    public static void navigateToURL(WebDriver driver, String targetUrl) {
        WebDriverBrowserActions.navigateToURL(driver, targetUrl);
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page. Waits for successfully navigating to the final url after
     * redirection.
     *
     * @param driver                    the current instance of Selenium webdriver
     * @param targetUrl                 a string that represents the URL that you
     *                                  wish to navigate to
     * @param targetUrlAfterRedirection a string that represents a part of the url
     *                                  that should be present after redirection,
     *                                  this string is used to confirm successful
     *                                  navigation
     */
    public static void navigateToURL(WebDriver driver, String targetUrl, String targetUrlAfterRedirection) {
        WebDriverBrowserActions.navigateToURL(driver, targetUrl, targetUrlAfterRedirection);
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @param driver the current instance of Selenium webdriver
     */
    public static void navigateBack(WebDriver driver) {
        WebDriverBrowserActions.navigateBack(driver);
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @param driver the current instance of Selenium webdriver
     */
    public static void navigateForward(WebDriver driver) {
        WebDriverBrowserActions.navigateForward(driver);
    }

    /**
     * Attempts to refresh the current page
     *
     * @param driver the current instance of Selenium webdriver
     */
    public static void refreshCurrentPage(WebDriver driver) {
        WebDriverBrowserActions.refreshCurrentPage(driver);
    }

    /**
     * Closes the current browser window
     *
     * @param driver the current instance of Selenium webdriver
     */
    public static synchronized void closeCurrentWindow(WebDriver driver) {
        WebDriverBrowserActions.closeCurrentWindow(driver);
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     *
     * @param driver the current instance of Selenium webdriver
     */
    public static void maximizeWindow(WebDriver driver) {
        WebDriverBrowserActions.maximizeWindow(driver);
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param driver the current instance of Selenium webdriver
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    public static void setWindowSize(WebDriver driver, int width, int height) {
        WebDriverBrowserActions.setWindowSize(driver, width, height);
    }

    /**
     * Resizes the current window to become full screen
     *
     * @param driver the current instance of Selenium webdriver
     */
    public static void fullScreenWindow(WebDriver driver) {
        WebDriverBrowserActions.fullScreenWindow(driver);
    }

}
