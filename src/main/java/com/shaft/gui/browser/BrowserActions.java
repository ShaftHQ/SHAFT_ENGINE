package com.shaft.gui.browser;

import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.SikuliActions;
import com.shaft.gui.element.TouchActions;
import io.github.shafthq.shaft.gui.browser.FluentBrowserActions;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.WebDriver;
import org.sikuli.script.App;

import java.util.Set;

@SuppressWarnings("unused")
public class BrowserActions extends FluentBrowserActions {

    public BrowserActions() {
        super();
    }

    public BrowserActions(WebDriver driver) {
        super();
    }

    public static BrowserActions getInstance() {
        return new BrowserActions();
    }

    @Deprecated
    public static FluentBrowserActions performBrowserAction(WebDriver driver) {
        return FluentBrowserActions.getInstance();
    }

    @Deprecated
    public static FluentElementActions performElementAction(WebDriver driver) {
        return FluentElementActions.getInstance();
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
        return FluentBrowserActions.getInstance().getCurrentURL();
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the title of the current window
     */
    @Deprecated
    public static String getCurrentWindowTitle(WebDriver driver) {
        return FluentBrowserActions.getInstance().getCurrentWindowTitle();
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the source of the current page
     */
    @Deprecated
    public static String getPageSource(WebDriver driver) {
        return FluentBrowserActions.getInstance().getPageSource();
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
        FluentBrowserActions.getInstance().capturePageSnapshot();
    }


    /**
     * Gets the current window handle and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the window handle for the current window
     */
    @Deprecated
    public static String getWindowHandle(WebDriver driver) {
        return FluentBrowserActions.getInstance().getWindowHandle();
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the position of the current window
     */
    @Deprecated
    public static String getWindowPosition(WebDriver driver) {
        return FluentBrowserActions.getInstance().getWindowPosition();
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @param driver the current instance of Selenium WebDriver
     * @return the size of the current window
     */
    @Deprecated
    public static String getWindowSize(WebDriver driver) {
        return FluentBrowserActions.getInstance().getWindowSize();
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
        FluentBrowserActions.getInstance().navigateToURL(targetUrl);
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
        FluentBrowserActions.getInstance().navigateToURL(targetUrl, targetUrlAfterRedirection);
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
        FluentBrowserActions.getInstance().navigateToURLWithBasicAuthentication(targetUrl, username, password, targetUrlAfterAuthentication);
    }

    /**
     * Navigates one step back from the browsers history
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void navigateBack(WebDriver driver) {
        FluentBrowserActions.getInstance().navigateBack();
    }

    /**
     * Navigates one step forward from the browsers history
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void navigateForward(WebDriver driver) {
        FluentBrowserActions.getInstance().navigateForward();
    }

    /**
     * Attempts to refresh the current page
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void refreshCurrentPage(WebDriver driver) {
        FluentBrowserActions.getInstance().refreshCurrentPage();
    }

    /**
     * Closes the current browser window
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void closeCurrentWindow(WebDriver driver) {
        FluentBrowserActions.getInstance().closeCurrentWindow();
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void maximizeWindow(WebDriver driver) {
        FluentBrowserActions.getInstance().maximizeWindow();
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
        FluentBrowserActions.getInstance().setWindowSize(width, height);
    }

    /**
     * Resizes the current window to become full screen
     *
     * @param driver the current instance of Selenium WebDriver
     */
    @Deprecated
    public static void fullScreenWindow(WebDriver driver) {
        FluentBrowserActions.getInstance().fullScreenWindow();
    }

    /**
     * Switches focus to another Tab
     *
     * @param driver the current instance of Selenium WebDriver
     * @param url    The name of the URL you want to navigate to
     */
    @Deprecated
    public static void switchToNewTab(WebDriver driver, String url) {
        FluentBrowserActions.getInstance().switchToNewTab(url);
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
        FluentBrowserActions.getInstance().switchToWindow(nameOrHandle);
    }

    @Deprecated
    public static void addCookie(WebDriver driver, String key, String value) {
        FluentBrowserActions.getInstance().addCookie(key, value);
    }

    @Deprecated
    public static Cookie getCookie(WebDriver driver, String cookieName) {
        return FluentBrowserActions.getInstance().getCookie(cookieName);
    }

    @Deprecated
    public static Set<Cookie> getAllCookies(WebDriver driver) {
        return FluentBrowserActions.getInstance().getAllCookies();
    }

    @Deprecated
    public static String getCookieDomain(WebDriver driver, String cookieName) {
        return FluentBrowserActions.getInstance().getCookieDomain(cookieName);
    }

    @Deprecated
    public static String getCookieValue(WebDriver driver, String cookieName) {
        return FluentBrowserActions.getInstance().getCookieValue(cookieName);
    }

    @Deprecated
    public static String getCookiePath(WebDriver driver, String cookieName) {
        return FluentBrowserActions.getInstance().getCookiePath(cookieName);
    }

    @Deprecated
    public static void deleteCookie(WebDriver driver, String cookieName) {
        FluentBrowserActions.getInstance().deleteCookie(cookieName);
    }

    @Deprecated
    public static void deleteAllCookies(WebDriver driver) {
        FluentBrowserActions.getInstance().deleteAllCookies();
    }

}