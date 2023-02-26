package io.github.shafthq.shaft.gui.browser;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.TouchActions;
import io.github.shafthq.shaft.driver.helpers.DriverFactoryHelper;
import io.github.shafthq.shaft.driver.helpers.WizardHelpers;
import io.github.shafthq.shaft.enums.Screenshots;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import io.github.shafthq.shaft.gui.image.ScreenshotManager;
import io.github.shafthq.shaft.tools.io.helpers.ReportManagerHelper;
import io.github.shafthq.shaft.validations.helpers.WebDriverBrowserValidationsBuilder;
import org.openqa.selenium.Cookie;

import java.io.ByteArrayInputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

import static io.github.shafthq.shaft.gui.browser.BrowserActionsHelpers.failAction;

public class FluentBrowserActions {

    public FluentBrowserActions() {
    }

    public TouchActions performTouchAction() {
        return new TouchActions();
    }

    public AlertActions performAlertAction() {
        return new AlertActions();
    }

    public FluentElementActions performElementAction() {
        return new FluentElementActions();
    }

    public TouchActions touch() {
        return new TouchActions();
    }

    public AlertActions alert() {
        return new AlertActions();
    }

    public FluentElementActions element() {
        return new FluentElementActions();
    }

    public FluentBrowserActions and() {
        return this;
    }

    public WebDriverBrowserValidationsBuilder assertThat() {
        return new WizardHelpers.WebDriverAssertions(DriverFactoryHelper.getDriver()).browser();
    }

    public WebDriverBrowserValidationsBuilder verifyThat() {
        return new WizardHelpers.WebDriverVerifications(DriverFactoryHelper.getDriver()).browser();
    }

    /**
     * Attempts to capture a page snapshot archive in the format of a .mht file
     * Works only for Chromium based driver instances
     * For other driver types attempts to attach the current page source (for web)
     * or accessibility tree (for mobile)
     *
     */
    public FluentBrowserActions capturePageSnapshot() {
        BrowserActions.capturePageSnapshot(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Gets the current page URL and returns it as a string
     *
     * @return the URL that's currently open in the current page
     */
    public String getCurrentURL() {
        return BrowserActions.getCurrentURL(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Gets the current window title and returns it as a string
     *
     * @return the title of the current window
     */
    public String getCurrentWindowTitle() {
        return BrowserActions.getCurrentWindowTitle(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Gets the current page source and returns it as a string
     *
     * @return the source of the current page
     */
    public String getPageSource() {
        return BrowserActions.getPageSource(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Gets the current window handle and returns it as a string
     *
     * @return the window handle for the current window
     */
    public String getWindowHandle() {
        return BrowserActions.getWindowHandle(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Gets the current window position and returns it as a string
     *
     * @return the position of the current window
     */
    public String getWindowPosition() {
        return BrowserActions.getWindowPosition(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Gets the current window size and returns it as a string
     *
     * @return the size of the current window
     */
    public String getWindowSize() {
        return BrowserActions.getWindowSize(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Navigates to targetUrl in case the current URL is different, else refreshes
     * the current page
     *
     * @param targetUrl a string that represents the URL that you wish to navigate
     *                  to
     */
    public FluentBrowserActions navigateToURL(String targetUrl) {
        BrowserActions.navigateToURL(DriverFactoryHelper.getDriver().get(), targetUrl);
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
    public FluentBrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        BrowserActions.navigateToURL(DriverFactoryHelper.getDriver().get(), targetUrl, targetUrlAfterRedirection);
        return this;
    }

    public FluentBrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password, String targetUrlAfterAuthentication) {
        BrowserActions.navigateToURLWithBasicAuthentication(DriverFactoryHelper.getDriver().get(), targetUrl, username, password, targetUrlAfterAuthentication);
        return this;
    }

    /**
     * Navigates one step back from the browsers history
     */
    public FluentBrowserActions navigateBack() {
        BrowserActions.navigateBack(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Navigates one step forward from the browsers history
     */
    public FluentBrowserActions navigateForward() {
        BrowserActions.navigateForward(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Attempts to refresh the current page
     */
    public FluentBrowserActions refreshCurrentPage() {
        BrowserActions.refreshCurrentPage(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Closes the current browser window
     */
    public FluentBrowserActions closeCurrentWindow() {
        BrowserActions.closeCurrentWindow(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Maximizes current window size based on screen size minus 5%
     */
    public FluentBrowserActions maximizeWindow() {
        BrowserActions.maximizeWindow(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Resizes the current window size based on the provided width and height
     *
     * @param width  the desired new width of the target window
     * @param height the desired new height of the target window
     */
    public FluentBrowserActions setWindowSize(int width, int height) {
        BrowserActions.setWindowSize(DriverFactoryHelper.getDriver().get(), width, height);
        return this;
    }

    /**
     * Resize the window to fill the current screen
     */
    public FluentBrowserActions fullScreenWindow() {
        BrowserActions.fullScreenWindow(DriverFactoryHelper.getDriver().get());
        return this;
    }


    /**
     * Switches focus to another Tab
     *
     * @param URL The name of the URL you want to navigate to
     */
    public FluentBrowserActions switchToNewTab(String URL) {
        BrowserActions.switchToNewTab(DriverFactoryHelper.getDriver().get(), URL);
        return this;
    }


    /**
     * Switches focus to another window
     *
     * @param nameOrHandle The name of the window or the handle as returned by
     *                     ElementActions.getWindowHandle(WebDriver driver)
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions switchToWindow(String nameOrHandle) {
        BrowserActions.switchToWindow(DriverFactoryHelper.getDriver().get(), nameOrHandle);
        return this;
    }

    /**
     * Adds a cookie to the current browsing context.
     *
     * @param name The cookie's name
     * @param value The cookie's name
     * @return a self-reference to be used to chain actions
     */
    public FluentBrowserActions addCookie(String name, String value) {
        BrowserActions.addCookie(DriverFactoryHelper.getDriver().get(), name, value);
        return this;
    }

    /**
     * Gets a cookie with a given name.
     *
     * @param cookieName The cookie's name.
     * @return the cookie.
     */
    public Cookie getCookie(String cookieName) {
        return BrowserActions.getCookie(DriverFactoryHelper.getDriver().get(), cookieName);
    }

    /**
     * Gets all cookies for the current browsing context.
     *
     * @return A Set of cookies for the current browsing context.
     */
    public Set getAllCookies() {
        return BrowserActions.getAllCookies(DriverFactoryHelper.getDriver().get());
    }

    /**
     * Gets the cookie domain.
     *
     * @param cookieName The cookie's name.
     * @return te cookie domain;
     */
    public String getCookieDomain(String cookieName) {
        return BrowserActions.getCookieDomain(DriverFactoryHelper.getDriver().get(), cookieName);
    }

    /**
     * Gets the cookie value.
     *
     * @param cookieName The cookie's name.
     * @return the cookie value;
     */
    public String getCookieValue(String cookieName) {
        return BrowserActions.getCookieValue(DriverFactoryHelper.getDriver().get(), cookieName);
    }

    /**
     * Gets the cookie path.
     *
     * @param cookieName The cookie's name.
     * @return the cookie path;
     */
    public String getCookiePath(String cookieName) {
        return BrowserActions.getCookiePath(DriverFactoryHelper.getDriver().get(), cookieName);
    }

    /**
     * Deletes the cookie data matching with the provided cookie name for the current browsing context.
     *
     * @param cookieName The name of the cookie to delete.
     * @return a self-reference to be used to chain actions.
     */
    public FluentBrowserActions deleteCookie(String cookieName) {
        BrowserActions.deleteCookie(DriverFactoryHelper.getDriver().get(), cookieName);
        return this;
    }

    /**
     * Deletes all the cookies of the current browsing context.
     *
     * @return a self-reference to be used to chain actions.
     */
    public FluentBrowserActions deleteAllCookies() {
        BrowserActions.deleteAllCookies(DriverFactoryHelper.getDriver().get());
        return this;
    }

    /**
     * Use this action to return a full page screenshot. This is a synonym to {@link  FluentBrowserActions#captureScreenshot(Screenshots type)} if you pass Screenshots.FULL
     *
     * @return a self-reference for chainable actions
     */
    public FluentBrowserActions captureScreenshot() {
        return this.captureScreenshot(Screenshots.FULL);
    }

    /**
     * Use this action to return a page screenshot. If you want to capture a screenshot then use this method instead {@see FluentBrowserActions#captureSnapshot()}
     *
     * @param type can either be Screenshots.FULL, or Screenshots.VIEWPORT
     * @return a self-reference for chainable actions
     */
    public FluentBrowserActions captureScreenshot(Screenshots type) {
        var logText = "Capture " + type.getValue().toLowerCase() + " screenshot";
        switch (type) {
            case FULL ->
                    ReportManagerHelper.log(logText, Collections.singletonList(ScreenshotManager.prepareImageforReport(ScreenshotManager.takeFullPageScreenshot(DriverFactoryHelper.getDriver().get()), "captureScreenshot")));
            case VIEWPORT ->
                    ReportManagerHelper.log(logText, Collections.singletonList(ScreenshotManager.prepareImageforReport(ScreenshotManager.takeViewportScreenshot(DriverFactoryHelper.getDriver().get()), "captureScreenshot")));
            case ELEMENT ->
                    failAction(DriverFactoryHelper.getDriver().get(), "Were you trying to use driver.element().captureScreenshot() instead?");
        }
        return this;
    }

    /**
     * Use this action to return a page snapshot. A page snapshot is a single .mht file that contains the full page DOM and any related assets
     * to help you view the page as a whole. If you want to capture a screenshot then use this method instead @see FluentBrowserActions#captureScreenshot()
     *
     * @return a self-reference for chainable actions
     */
    public FluentBrowserActions captureSnapshot() {
        var logMessage = "";
        var pageSnapshot = BrowserActionsHelpers.capturePageSnapshot(DriverFactoryHelper.getDriver().get(), true);
        if (pageSnapshot.startsWith("From: <Saved by Blink>")) {
            logMessage = "Capture page snapshot";
        } else if (pageSnapshot.startsWith("<html")) {
            logMessage = "Capture page HTML";
        }
        ReportManagerHelper.log(logMessage, Arrays.asList(Arrays.asList(logMessage, ScreenshotManager.generateAttachmentFileName("captureSnapshot"), new ByteArrayInputStream(pageSnapshot.getBytes()))));
        return this;
    }
}
