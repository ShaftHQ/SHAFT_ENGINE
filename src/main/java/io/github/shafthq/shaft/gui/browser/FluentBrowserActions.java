package io.github.shafthq.shaft.gui.browser;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.gui.element.AlertActions;
import com.shaft.gui.element.TouchActions;
import io.github.shafthq.shaft.driver.DriverFactoryHelper;
import io.github.shafthq.shaft.gui.element.FluentElementActions;
import org.openqa.selenium.WebDriver;

public class FluentBrowserActions {

    public FluentBrowserActions(WebDriver driver) {
    }

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

    /**
     * Attempts to capture a page snapshot archive in the format of a .mht file
     * Works only for Chromium based driver instances
     * For other driver types attempts to attach the current page source (for web)
     * or accessibility tree (for mobile)
     *
     * @return
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
}
