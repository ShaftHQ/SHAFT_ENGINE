package com.shaft.gui.driver;

import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.NetworkInterceptionRequestBuilder;
import com.shaft.validation.accessibility.AccessibilityActions;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

/**
 * Public contract for browser-level SHAFT actions.
 */
public interface BrowserActionsContract {

    BrowserActionsContract and();

    BrowserAssertions assertThat();

    BrowserAssertions verifyThat();

    BrowserActionsContract capturePageSnapshot();

    String getCurrentURL();

    String getCurrentWindowTitle();

    String getPageSource();

    String getWindowHandle();

    String getWindowPosition();

    String getWindowSize();

    String getWindowHeight();

    String getWindowWidth();

    BrowserActionsContract navigateToURL(String targetUrl);

    BrowserActionsContract navigateToURL(String targetUrl, WindowType windowType);

    /**
     * Opens the target URL in a new browser tab and switches focus to it.
     *
     * @param targetUrl target URL to open
     * @return a self-reference to be used to chain actions
     */
    default BrowserActionsContract openNewTab(String targetUrl) {
        throw new UnsupportedOperationException("openNewTab is not supported by this browser actions implementation.");
    }

    /**
     * Opens the target URL in a new browser window and switches focus to it.
     *
     * @param targetUrl target URL to open
     * @return a self-reference to be used to chain actions
     */
    default BrowserActionsContract openNewWindow(String targetUrl) {
        throw new UnsupportedOperationException("openNewWindow is not supported by this browser actions implementation.");
    }

    BrowserActionsContract navigateToURL(String targetUrl, String targetUrlAfterRedirection);

    BrowserActionsContract navigateToURLWithBasicAuthentication(String targetUrl, String username, String password,
                                                       String targetUrlAfterAuthentication);

    BrowserActionsContract navigateBack();

    BrowserActionsContract navigateForward();

    BrowserActionsContract refreshCurrentPage();

    void closeCurrentWindow();

    BrowserActionsContract maximizeWindow();

    BrowserActionsContract setWindowSize(int width, int height);

    BrowserActionsContract mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    NetworkInterceptionRequestBuilder interceptRequest();

    BrowserActionsContract intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    BrowserActionsContract clearNetworkInterceptors();

    BrowserActionsContract startContractRecording(String contractFilePath, String... urlContains);

    BrowserActionsContract assertContract(String contractFilePath, String... urlContains);

    BrowserActionsContract verifyContract(String contractFilePath, String... urlContains);

    BrowserActionsContract replayContract(String contractFilePath);

    /**
     * Replays recorded HAR (HTTP Archive) responses through the browser network interceptor.
     *
     * @param harFilePath path to a HAR 1.2 JSON file
     * @return a self-reference to be used to chain actions
     */
    BrowserActionsContract routeFromHar(String harFilePath);

    BrowserActionsContract fullScreenWindow();

    BrowserActionsContract switchToWindow(String nameOrHandle);

    /**
     * Checks whether a browser alert, confirm, or prompt dialog is currently present.
     *
     * @return {@code true} when an alert is present; otherwise {@code false}
     */
    default boolean isAlertPresent() {
        throw new UnsupportedOperationException("isAlertPresent is not supported by this browser actions implementation.");
    }

    /**
     * Accepts the current browser alert, confirm, or prompt dialog.
     *
     * @return a self-reference to be used to chain actions
     */
    default BrowserActionsContract acceptAlert() {
        throw new UnsupportedOperationException("acceptAlert is not supported by this browser actions implementation.");
    }

    /**
     * Dismisses the current browser alert, confirm, or prompt dialog.
     *
     * @return a self-reference to be used to chain actions
     */
    default BrowserActionsContract dismissAlert() {
        throw new UnsupportedOperationException("dismissAlert is not supported by this browser actions implementation.");
    }

    /**
     * Gets the current browser alert, confirm, or prompt dialog text.
     *
     * @return the alert text
     */
    default String getAlertText() {
        throw new UnsupportedOperationException("getAlertText is not supported by this browser actions implementation.");
    }

    /**
     * Types text into the current browser prompt dialog.
     *
     * @param text text to type into the prompt
     * @return a self-reference to be used to chain actions
     */
    default BrowserActionsContract typeIntoPromptAlert(String text) {
        throw new UnsupportedOperationException("typeIntoPromptAlert is not supported by this browser actions implementation.");
    }

    BrowserActionsContract addCookie(String key, String value);

    Cookie getCookie(String cookieName);

    Set<Cookie> getAllCookies();

    String getCookieDomain(String cookieName);

    String getCookieValue(String cookieName);

    String getCookiePath(String cookieName);

    BrowserActionsContract deleteCookie(String cookieName);

    BrowserActionsContract deleteAllCookies();

    BrowserActionsContract captureScreenshot();

    BrowserActionsContract captureScreenshot(Screenshots type);

    BrowserActionsContract captureSnapshot();

    void generateLightHouseReport();

    BrowserActionsContract waitForLazyLoading();

    String getContext();

    BrowserActionsContract setContext(String context);

    List<String> getWindowHandles();

    List<String> getContextHandles();

    AccessibilityActions accessibility();
}
