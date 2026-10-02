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

    /**
     * Returns cohesive browser-network observation, mocking, replay, and emulation actions.
     * Existing implementations that do not declare a competing zero-argument {@code network}
     * method inherit this fail-closed default. As with every Java interface default-method
     * addition, a facade that also inherits an unrelated {@code network()} declaration with a
     * covariant-compatible return must override it to resolve default dispatch. An existing
     * declaration with an incompatible return type is source-incompatible and cannot be bridged
     * by an override; that facade must rename one API or stop combining the interfaces.
     *
     * @return network actions facade
     */
    default NetworkActionsContract network() {
        throw new UnsupportedOperationException("Network actions are not supported by this browser facade.");
    }

    /**
     * Returns concise alert, confirm, and prompt actions. The Java default-method collision
     * boundary documented for {@link #network()} also applies to this namespace method.
     */
    default DialogActionsContract dialog() {
        throw new UnsupportedOperationException("Dialog actions are not supported by this browser facade.");
    }

    /**
     * Returns native/web browsing-context actions. The Java default-method collision boundary
     * documented for {@link #network()} also applies to this namespace method.
     */
    default ContextActionsContract context() {
        throw new UnsupportedOperationException("Context actions are not supported by this browser facade.");
    }

    /**
     * Returns scoped browser storage actions.
     *
     * <p>This default preserves compatibility for implementations that do not provide storage actions. The
     * default-method collision boundary is the same as documented for {@link #network()}.</p>
     *
     * @return storage state and key/value actions
     * @throws UnsupportedOperationException when this facade has no storage implementation
     */
    default StorageActionsContract storage() {
        throw new UnsupportedOperationException("Storage actions are not supported by this browser facade.");
    }

    /**
     * Returns bounded browser console observations.
     *
     * <p>The default-method collision boundary is the same as documented for {@link #network()}.</p>
     *
     * @return console query and clear actions
     * @throws UnsupportedOperationException when this facade has no console implementation
     */
    default ConsoleActionsContract console() {
        throw new UnsupportedOperationException("Console actions are not supported by this browser facade.");
    }

    /**
     * Returns script evaluation actions. The Java default-method collision boundary documented for
     * {@link #network()} also applies.
     *
     * @return script actions facade
     */
    default ScriptActionsContract script() {
        throw new UnsupportedOperationException("Script actions are not supported by this browser facade.");
    }

    /**
     * Returns browser-context permission controls. The Java default-method collision boundary documented for
     * {@link #network()} also applies.
     *
     * @return permission actions facade
     */
    default PermissionActionsContract permissions() {
        throw new UnsupportedOperationException("Permission actions are not supported by this browser facade.");
    }

    /**
     * Returns session-scoped HTTP authentication actions. The Java default-method collision boundary documented for
     * {@link #network()} also applies.
     */
    default AuthenticationActionsContract authentication() {
        throw new UnsupportedOperationException("Authentication actions are not supported by this browser facade.");
    }

    /**
     * Returns managed browser-download actions. The Java default-method collision boundary documented for
     * {@link #network()} also applies.
     */
    default DownloadActionsContract downloads() {
        throw new UnsupportedOperationException("Download actions are not supported by this browser facade.");
    }

    /**
     * Returns categorized browser environment emulation actions. The Java default-method collision boundary documented
     * for {@link #network()} also applies.
     */
    default EmulationActionsContract emulation() {
        throw new UnsupportedOperationException("Emulation actions are not supported by this browser facade.");
    }

    /**
     * Returns this instance to chain the next browser action.
     */
    BrowserActionsContract and();

    /**
     * Starts a hard assertion on the browser; a failure stops the test.
     */
    BrowserAssertions assertThat();

    /**
     * Starts a soft verification on the browser; failures are reported at the end of the test.
     */
    BrowserAssertions verifyThat();

    /**
     * Attaches a snapshot of the current page to the report.
     */
    BrowserActionsContract capturePageSnapshot();

    /**
     * Returns the URL of the current page.
     */
    String getCurrentURL();

    /**
     * Returns the title of the current window.
     */
    String getCurrentWindowTitle();

    /**
     * Returns the source of the current page.
     */
    String getPageSource();

    /**
     * Returns the handle of the current window.
     */
    String getWindowHandle();

    /**
     * Returns the position of the current window.
     */
    String getWindowPosition();

    /**
     * Returns the size of the current window.
     */
    String getWindowSize();

    /**
     * Returns the height of the current window.
     */
    String getWindowHeight();

    /**
     * Returns the width of the current window.
     */
    String getWindowWidth();

    /**
     * Navigates to the target URL in the current window.
     */
    BrowserActionsContract navigateToURL(String targetUrl);

    /**
     * Opens the target URL in a new tab or window.
     */
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

    /**
     * Navigates to the target URL and waits for the redirect to the expected URL.
     */
    BrowserActionsContract navigateToURL(String targetUrl, String targetUrlAfterRedirection);

    /**
     * Navigates to a page protected by HTTP basic authentication and waits for the expected URL
     * after logging in.
     */
    BrowserActionsContract navigateToURLWithBasicAuthentication(String targetUrl, String username, String password,
                                                       String targetUrlAfterAuthentication);

    /**
     * Navigates back in the browser history.
     */
    BrowserActionsContract navigateBack();

    /**
     * Navigates forward in the browser history.
     */
    BrowserActionsContract navigateForward();

    /**
     * Reloads the current page.
     */
    BrowserActionsContract refreshCurrentPage();

    /**
     * Closes the current window.
     */
    void closeCurrentWindow();

    /**
     * Maximizes the current window.
     */
    BrowserActionsContract maximizeWindow();

    /**
     * Resizes the current window to the given width and height in pixels.
     */
    BrowserActionsContract setWindowSize(int width, int height);

    /**
     * Answers every request that matches the predicate with the mocked response.
     */
    BrowserActionsContract mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    /**
     * Starts building a network interception rule.
     */
    NetworkInterceptionRequestBuilder interceptRequest();

    /**
     * Intercepts every request that matches the predicate and replies with the given response.
     */
    BrowserActionsContract intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    /**
     * Removes every network mock and interception rule.
     */
    BrowserActionsContract clearNetworkInterceptors();

    /**
     * Starts recording API traffic whose URL contains any of the given fragments into a contract
     * file.
     */
    BrowserActionsContract startContractRecording(String contractFilePath, String... urlContains);

    /**
     * Asserts that recorded traffic still matches the contract file; a failure stops the test.
     */
    BrowserActionsContract assertContract(String contractFilePath, String... urlContains);

    /**
     * Verifies that recorded traffic still matches the contract file; failures are reported at the
     * end of the test.
     */
    BrowserActionsContract verifyContract(String contractFilePath, String... urlContains);

    /**
     * Replays the responses saved in the contract file instead of calling the real services.
     */
    BrowserActionsContract replayContract(String contractFilePath);

    /**
     * Replays recorded HAR (HTTP Archive) responses through the browser network interceptor.
     *
     * @param harFilePath path to a HAR 1.2 JSON file
     * @return a self-reference to be used to chain actions
     */
    BrowserActionsContract routeFromHar(String harFilePath);

    /**
     * Switches the current window to full screen.
     */
    BrowserActionsContract fullScreenWindow();

    /**
     * Switches to the window with the given name or handle.
     */
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

    /**
     * Adds a cookie with the given name and value to the current domain.
     */
    BrowserActionsContract addCookie(String key, String value);

    /**
     * Returns the cookie with the given name.
     */
    Cookie getCookie(String cookieName);

    /**
     * Returns every cookie of the current domain.
     */
    Set<Cookie> getAllCookies();

    /**
     * Returns the domain of the cookie with the given name.
     */
    String getCookieDomain(String cookieName);

    /**
     * Returns the value of the cookie with the given name.
     */
    String getCookieValue(String cookieName);

    /**
     * Returns the path of the cookie with the given name.
     */
    String getCookiePath(String cookieName);

    /**
     * Deletes the cookie with the given name.
     */
    BrowserActionsContract deleteCookie(String cookieName);

    /**
     * Deletes every cookie of the current domain.
     */
    BrowserActionsContract deleteAllCookies();

    /**
     * Attaches a screenshot of the current page to the report.
     */
    BrowserActionsContract captureScreenshot();

    /**
     * Attaches a screenshot of the given type to the report.
     */
    BrowserActionsContract captureScreenshot(Screenshots type);

    /**
     * Attaches a full page snapshot (MHTML) to the report.
     */
    BrowserActionsContract captureSnapshot();

    /**
     * Generates a Lighthouse performance report for the current page.
     */
    void generateLightHouseReport();

    /**
     * Waits until lazily loaded content on the current page has finished loading.
     */
    BrowserActionsContract waitForLazyLoading();

    /**
     * Explicitly sweeps the current page with bounded progressive scrolling (viewport-height
     * steps, up to {@code timeouts.lazyLoadingScrollSweepMaxSteps}), waiting for lazy-loading
     * readiness between steps, to force scroll-triggered content (infinite lists, IntersectionObserver
     * sections that only hydrate once visible) to fully load before full-page assertions or
     * screenshots. Use it right before such assertions on pages known to lazy-load on scroll --
     * it is never invoked automatically by any readiness wait, since sweeping the whole page is not
     * a safe default before an arbitrary action.
     *
     * <p><b>Mutates scroll position during execution</b> (restored to its original position when
     * the sweep finishes, including on early exit).
     *
     * @return a self-reference to be used to chain actions
     */
    default BrowserActionsContract scrollToLoadAll() {
        throw new UnsupportedOperationException("scrollToLoadAll is not supported by this browser actions implementation.");
    }

    /**
     * Returns the current mobile context, such as {@code NATIVE_APP} or a web view.
     */
    String getContext();

    /**
     * Switches to the given mobile context.
     */
    BrowserActionsContract setContext(String context);

    /**
     * Returns the handles of all open windows.
     */
    List<String> getWindowHandles();

    /**
     * Returns the names of all available mobile contexts.
     */
    List<String> getContextHandles();

    /**
     * Returns the accessibility actions for the current page.
     */
    AccessibilityActions accessibility();
}
