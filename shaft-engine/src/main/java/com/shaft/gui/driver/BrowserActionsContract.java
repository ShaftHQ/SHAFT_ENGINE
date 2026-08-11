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

    String getContext();

    BrowserActionsContract setContext(String context);

    List<String> getWindowHandles();

    List<String> getContextHandles();

    AccessibilityActions accessibility();
}
