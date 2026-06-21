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

    BrowserActionsContract fullScreenWindow();

    BrowserActionsContract switchToWindow(String nameOrHandle);

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
