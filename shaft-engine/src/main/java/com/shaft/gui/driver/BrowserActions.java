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
public interface BrowserActions {

    BrowserActions and();

    BrowserAssertions assertThat();

    BrowserAssertions verifyThat();

    BrowserActions capturePageSnapshot();

    String getCurrentURL();

    String getCurrentWindowTitle();

    String getPageSource();

    String getWindowHandle();

    String getWindowPosition();

    String getWindowSize();

    String getWindowHeight();

    String getWindowWidth();

    BrowserActions navigateToURL(String targetUrl);

    BrowserActions navigateToURL(String targetUrl, WindowType windowType);

    BrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection);

    BrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password,
                                                       String targetUrlAfterAuthentication);

    BrowserActions navigateBack();

    BrowserActions navigateForward();

    BrowserActions refreshCurrentPage();

    void closeCurrentWindow();

    BrowserActions maximizeWindow();

    BrowserActions setWindowSize(int width, int height);

    BrowserActions mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    NetworkInterceptionRequestBuilder interceptRequest();

    BrowserActions intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse);

    BrowserActions clearNetworkInterceptors();

    BrowserActions fullScreenWindow();

    BrowserActions switchToWindow(String nameOrHandle);

    BrowserActions addCookie(String key, String value);

    Cookie getCookie(String cookieName);

    Set<Cookie> getAllCookies();

    String getCookieDomain(String cookieName);

    String getCookieValue(String cookieName);

    String getCookiePath(String cookieName);

    BrowserActions deleteCookie(String cookieName);

    BrowserActions deleteAllCookies();

    BrowserActions captureScreenshot();

    BrowserActions captureScreenshot(Screenshots type);

    BrowserActions captureSnapshot();

    void generateLightHouseReport();

    BrowserActions waitForLazyLoading();

    String getContext();

    BrowserActions setContext(String context);

    List<String> getWindowHandles();

    List<String> getContextHandles();

    AccessibilityActions accessibility();
}
