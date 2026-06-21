package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.Cookie;
import com.microsoft.playwright.options.LoadState;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.NetworkInterceptionRequestBuilder;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.validation.PlaywrightBrowserValidationsBuilder;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.accessibility.AccessibilityActions;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.function.Predicate;

public class BrowserActions implements com.shaft.gui.driver.BrowserActions {
    private final PlaywrightSession session;

    public BrowserActions(PlaywrightSession session) {
        this.session = session;
    }

    @Override
    public BrowserActions and() {
        return this;
    }

    @Override
    public PlaywrightBrowserValidationsBuilder assertThat() {
        return new PlaywrightBrowserValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, session);
    }

    @Override
    public PlaywrightBrowserValidationsBuilder verifyThat() {
        return new PlaywrightBrowserValidationsBuilder(ValidationEnums.ValidationCategory.SOFT_ASSERT, session);
    }

    @Override
    public BrowserActions capturePageSnapshot() {
        ReportManagerHelper.attach("Playwright Page Snapshot", "page.html", page().content());
        ReportManager.log("Captured Playwright page snapshot.");
        return this;
    }

    @Override
    public String getCurrentURL() {
        return page().url();
    }

    @Override
    public String getCurrentWindowTitle() {
        return page().title();
    }

    @Override
    public String getPageSource() {
        return page().content();
    }

    @Override
    public String getWindowHandle() {
        return page().url();
    }

    @Override
    public String getWindowPosition() {
        return "0,0";
    }

    @Override
    public String getWindowSize() {
        return getWindowWidth() + "x" + getWindowHeight();
    }

    @Override
    public String getWindowHeight() {
        return String.valueOf(page().evaluate("() => window.innerHeight"));
    }

    @Override
    public String getWindowWidth() {
        return String.valueOf(page().evaluate("() => window.innerWidth"));
    }

    @Override
    public BrowserActions navigateToURL(String targetUrl) {
        page().navigate(targetUrl);
        ReportManager.log("Navigated Playwright page to URL: " + targetUrl);
        return this;
    }

    @Override
    public BrowserActions navigateToURL(String targetUrl, WindowType windowType) {
        if (windowType == WindowType.TAB || windowType == WindowType.WINDOW) {
            Page newPage = session.browserContext().newPage();
            session.setPage(newPage);
        }
        return navigateToURL(targetUrl);
    }

    @Override
    public BrowserActions navigateToURL(String targetUrl, String targetUrlAfterRedirection) {
        navigateToURL(targetUrl);
        if (targetUrlAfterRedirection != null && !targetUrlAfterRedirection.isBlank()) {
            page().waitForURL(targetUrlAfterRedirection);
        }
        return this;
    }

    @Override
    public BrowserActions navigateToURLWithBasicAuthentication(String targetUrl, String username, String password,
                                                              String targetUrlAfterAuthentication) {
        URI uri = URI.create(targetUrl);
        String authenticatedUrl = uri.getScheme() + "://" + username + ":" + password + "@" + uri.getAuthority()
                + uri.getRawPath() + (uri.getRawQuery() == null ? "" : "?" + uri.getRawQuery());
        return navigateToURL(authenticatedUrl, targetUrlAfterAuthentication);
    }

    @Override
    public BrowserActions navigateBack() {
        page().goBack();
        return this;
    }

    @Override
    public BrowserActions navigateForward() {
        page().goForward();
        return this;
    }

    @Override
    public BrowserActions refreshCurrentPage() {
        page().reload();
        return this;
    }

    @Override
    public void closeCurrentWindow() {
        page().close();
        List<Page> pages = session.browserContext().pages();
        if (!pages.isEmpty()) {
            session.setPage(pages.getFirst());
        }
    }

    @Override
    public BrowserActions maximizeWindow() {
        return setWindowSize(SHAFT.Properties.web.browserWindowWidth(), SHAFT.Properties.web.browserWindowHeight());
    }

    @Override
    public BrowserActions setWindowSize(int width, int height) {
        page().setViewportSize(width, height);
        return this;
    }

    @Override
    public BrowserActions mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        throw unsupported("Selenium HttpRequest/HttpResponse mocking");
    }

    @Override
    public NetworkInterceptionRequestBuilder interceptRequest() {
        throw unsupported("Selenium network interception builder");
    }

    @Override
    public BrowserActions intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        throw unsupported("Selenium HttpRequest/HttpResponse interception");
    }

    @Override
    public BrowserActions clearNetworkInterceptors() {
        return this;
    }

    @Override
    public BrowserActions fullScreenWindow() {
        return maximizeWindow();
    }

    @Override
    public BrowserActions switchToWindow(String nameOrHandle) {
        for (Page candidate : session.browserContext().pages()) {
            if (candidate.url().equals(nameOrHandle) || candidate.title().equals(nameOrHandle)) {
                session.setPage(candidate);
                return this;
            }
        }
        int index = Integer.parseInt(nameOrHandle);
        session.setPage(session.browserContext().pages().get(index));
        return this;
    }

    @Override
    public BrowserActions addCookie(String key, String value) {
        session.browserContext().addCookies(List.of(new Cookie(key, value).setUrl(page().url())));
        return this;
    }

    @Override
    public org.openqa.selenium.Cookie getCookie(String cookieName) {
        return session.browserContext().cookies().stream()
                .filter(cookie -> cookie.name.equals(cookieName))
                .findFirst()
                .map(this::toSeleniumCookie)
                .orElse(null);
    }

    @Override
    public Set<org.openqa.selenium.Cookie> getAllCookies() {
        return session.browserContext().cookies().stream()
                .map(this::toSeleniumCookie)
                .collect(java.util.stream.Collectors.toCollection(java.util.LinkedHashSet::new));
    }

    @Override
    public String getCookieDomain(String cookieName) {
        org.openqa.selenium.Cookie cookie = getCookie(cookieName);
        return cookie == null ? null : cookie.getDomain();
    }

    @Override
    public String getCookieValue(String cookieName) {
        org.openqa.selenium.Cookie cookie = getCookie(cookieName);
        return cookie == null ? null : cookie.getValue();
    }

    @Override
    public String getCookiePath(String cookieName) {
        org.openqa.selenium.Cookie cookie = getCookie(cookieName);
        return cookie == null ? null : cookie.getPath();
    }

    @Override
    public BrowserActions deleteCookie(String cookieName) {
        page().evaluate("(name) => document.cookie = `${name}=; Max-Age=0; path=/`", cookieName);
        return this;
    }

    @Override
    public BrowserActions deleteAllCookies() {
        session.browserContext().clearCookies();
        return this;
    }

    @Override
    public BrowserActions captureScreenshot() {
        return captureScreenshot(Screenshots.FULL);
    }

    @Override
    public BrowserActions captureScreenshot(Screenshots type) {
        boolean fullPage = type == Screenshots.FULL;
        byte[] screenshot = page().screenshot(new Page.ScreenshotOptions().setFullPage(fullPage));
        ReportManagerHelper.attach("Playwright Screenshot", type.name().toLowerCase(Locale.ROOT) + ".png",
                new ByteArrayInputStream(screenshot));
        ReportManager.log("Captured Playwright page screenshot.");
        return this;
    }

    @Override
    public BrowserActions captureSnapshot() {
        return capturePageSnapshot();
    }

    @Override
    public void generateLightHouseReport() {
        throw unsupported("Lighthouse report generation");
    }

    @Override
    public BrowserActions waitForLazyLoading() {
        page().waitForLoadState(LoadState.LOAD);
        return this;
    }

    @Override
    public String getContext() {
        return "PLAYWRIGHT";
    }

    @Override
    public BrowserActions setContext(String context) {
        if ("PLAYWRIGHT".equalsIgnoreCase(context) || "WEB".equalsIgnoreCase(context)) {
            return this;
        }
        throw unsupported("context switching to '" + context + "'");
    }

    @Override
    public List<String> getWindowHandles() {
        return session.browserContext().pages().stream().map(Page::url).toList();
    }

    @Override
    public List<String> getContextHandles() {
        return List.of("PLAYWRIGHT");
    }

    @Override
    public AccessibilityActions accessibility() {
        throw unsupported("Selenium axe accessibility actions");
    }

    public BrowserContext getNativeContext() {
        return session.browserContext();
    }

    public Page getNativePage() {
        return page();
    }

    public Locator locator(String selector) {
        return page().locator(selector);
    }

    private Page page() {
        return session.page();
    }

    private org.openqa.selenium.Cookie toSeleniumCookie(Cookie cookie) {
        return new org.openqa.selenium.Cookie(cookie.name, cookie.value, cookie.domain, cookie.path, null,
                Boolean.TRUE.equals(cookie.secure), Boolean.TRUE.equals(cookie.httpOnly));
    }

    private UnsupportedOperationException unsupported(String capability) {
        return new UnsupportedOperationException(capability + " is WebDriver-specific in SHAFT and is not available through the Playwright backend.");
    }
}
