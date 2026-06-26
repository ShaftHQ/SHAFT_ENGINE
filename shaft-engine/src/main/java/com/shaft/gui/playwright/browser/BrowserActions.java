package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.Cookie;
import com.microsoft.playwright.options.LoadState;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.NetworkInterceptionRequestBuilder;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.validation.PlaywrightBrowserValidationsBuilder;
import com.shaft.tools.io.internal.BrowserPerformanceExecutionReport;
import com.shaft.tools.io.internal.HttpContractRecorder;
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

public class BrowserActions implements com.shaft.gui.driver.BrowserActionsContract {
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
        return timedBrowserAction("playwright.browser.capturePageSnapshot", () -> {
            ReportManagerHelper.attach("Playwright Page Snapshot", "page.html", page().content());
            ReportManager.log("Captured Playwright page snapshot.");
        });
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
        return session.pageHandle(page());
    }

    @Override
    public String getWindowPosition() {
        return String.valueOf(page().evaluate("() => `(${window.screenX}, ${window.screenY})`"));
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
        return timedPageLoad("playwright.browser.navigateToURL", targetUrl, () -> {
            page().navigate(targetUrl);
            ReportManager.log("Navigate to url \"" + targetUrl + "\".");
        });
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
            timedPageLoad("playwright.browser.waitForURL", targetUrlAfterRedirection,
                    () -> page().waitForURL(targetUrlAfterRedirection));
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
        return timedBrowserAction("playwright.browser.navigateBack", () -> page().goBack());
    }

    @Override
    public BrowserActions navigateForward() {
        return timedBrowserAction("playwright.browser.navigateForward", () -> page().goForward());
    }

    @Override
    public BrowserActions refreshCurrentPage() {
        return timedPageLoad("playwright.browser.refreshCurrentPage", page().url(), () -> page().reload());
    }

    @Override
    public void closeCurrentWindow() {
        long start = System.nanoTime();
        try {
            page().close();
            List<Page> pages = session.browserContext().pages();
            if (!pages.isEmpty()) {
                session.setPage(pages.getFirst());
            }
        } finally {
            BrowserPerformanceExecutionReport.recordBrowserAction(
                    "playwright.browser.closeCurrentWindow",
                    System.nanoTime() - start);
        }
    }

    @Override
    public BrowserActions maximizeWindow() {
        return setWindowSize(SHAFT.Properties.web.browserWindowWidth(), SHAFT.Properties.web.browserWindowHeight());
    }

    @Override
    public BrowserActions setWindowSize(int width, int height) {
        return timedBrowserAction("playwright.browser.setWindowSize", () -> page().setViewportSize(width, height));
    }

    @Override
    public BrowserActions mock(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        return internalIntercept(requestPredicate, mockedResponse, "Configured Playwright network response mock.");
    }

    @Override
    public NetworkInterceptionRequestBuilder<BrowserActions> interceptRequest() {
        return new NetworkInterceptionRequestBuilder<>(this, this::registerNetworkInterceptionRule);
    }

    /**
     * Starts recording selected Playwright browser traffic into an HTTP contract.
     *
     * @param contractFilePath destination JSON contract path
     * @param urlContains optional URL fragments used to select recorded traffic
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions startContractRecording(String contractFilePath, String... urlContains) {
        HttpContractRecorder.startRecording(contractFilePath, urlContains);
        session.networkInterceptor().startObserving();
        ReportManager.log("Started HTTP contract recording.");
        return this;
    }

    /**
     * Starts hard-assert validation of live Playwright responses against an HTTP contract.
     *
     * @param contractFilePath source JSON contract path
     * @param urlContains optional URL fragments used to select validated traffic
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions assertContract(String contractFilePath, String... urlContains) {
        HttpContractRecorder.startAssertMode(contractFilePath, urlContains);
        session.networkInterceptor().startObserving();
        ReportManager.log("Started HTTP contract assertion mode.");
        return this;
    }

    /**
     * Starts soft-verify validation of live Playwright responses against an HTTP contract.
     *
     * @param contractFilePath source JSON contract path
     * @param urlContains optional URL fragments used to select validated traffic
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions verifyContract(String contractFilePath, String... urlContains) {
        HttpContractRecorder.startVerifyMode(contractFilePath, urlContains);
        session.networkInterceptor().startObserving();
        ReportManager.log("Started HTTP contract verification mode.");
        return this;
    }

    /**
     * Replays recorded contract responses through the Playwright network interceptor.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().replayContract("src/test/resources/contracts/checkout.json");
     * driver.browser().navigateToURL("https://shop.example/checkout");
     * }</pre>
     *
     * @param contractFilePath source JSON contract path
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions replayContract(String contractFilePath) {
        HttpContractRecorder.browserReplayRules(contractFilePath)
                .forEach(rule -> session.networkInterceptor().addRule(rule));
        ReportManager.log("Loaded HTTP contract replay rules.");
        return this;
    }

    @Override
    public BrowserActions intercept(Predicate<HttpRequest> requestPredicate, HttpResponse mockedResponse) {
        return internalIntercept(requestPredicate, mockedResponse, "Configured Playwright network interceptor.");
    }

    @Override
    public BrowserActions clearNetworkInterceptors() {
        session.networkInterceptor().clear();
        ReportManager.log("Cleared Playwright network interceptors.");
        return this;
    }

    @Override
    public BrowserActions fullScreenWindow() {
        return maximizeWindow();
    }

    @Override
    public BrowserActions switchToWindow(String nameOrHandle) {
        Page pageByHandle = session.pageByHandle(nameOrHandle);
        if (pageByHandle != null) {
            session.setPage(pageByHandle);
            return this;
        }
        for (Page candidate : session.browserContext().pages()) {
            if (candidate.url().equals(nameOrHandle) || candidate.title().equals(nameOrHandle)) {
                session.setPage(candidate);
                return this;
            }
        }
        try {
            int index = Integer.parseInt(nameOrHandle);
            List<Page> pages = session.browserContext().pages();
            if (index < 0 || index >= pages.size()) {
                throw new IllegalArgumentException("No Playwright page exists at index " + index + ".");
            }
            session.setPage(pages.get(index));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("No Playwright page matches handle, URL, title, or index: " + nameOrHandle, e);
        }
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
        return timedBrowserAction("playwright.browser.captureScreenshot", () -> {
            boolean fullPage = type == Screenshots.FULL;
            byte[] screenshot = page().screenshot(new Page.ScreenshotOptions().setFullPage(fullPage));
            ReportManagerHelper.attach("Playwright Screenshot", type.name().toLowerCase(Locale.ROOT) + ".png",
                    new ByteArrayInputStream(screenshot));
            ReportManager.log("Captured Playwright page screenshot.");
        });
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
        return timedPageLoad("playwright.browser.waitForLazyLoading", page().url(),
                () -> page().waitForLoadState(LoadState.LOAD));
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
        return session.browserContext().pages().stream().map(session::pageHandle).toList();
    }

    @Override
    public List<String> getContextHandles() {
        return List.of("PLAYWRIGHT");
    }

    @Override
    public AccessibilityActions accessibility() {
        return new AccessibilityActions(page(), this);
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

    private BrowserActions internalIntercept(Predicate<HttpRequest> requestPredicate,
                                             HttpResponse mockedResponse,
                                             String successMessage) {
        ReportManager.logDiscrete("Configuring Playwright network interceptor for \"" + requestPredicate + "\".");
        ReportManagerHelper.attach("HTTP Response", "Mocked HTTP Response", String.valueOf(mockedResponse));
        return registerNetworkInterceptionRule(
                BrowserNetworkInterceptionRule.mock(requestPredicate, request -> mockedResponse),
                successMessage);
    }

    public BrowserActions registerNetworkInterceptionRule(BrowserNetworkInterceptionRule rule, String successMessage) {
        session.networkInterceptor().addRule(rule);
        ReportManager.log(successMessage);
        return this;
    }

    private BrowserActions timedBrowserAction(String actionName, Runnable action) {
        long start = System.nanoTime();
        try {
            action.run();
            return this;
        } finally {
            BrowserPerformanceExecutionReport.recordBrowserAction(actionName, System.nanoTime() - start);
        }
    }

    private BrowserActions timedPageLoad(String actionName, String pageName, Runnable action) {
        long start = System.nanoTime();
        try {
            action.run();
            return this;
        } finally {
            long durationNanos = System.nanoTime() - start;
            BrowserPerformanceExecutionReport.recordBrowserAction(actionName, durationNanos);
            BrowserPerformanceExecutionReport.recordPageLoad(pageName, durationNanos);
        }
    }

    private org.openqa.selenium.Cookie toSeleniumCookie(Cookie cookie) {
        return new org.openqa.selenium.Cookie(cookie.name, cookie.value, cookie.domain, cookie.path, null,
                Boolean.TRUE.equals(cookie.secure), Boolean.TRUE.equals(cookie.httpOnly));
    }

    private UnsupportedOperationException unsupported(String capability) {
        return new UnsupportedOperationException(capability + " is WebDriver-specific in SHAFT and is not available through the Playwright backend.");
    }
}
