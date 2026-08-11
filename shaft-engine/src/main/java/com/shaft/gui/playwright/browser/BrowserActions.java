package com.shaft.gui.playwright.browser;

import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.Locator;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.options.Cookie;
import com.microsoft.playwright.options.LoadState;
import com.microsoft.playwright.options.ColorScheme;
import com.microsoft.playwright.options.Geolocation;
import com.microsoft.playwright.options.Media;
import com.microsoft.playwright.options.ReducedMotion;
import com.shaft.gui.driver.EmulatedColorScheme;
import com.shaft.gui.driver.EmulatedMediaType;
import com.shaft.gui.driver.EmulatedReducedMotion;
import com.shaft.driver.SHAFT;
import com.shaft.enums.internal.Screenshots;
import com.shaft.gui.browser.NetworkInterceptionRequestBuilder;
import com.shaft.gui.browser.internal.BrowserNetworkInterceptionRule;
import com.shaft.gui.browser.internal.HarReplayRules;
import com.shaft.gui.browser.internal.PlaywrightStorageStateManager;
import com.shaft.gui.browser.internal.PermissionOrigin;
import com.shaft.gui.capabilities.AutomationBackend;
import com.shaft.gui.driver.BrowserConsoleMessage;
import com.shaft.gui.playwright.internal.PlaywrightSession;
import com.shaft.gui.playwright.validation.PlaywrightBrowserValidationsBuilder;
import com.shaft.tools.io.internal.BrowserPerformanceExecutionReport;
import com.shaft.tools.io.internal.HttpContractRecorder;
import com.shaft.tools.io.internal.FailureTraceReporter;
import com.shaft.tools.io.ReportManager;
import com.shaft.tools.io.internal.ReportManagerHelper;
import com.shaft.tools.io.internal.TraceEventRecorder;
import com.shaft.validation.ValidationEnums;
import com.shaft.validation.accessibility.AccessibilityActions;
import org.openqa.selenium.WindowType;
import org.openqa.selenium.remote.http.HttpRequest;
import org.openqa.selenium.remote.http.HttpResponse;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Locale;
import java.util.Set;
import java.util.Base64;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BrowserActions implements com.shaft.gui.driver.BrowserActionsContract {
    // Matches embedded credentials in URLs (e.g. protocol://user:password@host); compiled once for reuse
    private static final Pattern EMBEDDED_PASSWORD_PATTERN = Pattern.compile(":\\/\\/.*:(.*)@");

    private final PlaywrightSession session;

    public BrowserActions(PlaywrightSession session) {
        this.session = session;
        FailureTraceReporter.activateBrowserEvidenceOwner(session);
    }

    @Override
    public BrowserActions and() {
        return this;
    }

    /** @return cohesive network observation, mocking, and replay actions */
    @Override
    public NetworkActions network() {
        return new NetworkActions(this);
    }

    @Override
    public DialogActions dialog() {
        return new DialogActions(this);
    }

    @Override
    public ContextActions context() {
        return new ContextActions(this);
    }

    @Override
    public StorageActions storage() {
        return new StorageActions(this);
    }

    @Override
    public ConsoleActions console() {
        return new ConsoleActions(this);
    }

    @Override
    public ScriptActions script() {
        return new ScriptActions(this);
    }

    @Override
    public PermissionActions permissions() {
        return new PermissionActions(this);
    }

    @Override
    public AuthenticationActions authentication() {
        return new AuthenticationActions(this);
    }

    @Override
    public DownloadActions downloads() {
        return new DownloadActions(this);
    }

    @Override
    public EmulationActions emulation() {
        return new EmulationActions(this);
    }

    void emulateViewportNamespace(int width, int height) {
        queryEmulation("screen", "viewport", width + "x" + height, () -> {
            requirePositiveDimensions(width, height);
            requireLiveSession("emulation screen viewport");
            session.setViewport(page(), width, height);
            return null;
        });
    }

    void clearViewportEmulationNamespace() {
        queryEmulation("screen", "clear-viewport", "", () -> {
            requireLiveSession("emulation screen clear viewport");
            session.clearViewport(page());
            return null;
        });
    }

    void emulateGeolocationNamespace(double latitude, double longitude, double accuracy) {
        queryEmulation("location", "geolocation", "<geolocation>", () -> {
            requireValidGeolocation(latitude, longitude, accuracy);
            requireLivePermissionContext("emulation geolocation");
            session.browserContext().setGeolocation(new Geolocation(latitude, longitude).setAccuracy(accuracy));
            return null;
        }, latitude, longitude, accuracy);
    }

    void clearGeolocationEmulationNamespace() {
        queryEmulation("location", "clear-geolocation", "", () -> {
            requireLivePermissionContext("emulation clear geolocation");
            session.browserContext().setGeolocation(null);
            return null;
        });
    }

    void emulateMediaTypeNamespace(EmulatedMediaType type) {
        queryEmulation("media", "type", String.valueOf(type), () -> {
            requireEmulationValue(type, "Media type");
            requireLiveSession("emulation media type");
            session.setMediaType(page(), switch (type) {
                case SCREEN -> Media.SCREEN;
                case PRINT -> Media.PRINT;
            });
            return null;
        });
    }

    void emulateColorSchemeNamespace(EmulatedColorScheme scheme) {
        queryEmulation("media", "color-scheme", String.valueOf(scheme), () -> {
            requireEmulationValue(scheme, "Color scheme");
            requireLiveSession("emulation color scheme");
            session.setColorScheme(page(), switch (scheme) {
                case LIGHT -> ColorScheme.LIGHT;
                case DARK -> ColorScheme.DARK;
                case NO_PREFERENCE -> ColorScheme.NO_PREFERENCE;
            });
            return null;
        });
    }

    void emulateReducedMotionNamespace(EmulatedReducedMotion motion) {
        queryEmulation("media", "reduced-motion", String.valueOf(motion), () -> {
            requireEmulationValue(motion, "Reduced motion");
            requireLiveSession("emulation reduced motion");
            session.setReducedMotion(page(), switch (motion) {
                case REDUCE -> ReducedMotion.REDUCE;
                case NO_PREFERENCE -> ReducedMotion.NO_PREFERENCE;
            });
            return null;
        });
    }

    void resetMediaEmulationNamespace() {
        queryEmulation("media", "reset", "", () -> {
            requireLiveSession("emulation media reset");
            session.resetMedia(page());
            return null;
        });
    }

    void unsupportedLiveContextEmulation(String category, String operation, String property,
                                         Object... sensitiveSourceValues) {
        if (sensitiveSourceValues != null) {
            for (Object value : sensitiveSourceValues) {
                if (value != null) {
                    FailureTraceReporter.registerSensitiveSourceValue(String.valueOf(value));
                }
            }
        }
        queryEmulation(category, operation, "", () -> {
            requireLivePermissionContext("emulation " + operation);
            throw new UnsupportedOperationException("Playwright " + operation
                    + " is fixed when BrowserContext is created; configure " + property + " before creating the session.");
        });
    }

    private <T> T queryEmulation(String category, String operation, String locator, Supplier<T> action,
                                 Object... sensitiveValues) {
        boolean sensitive = sensitiveValues != null && sensitiveValues.length > 0;
        if (sensitive) {
            FailureTraceReporter.suppressSensitiveBrowserArtifacts();
            for (Object value : sensitiveValues) {
                if (value != null) {
                    FailureTraceReporter.registerSensitiveSourceValue(String.valueOf(value));
                }
            }
        }
        var event = TraceEventRecorder.startForBackend("emulation/" + category, operation, locator,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            T result = action.get();
            if (sensitive) {
                FailureTraceReporter.registerPersistentSensitiveBrowserState(session, operation, sensitiveValues);
            } else if ("clear-geolocation".equals(operation)) {
                FailureTraceReporter.clearPersistentSensitiveBrowserState(session, "geolocation");
            }
            TraceEventRecorder.finish(event, "passed", "emulation " + category + " " + operation + " completed.",
                    null, Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            if (sensitive) {
                FailureTraceReporter.registerSensitiveThrowable(exception);
                FailureTraceReporter.registerSensitiveValues(sensitiveValues);
            }
            TraceEventRecorder.finish(event, "failed", "emulation " + category + " " + operation + " failed.",
                    exception, Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    private static void requirePositiveDimensions(int width, int height) {
        if (width < 1 || height < 1) {
            throw new IllegalArgumentException("Viewport width and height must both be positive.");
        }
    }

    private static void requireValidGeolocation(double latitude, double longitude, double accuracy) {
        if (!Double.isFinite(latitude) || latitude < -90 || latitude > 90) {
            throw new IllegalArgumentException("Latitude must be finite and between -90 and 90.");
        }
        if (!Double.isFinite(longitude) || longitude < -180 || longitude > 180) {
            throw new IllegalArgumentException("Longitude must be finite and between -180 and 180.");
        }
        if (!Double.isFinite(accuracy) || accuracy < 0) {
            throw new IllegalArgumentException("Geolocation accuracy must be finite and non-negative.");
        }
    }

    private static void requireEmulationValue(Object value, String name) {
        if (value == null) {
            throw new IllegalArgumentException(name + " must not be null.");
        }
    }

    List<com.shaft.gui.driver.BrowserDownload> downloadedFilesNamespace(DownloadActions owner) {
        return queryDownloads("all", "", () -> {
            requireLivePermissionContext("downloads all");
            return downloadedFiles(owner);
        });
    }

    private List<com.shaft.gui.driver.BrowserDownload> downloadedFiles(DownloadActions owner) {
        return session.downloadSnapshot().stream()
                .map(download -> (com.shaft.gui.driver.BrowserDownload) new PlaywrightBrowserDownload(download, owner, session))
                .toList();
    }

    com.shaft.gui.driver.BrowserDownload latestDownloadedFileNamespace(DownloadActions owner) {
        return queryDownloads("latest", "", () -> {
            requireLivePermissionContext("downloads latest");
            List<com.shaft.gui.driver.BrowserDownload> downloads = downloadedFiles(owner);
            if (downloads.isEmpty()) {
                throw new java.util.NoSuchElementException("No browser downloads are available in this Playwright session.");
            }
            return downloads.getLast();
        });
    }

    com.shaft.gui.driver.BrowserDownload waitForDownloadedFileNamespace(DownloadActions owner,
                                                                         java.util.function.Predicate<com.shaft.gui.driver.BrowserDownload> predicate,
                                                                         Runnable trigger) {
        return queryDownloads("wait-for", "", () -> {
            Objects.requireNonNull(predicate, "predicate");
            Objects.requireNonNull(trigger, "trigger");
            requireLiveSession("downloads wait-for");
            Page.WaitForDownloadOptions options = new Page.WaitForDownloadOptions()
                    .setPredicate(download -> predicate.test(new PlaywrightBrowserDownload(download, owner, session)));
            com.microsoft.playwright.Download download = page().waitForDownload(options, trigger);
            session.trackDownload(download);
            return new PlaywrightBrowserDownload(download, owner, session);
        });
    }

    void clearDownloadedFilesNamespace() {
        queryDownloads("clear", "", () -> {
            requireLivePermissionContext("downloads clear");
            for (com.microsoft.playwright.Download download : session.downloadSnapshot()) {
                download.delete();
                session.forgetDownload(download);
            }
            return null;
        });
    }

    private <T> T queryDownloads(String operation, String locator, Supplier<T> action) {
        var event = TraceEventRecorder.startForBackend("downloads", operation, locator,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            T result = action.get();
            TraceEventRecorder.finish(event, "passed", "downloads " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "downloads " + operation + " failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
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
        String loggedUrl = obfuscateEmbeddedPassword(targetUrl);
        return timedPageLoad("playwright.browser.navigateToURL", loggedUrl, () -> {
            page().navigate(targetUrl);
            ReportManager.log("Navigate to url \"" + loggedUrl + "\".");
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

    /**
     * Opens the target URL in a new browser tab and switches focus to it.
     *
     * @param targetUrl target URL to open
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions openNewTab(String targetUrl) {
        return navigateToURL(targetUrl, WindowType.TAB);
    }

    /**
     * Opens the target URL in a new browser window and switches focus to it.
     *
     * @param targetUrl target URL to open
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions openNewWindow(String targetUrl) {
        return navigateToURL(targetUrl, WindowType.WINDOW);
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
        authentication().navigateTo(targetUrl, username, password);
        if (targetUrlAfterAuthentication != null && !targetUrlAfterAuthentication.isBlank()) {
            timedPageLoad("playwright.browser.waitForURL", targetUrlAfterAuthentication,
                    () -> page().waitForURL(targetUrlAfterAuthentication));
        }
        return this;
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
        session.networkInterceptor().startObserving();
        HttpContractRecorder.startRecording(contractFilePath, urlContains);
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
        session.networkInterceptor().startObserving();
        HttpContractRecorder.startAssertMode(contractFilePath, urlContains);
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
        session.networkInterceptor().startObserving();
        HttpContractRecorder.startVerifyMode(contractFilePath, urlContains);
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

    /**
     * Replays recorded HAR (HTTP Archive) responses through the Playwright network interceptor.
     *
     * <p>Example:
     * <pre>{@code
     * driver.browser().routeFromHar("src/test/resources/har/checkout.har");
     * driver.browser().navigateToURL("https://shop.example/checkout");
     * }</pre>
     *
     * @param harFilePath path to a HAR 1.2 JSON file
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions routeFromHar(String harFilePath) {
        HarReplayRules.buildRules(harFilePath)
                .forEach(rule -> session.networkInterceptor().addRule(rule));
        ReportManager.log("Loaded HAR replay rules.");
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

    /**
     * Checks whether a browser alert, confirm, or prompt dialog is currently present.
     *
     * @return {@code true} when an alert is present; otherwise {@code false}
     */
    @Override
    public boolean isAlertPresent() {
        return session.isDialogSeen();
    }

    /**
     * Accepts the current browser alert, confirm, or prompt dialog.
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions acceptAlert() {
        session.acceptNextDialog();
        return this;
    }

    /**
     * Dismisses the current browser alert, confirm, or prompt dialog.
     *
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions dismissAlert() {
        session.dismissNextDialog();
        return this;
    }

    /**
     * Gets the current browser alert, confirm, or prompt dialog text.
     *
     * @return the alert text
     */
    @Override
    public String getAlertText() {
        return session.lastDialogText();
    }

    /**
     * Types text into the current browser prompt dialog.
     *
     * @param text text to type into the prompt
     * @return a self-reference to be used to chain actions
     */
    @Override
    public BrowserActions typeIntoPromptAlert(String text) {
        session.typeIntoNextPrompt(text);
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

    /**
     * Saves the current browser cookies, {@code localStorage}, and {@code sessionStorage} to a JSON file.
     *
     * <p>Produces the same JSON schema as the WebDriver backend's {@code saveStorageState}, so files are
     * interchangeable between backends. Example:
     * <pre>{@code
     * driver.browser().saveStorageState("target/auth-state.json");
     * }</pre>
     *
     * @param filePath target JSON file path
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions saveStorageState(String filePath) {
        return timedBrowserAction("playwright.browser.saveStorageState", () -> {
            PlaywrightStorageStateManager.save(session.browserContext(), page(), filePath);
            ReportManager.log("Saved Playwright browser storage state to \"" + filePath + "\".");
        });
    }

    /**
     * Loads browser cookies, {@code localStorage}, and {@code sessionStorage} from a JSON file.
     *
     * <p>Navigate to the target origin before loading storage so browser cookie domain rules can apply.
     * Reads the same JSON schema as the WebDriver backend's {@code loadStorageState}. Example:
     * <pre>{@code
     * driver.browser()
     *       .navigateToURL("https://example.com")
     *       .and().loadStorageState("target/auth-state.json");
     * }</pre>
     *
     * @param filePath source JSON file path
     * @return a self-reference to be used to chain actions
     */
    public BrowserActions loadStorageState(String filePath) {
        return timedBrowserAction("playwright.browser.loadStorageState", () -> {
            PlaywrightStorageStateManager.load(session.browserContext(), page(), filePath);
            ReportManager.log("Loaded Playwright browser storage state from \"" + filePath + "\".");
        });
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

    void requireLiveSession(String operation) {
        FailureTraceReporter.activateBrowserEvidenceOwner(session);
        if (session == null || session.browserContext() == null || session.browserContext().isClosed()
                || session.page() == null
                || session.page().isClosed() || session.browser() == null || !session.browser().isConnected()) {
            throw new UnsupportedOperationException("Operation " + operation
                    + " requires a live Playwright session. Query capabilities() again after session changes.");
        }
    }

    void performNetworkAction(String operation, Runnable action) {
        var event = TraceEventRecorder.startForBackend("network", operation, "", AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            requireLiveSession(operation);
            timedBrowserAction("playwright.browser.network." + operation, action);
            TraceEventRecorder.finish(event, "passed", "Playwright network " + operation + " completed.",
                    null, Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "Playwright network " + operation + " failed.",
                    exception, Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    void performNamespace(String category, String operation, Runnable action) {
        queryNamespace(category, operation, () -> {
            action.run();
            return null;
        });
    }

    <T> T queryNamespace(String category, String operation, Supplier<T> action) {
        return queryNamespace(category, operation, "", action);
    }

    <T> T queryNamespace(String category, String operation, String locator, Supplier<T> action) {
        var event = TraceEventRecorder.startForBackend(category, operation, locator,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            requireLiveSession(operation);
            T result = TraceEventRecorder.withoutNestedEvents(action);
            TraceEventRecorder.finish(event, "passed", category + " " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", category + " " + operation + " failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    void saveStorageStateNamespace(String filePath) {
        queryNamespace("storage", "save-state", filePath, () -> saveStorageState(filePath));
    }

    void loadStorageStateNamespace(String filePath) {
        queryNamespace("storage", "load-state", filePath, () -> loadStorageState(filePath));
    }

    String getStorageValue(String scope, String key) {
        return queryNamespace("storage", scope + "/get", key, () -> {
            requireStorageKey(key);
            Object value = page().evaluate("([scope, key]) => window[scope].getItem(key)", List.of(scope, key));
            return value == null ? null : String.valueOf(value);
        });
    }

    void setStorageValue(String scope, String key, String value) {
        performSensitiveStorageWrite(scope + "/set", key, value, () -> {
            requireStorageKey(key);
            Objects.requireNonNull(value, "value");
                page().evaluate("([scope, key, value]) => window[scope].setItem(key, value)",
                        List.of(scope, key, value));
        });
    }

    void removeStorageValue(String scope, String key) {
        queryNamespace("storage", scope + "/remove", key, () -> {
            requireStorageKey(key);
            page().evaluate("([scope, key]) => window[scope].removeItem(key)", List.of(scope, key));
            return null;
        });
    }

    void clearStorage(String scope) {
        queryNamespace("storage", scope + "/clear", () -> page().evaluate("scope => window[scope].clear()", scope));
    }

    private void requireStorageKey(String key) {
        Objects.requireNonNull(key, "key");
    }

    private void performSensitiveStorageWrite(String operation, String key, String value, Runnable action) {
        var event = TraceEventRecorder.startForBackend("storage", operation, key,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            requireLiveSession(operation);
            TraceEventRecorder.withoutNestedEvents(() -> {
                action.run();
                return null;
            });
            TraceEventRecorder.finish(event, "passed", "storage " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveValue(value);
            TraceEventRecorder.finish(event, "failed", "storage " + operation + " failed.",
                    exception, Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    List<BrowserConsoleMessage> consoleMessages(String operation, boolean errorsOnly) {
        return queryNamespace("console", operation, () -> session.consoleSnapshot().stream()
                .map(entry -> new BrowserConsoleMessage(entry.source(), entry.level(), entry.message(), entry.timestamp()))
                .filter(message -> !errorsOnly || message.isError())
                .toList());
    }

    void clearConsoleNamespace() {
        queryNamespace("console", "clear", () -> {
            session.clearConsole();
            return null;
        });
    }

    Object evaluateScriptNamespace(boolean asynchronous, boolean hasArgument, String script, Object argument) {
        Objects.requireNonNull(script, "script");
        String operation = asynchronous ? "evaluate-async" : "evaluate";
        var event = TraceEventRecorder.startForBackend("script", operation, "",
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            requireLiveSession(operation);
            Object result = TraceEventRecorder.withoutNestedEvents(
                    () -> hasArgument ? page().evaluate(script, argument) : page().evaluate(script));
            TraceEventRecorder.finish(event, "passed", "script " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveThrowable(exception);
            if (hasArgument) {
                FailureTraceReporter.registerSensitiveValues(argument);
            }
            TraceEventRecorder.finish(event, "failed", "script " + operation + " failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    void registerBasicAuthenticationNamespace(String origin, String username, String password) {
        queryAuthentication("register-basic", authenticationOriginTraceLocator(origin), username, password, () -> {
            registerBasicAuthentication(origin, username, password);
            return null;
        }, origin == null);
    }

    void navigateWithBasicAuthenticationNamespace(String url, String username, String password) {
        Objects.requireNonNull(url, "url");
        queryAuthentication("navigate-basic", authenticationTraceLocator(url), username, password, () -> {
            URI target = URI.create(url);
            if (target.getRawUserInfo() != null) {
                throw new IllegalArgumentException("Authentication navigation URL must not embed credentials.");
            }
            String origin = PermissionOrigin.normalize(target.getScheme() + "://" + target.getRawAuthority());
            registerBasicAuthentication(origin, username, password);
            page().navigate(url);
            return null;
        }, true);
    }

    private static String authenticationTraceLocator(String url) {
        try {
            URI target = URI.create(url);
            if (target.getRawUserInfo() != null) {
                return "<credential-bearing-url>";
            }
            return PermissionOrigin.normalize(target.getScheme() + "://" + target.getRawAuthority());
        } catch (RuntimeException ignored) {
            return "<invalid-url>";
        }
    }

    private static String authenticationOriginTraceLocator(String origin) {
        if (origin == null) {
            return "<current-origin>";
        }
        try {
            URI target = URI.create(origin);
            if (target.getRawUserInfo() != null) {
                return "<credential-bearing-origin>";
            }
            return PermissionOrigin.normalize(origin);
        } catch (RuntimeException ignored) {
            return "<invalid-origin>";
        }
    }

    void clearAuthenticationNamespace() {
        queryAuthentication("clear", null, "", "", () -> {
            session.networkInterceptor().clearAuthentication();
            return null;
        }, false);
    }

    private void registerBasicAuthentication(String origin, String username, String password) {
        String candidateOrigin = origin;
        if (candidateOrigin == null) {
            URI current = URI.create(page().url());
            candidateOrigin = current.getScheme() + "://" + current.getRawAuthority();
        }
        String normalizedOrigin = PermissionOrigin.normalize(candidateOrigin);
        String token = Base64.getEncoder().encodeToString((username + ":" + password)
                .getBytes(StandardCharsets.UTF_8));
        session.networkInterceptor().registerBasicAuthentication(normalizedOrigin, "Basic " + token);
    }

    private <T> T queryAuthentication(String operation, String locator, String username, String password,
                                      Supplier<T> action, boolean pageRequired) {
        var event = TraceEventRecorder.startForBackend("authentication", operation, locator == null ? "" : locator,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            Objects.requireNonNull(username, "username");
            Objects.requireNonNull(password, "password");
            if (username.contains(":")) {
                throw new IllegalArgumentException("HTTP Basic usernames must not contain ':'.");
            }
            FailureTraceReporter.registerSensitiveSourceValue(username);
            FailureTraceReporter.registerSensitiveSourceValue(password);
            if (pageRequired) {
                requireLiveSession(operation);
            } else {
                requireLivePermissionContext(operation);
            }
            T result = TraceEventRecorder.withoutNestedEvents(action);
            TraceEventRecorder.finish(event, "passed", "authentication " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            FailureTraceReporter.registerSensitiveThrowable(exception);
            FailureTraceReporter.registerSensitiveValues(username);
            FailureTraceReporter.registerSensitiveValues(password);
            TraceEventRecorder.finish(event, "failed", "authentication " + operation + " failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    void setPermissionsNamespace(String stateName, String origin, String... permissionNames) {
        String locator = origin == null ? "" : origin;
        queryPermissionNamespace(stateName, locator, () -> {
            List<String> permissions = validatedPermissionNames(permissionNames);
            if (!stateName.equals("grant")) {
                throw new UnsupportedOperationException("Playwright supports granting and clearing permissions, but not explicit "
                        + stateName + "; use clear() or recreate the browser context.");
            }
            if (origin == null) {
                session.browserContext().grantPermissions(permissions);
            } else {
                session.browserContext().grantPermissions(permissions,
                        new BrowserContext.GrantPermissionsOptions().setOrigin(PermissionOrigin.normalize(origin)));
            }
            return null;
        });
    }

    void clearPermissionsNamespace() {
        queryPermissionNamespace("clear", "", () -> {
            session.browserContext().clearPermissions();
            return null;
        });
    }

    private <T> T queryPermissionNamespace(String operation, String locator, Supplier<T> action) {
        var event = TraceEventRecorder.startForBackend("permissions", operation, locator,
                AutomationBackend.MICROSOFT_PLAYWRIGHT);
        try {
            requireLivePermissionContext(operation);
            T result = TraceEventRecorder.withoutNestedEvents(action);
            TraceEventRecorder.finish(event, "passed", "permissions " + operation + " completed.", null,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            return result;
        } catch (RuntimeException exception) {
            TraceEventRecorder.finish(event, "failed", "permissions " + operation + " failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
    }

    private void requireLivePermissionContext(String operation) {
        FailureTraceReporter.activateBrowserEvidenceOwner(session);
        if (session == null || session.browserContext() == null || session.browserContext().isClosed()
                || session.browser() == null || !session.browser().isConnected()) {
            throw new UnsupportedOperationException("Operation " + operation
                    + " requires a live Playwright BrowserContext and connected Browser.");
        }
    }

    private static List<String> validatedPermissionNames(String... permissionNames) {
        if (permissionNames == null || permissionNames.length == 0) {
            throw new IllegalArgumentException("At least one permission name is required.");
        }
        return java.util.Arrays.stream(permissionNames)
                .map(name -> Objects.requireNonNull(name, "permission name").trim())
                .peek(name -> {
                    if (name.isEmpty()) {
                        throw new IllegalArgumentException("Permission names must not be blank.");
                    }
                }).toList();
    }

    NetworkInterceptionRequestBuilder<BrowserActions> networkInterceptRequest() {
        try {
            requireLiveSession("interception");
        } catch (RuntimeException exception) {
            var event = TraceEventRecorder.startForBackend("network", "intercept-request", "",
                    AutomationBackend.MICROSOFT_PLAYWRIGHT);
            TraceEventRecorder.finish(event, "failed", "Playwright network intercept-request failed.", exception,
                    Map.of("backend", "MICROSOFT_PLAYWRIGHT"), List.of());
            throw exception;
        }
        return new NetworkInterceptionRequestBuilder<>(this, (rule, message) -> {
            performNetworkAction("intercept-request", () -> registerNetworkInterceptionRule(rule, message));
            return this;
        });
    }

    void startNetworkContractRecording(String contractFilePath, String... urlContains) {
        initializeNetworkContract(() -> HttpContractRecorder.startRecording(contractFilePath, urlContains));
        ReportManager.log("Started HTTP contract recording.");
    }

    void startNetworkContractAssertion(String contractFilePath, String... urlContains) {
        initializeNetworkContract(() -> HttpContractRecorder.startAssertMode(contractFilePath, urlContains));
        ReportManager.log("Started HTTP contract assertion mode.");
    }

    void startNetworkContractVerification(String contractFilePath, String... urlContains) {
        initializeNetworkContract(() -> HttpContractRecorder.startVerifyMode(contractFilePath, urlContains));
        ReportManager.log("Started HTTP contract verification mode.");
    }

    private void initializeNetworkContract(Runnable initializer) {
        session.networkInterceptor().startObserving();
        try {
            initializer.run();
        } catch (RuntimeException exception) {
            HttpContractRecorder.clear();
            session.networkInterceptor().stopObserving();
            throw exception;
        }
    }

    public Page getNativePage() {
        return page();
    }

    public Locator locator(String selector) {
        return page().locator(selector);
    }

    private Page page() {
        FailureTraceReporter.activateBrowserEvidenceOwner(session);
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

    private String obfuscateEmbeddedPassword(String targetUrl) {
        Matcher matcher = EMBEDDED_PASSWORD_PATTERN.matcher(targetUrl);
        if (matcher.find()) {
            return targetUrl.replaceAll(matcher.group(1), "•".repeat(matcher.group(1).length()));
        }
        return targetUrl;
    }

    private org.openqa.selenium.Cookie toSeleniumCookie(Cookie cookie) {
        return new org.openqa.selenium.Cookie(cookie.name, cookie.value, cookie.domain, cookie.path, null,
                Boolean.TRUE.equals(cookie.secure), Boolean.TRUE.equals(cookie.httpOnly));
    }

    private UnsupportedOperationException unsupported(String capability) {
        return new UnsupportedOperationException(capability + " is WebDriver-specific in SHAFT and is not available through the Playwright backend.");
    }
}
