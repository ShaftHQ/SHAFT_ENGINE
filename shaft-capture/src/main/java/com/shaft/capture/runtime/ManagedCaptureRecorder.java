package com.shaft.capture.runtime;

import com.shaft.capture.collector.BidiBrowserEventCollector;
import com.shaft.capture.collector.BrowserEventCollector;
import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.collector.CompositeBrowserEventCollector;
import com.shaft.capture.collector.PollingBrowserEventCollector;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.privacy.CapturePrivacyClassifier;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.BrowserStorageStateManager;
import com.shaft.listeners.TestNGListener;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.UnexpectedAlertBehaviour;
import org.openqa.selenium.UnhandledAlertException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.chromium.ChromiumOptions;
import org.openqa.selenium.devtools.Command;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.HasDevTools;
import org.openqa.selenium.edge.EdgeOptions;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Owns one SHAFT-managed browser and its deterministic capture pipeline.
 */
class ManagedCaptureRecorder {
    private static final Object SHAFT_INITIALIZATION_LOCK = new Object();
    private static final Map<String, DeviceProfile> DEVICE_PROFILES = Map.of(
            "pixel 5", new DeviceProfile(393, 851, 2.75,
                    "Mozilla/5.0 (Linux; Android 11; Pixel 5) AppleWebKit/537.36 "
                            + "(KHTML, like Gecko) Chrome/91.0.4472.77 Mobile Safari/537.36"),
            "pixel 7", new DeviceProfile(412, 915, 2.625,
                    "Mozilla/5.0 (Linux; Android 13; Pixel 7) AppleWebKit/537.36 "
                            + "(KHTML, like Gecko) Chrome/109.0.0.0 Mobile Safari/537.36"));

    private final CaptureStartRequest request;
    private final CapturePrivacyPolicy privacyPolicy;
    private final String sessionId = UUID.randomUUID().toString();
    private final Instant startedAt = Instant.now();
    private final List<String> warnings = new CopyOnWriteArrayList<>();
    private final Path profileDirectory;
    private final boolean temporaryProfileDirectory;
    private CaptureStatus.State state = CaptureStatus.State.STARTING;
    private SHAFT.GUI.WebDriver shaftDriver;
    private WebDriver driver;
    private BrowserEventCollector collector;
    private BrowserEventSink eventSink;
    private CaptureSessionStore store;
    private CaptureEventPipeline pipeline;
    private String currentUrl;
    private volatile boolean paused;
    private volatile boolean uiStopRequested;

    ManagedCaptureRecorder(CaptureStartRequest request) {
        this(request, CapturePrivacyPolicy.defaults());
    }

    ManagedCaptureRecorder(CaptureStartRequest request, CapturePrivacyPolicy privacyPolicy) {
        if (request == null) {
            throw new IllegalArgumentException("Capture start request is required.");
        }
        this.request = request;
        this.privacyPolicy = privacyPolicy == null ? CapturePrivacyPolicy.defaults() : privacyPolicy;
        Path profilesRoot = request.runtimeDirectory().resolve("profiles").toAbsolutePath().normalize();
        Path requestedProfile = request.options().userDataDirectory();
        profileDirectory = requestedProfile == null ? profilesRoot.resolve(sessionId).normalize() : requestedProfile;
        temporaryProfileDirectory = requestedProfile == null;
        if (temporaryProfileDirectory && !profileDirectory.startsWith(profilesRoot)) {
            throw new IllegalArgumentException("Capture profile directory escaped the runtime root.");
        }
        warnings.addAll(request.options().warnings());
        currentUrl = new CapturePrivacyClassifier(this.privacyPolicy)
                .sanitizeUrl(request.targetUrl()).value();
    }

    void start() {
        try {
            Files.createDirectories(profileDirectory);
            Files.createDirectories(request.outputPath().getParent());
            initializeShaftRuntime();
            configureShaft();
            shaftDriver = new SHAFT.GUI.WebDriver(request.browser().driverType(), browserOptions());
            driver = shaftDriver.getDriver();
            applyRuntimeOptions();
            store = new CaptureSessionStore(request.outputPath());
            store.start(CaptureSession.start(
                    sessionId,
                    startedAt,
                    browserMetadata(driver)));
            pipeline = new CaptureEventPipeline(
                    store,
                    request.outputPath(),
                    privacyPolicy,
                    value -> currentUrl = value,
                    this::warn);
            startEventSink();
            startCollector(driver);
            driver.navigate().to(request.targetUrl());
            if (driver instanceof JavascriptExecutor javascript) {
                javascript.executeScript(
                        com.shaft.capture.collector.BrowserEventScript.fallbackInstallation(
                                request.options().testIdAttributes(),
                                eventSinkEndpoint(),
                                eventSinkToken()));
            }
            pipeline.accept(BrowserSignal.generated(
                    "navigation",
                    safeWindowHandle(),
                    Map.of(
                            "url", driver.getCurrentUrl(),
                            "title", driver.getTitle(),
                            "width", 0,
                            "height", 0),
                    Map.of("action", "OPEN")));
            state = CaptureStatus.State.ACTIVE;
        } catch (RuntimeException | IOException exception) {
            state = CaptureStatus.State.FAILED;
            interrupt();
            throw new IllegalStateException("SHAFT Capture could not start the managed browser.", exception);
        }
    }

    synchronized CaptureStatus status() {
        CaptureReadiness readiness = readiness();
        return new CaptureStatus(
                state,
                sessionId,
                request.browser().name().toLowerCase(),
                currentUrl,
                pipeline == null ? 0 : pipeline.eventCount(),
                readiness.state(),
                readiness.warnings().isEmpty() ? warnings : readiness.warnings(),
                request.outputPath().toString(),
                false,
                ProcessHandle.current().pid(),
                startedAt);
    }

    private CaptureReadiness readiness() {
        if (store == null) {
            return new CaptureReadiness(
                    warnings.isEmpty() ? CaptureReadiness.State.READY : CaptureReadiness.State.RISKY,
                    warnings);
        }
        try {
            return CaptureReadiness.from(store.read(), warnings);
        } catch (RuntimeException exception) {
            return new CaptureReadiness(CaptureReadiness.State.RISKY, warnings);
        }
    }

    synchronized void checkpoint(String description, Checkpoint.CheckpointKind kind) {
        ensureActive();
        pipeline.checkpoint(description, kind);
    }

    synchronized CaptureStatus stop(boolean discard) {
        if (state != CaptureStatus.State.ACTIVE && state != CaptureStatus.State.STOPPING) {
            return status();
        }
        state = CaptureStatus.State.STOPPING;
        saveRuntimeArtifacts();
        closeCollectorAndPipeline();
        closeBrowser();
        if (store != null) {
            store.stop(Instant.now());
        }
        cleanupProfile();
        SHAFT.Properties.clearForCurrentThread();
        if (discard) {
            deleteCaptureFiles();
            state = CaptureStatus.State.DISCARDED;
        } else {
            state = CaptureStatus.State.COMPLETED;
        }
        return status();
    }

    synchronized CaptureStatus interrupt() {
        closeCollectorAndPipeline();
        closeBrowser();
        if (store != null) {
            try {
                CaptureSession current = store.read();
                if (current.status() == CaptureSession.SessionStatus.INCOMPLETE) {
                    store.markIncomplete(Instant.now());
                }
            } catch (RuntimeException ignored) {
                warn("The incomplete capture snapshot could not be updated after recorder failure.");
            }
        }
        cleanupProfile();
        SHAFT.Properties.clearForCurrentThread();
        if (state != CaptureStatus.State.FAILED) {
            state = CaptureStatus.State.INCOMPLETE;
        }
        return status();
    }

    synchronized boolean isBrowserAlive() {
        if (driver == null || state != CaptureStatus.State.ACTIVE) {
            return false;
        }
        try {
            return !driver.getWindowHandles().isEmpty();
        } catch (UnhandledAlertException exception) {
            return true;
        } catch (WebDriverException exception) {
            return false;
        }
    }

    WebDriver driverForTesting() {
        return driver;
    }

    void activeSessionForTesting(CaptureSessionStore store, CaptureEventPipeline pipeline) {
        this.store = store;
        this.pipeline = pipeline;
        this.state = CaptureStatus.State.ACTIVE;
    }

    private void configureShaft() {
        SHAFT.Properties.clearForCurrentThread();
        SHAFT.Properties.platform.set()
                .executionAddress("local")
                .enableBiDi(true);
        SHAFT.Properties.web.set()
                .targetBrowserName(request.browser().name().toLowerCase())
                .headlessExecution(request.headless())
                .incognitoMode(false);
        if (!request.options().saveHarPath().isBlank()) {
            SHAFT.Properties.reporting.set()
                    .traceEnabled(true)
                    .traceIncludeNetwork(true);
        }
    }

    private static void initializeShaftRuntime() throws IOException {
        if (com.shaft.properties.internal.Properties.isInitialized()) {
            return;
        }
        synchronized (SHAFT_INITIALIZATION_LOCK) {
            if (com.shaft.properties.internal.Properties.isInitialized()) {
                return;
            }
            Files.createDirectories(Path.of("allure-results"));
            Files.createDirectories(Path.of("src", "main", "resources", "properties"));
            TestNGListener.engineSetup(ProjectStructureManager.RunType.AI_AGENT);
        }
    }

    MutableCapabilities browserOptions() {
        String profileArgument = "--user-data-dir=" + profileDirectory;
        List<String> arguments = new ArrayList<>();
        arguments.add(profileArgument);
        arguments.add("--profile-directory=Default");
        if (!request.options().userAgent().isBlank()) {
            arguments.add("--user-agent=" + request.options().userAgent());
        }
        if (!request.options().language().isBlank()) {
            arguments.add("--lang=" + request.options().language());
        }
        if (!request.options().proxyServer().isBlank()) {
            arguments.add("--proxy-server=" + request.options().proxyServer());
        }
        if (!request.options().proxyBypass().isBlank()) {
            arguments.add("--proxy-bypass-list=" + request.options().proxyBypass());
        }
        if (request.browser() == CaptureBrowser.EDGE) {
            EdgeOptions options = new EdgeOptions();
            options.addArguments(arguments);
            applyDeviceProfile(options);
            options.setAcceptInsecureCerts(request.options().ignoreHttpsErrors());
            options.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.IGNORE);
            return options;
        }
        ChromeOptions options = new ChromeOptions();
        options.addArguments(arguments);
        applyDeviceProfile(options);
        options.setAcceptInsecureCerts(request.options().ignoreHttpsErrors());
        options.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.IGNORE);
        return options;
    }

    private void applyRuntimeOptions() {
        CaptureStartOptions.Viewport viewport = request.options().viewport();
        if (viewport != null) {
            try {
                driver.manage().window().setSize(new Dimension(viewport.width(), viewport.height()));
            } catch (WebDriverException exception) {
                warn("Requested capture viewport size could not be applied.");
            }
        }
        if (!request.options().timeout().isZero()) {
            try {
                driver.manage().timeouts().pageLoadTimeout(request.options().timeout());
            } catch (WebDriverException exception) {
                warn("Requested capture timeout could not be applied.");
            }
        }
        applyProtocolRuntimeOptions();
        loadStorageState();
    }

    private void applyDeviceProfile(ChromiumOptions<?> options) {
        DeviceProfile profile = deviceProfile();
        if (profile == null) {
            return;
        }
        Map<String, Object> metrics = new LinkedHashMap<>();
        metrics.put("width", profile.width());
        metrics.put("height", profile.height());
        metrics.put("pixelRatio", profile.pixelRatio());
        metrics.put("touch", true);
        metrics.put("mobile", true);
        Map<String, Object> mobileEmulation = new LinkedHashMap<>();
        mobileEmulation.put("deviceMetrics", metrics);
        mobileEmulation.put("userAgent", request.options().userAgent().isBlank()
                ? profile.userAgent()
                : request.options().userAgent());
        options.setExperimentalOption("mobileEmulation", mobileEmulation);
    }

    private DeviceProfile deviceProfile() {
        if (request.options().deviceName().isBlank()) {
            return null;
        }
        DeviceProfile profile = DEVICE_PROFILES.get(request.options().deviceName().toLowerCase(Locale.ROOT));
        if (profile == null) {
            warn("Capture device preset `" + request.options().deviceName()
                    + "` is not bundled; set viewport and user agent instead.");
        }
        return profile;
    }

    private void applyProtocolRuntimeOptions() {
        if (!needsProtocolRuntimeOptions()) {
            return;
        }
        DevTools devTools = devTools();
        if (devTools == null) {
            return;
        }
        applyColorScheme(devTools);
        applyGeolocation(devTools);
        applyTimezone(devTools);
        applyServiceWorkerBlocking(devTools);
    }

    private boolean needsProtocolRuntimeOptions() {
        return !request.options().colorScheme().isBlank()
                || !request.options().geolocation().isBlank()
                || !request.options().timezone().isBlank()
                || request.options().blockServiceWorkers();
    }

    private DevTools devTools() {
        if (!(driver instanceof HasDevTools hasDevTools)) {
            warn("Capture browser protocol emulation is not supported by this driver.");
            return null;
        }
        try {
            DevTools devTools = hasDevTools.getDevTools();
            devTools.createSessionIfThereIsNotOne();
            return devTools;
        } catch (RuntimeException exception) {
            warn("Capture browser protocol emulation could not be initialized.");
            return null;
        }
    }

    private void applyColorScheme(DevTools devTools) {
        String colorScheme = request.options().colorScheme().toLowerCase(Locale.ROOT);
        if (colorScheme.isBlank()) {
            return;
        }
        if (!List.of("dark", "light", "no-preference").contains(colorScheme)) {
            warn("Requested capture color scheme is unsupported; use dark, light, or no-preference.");
            return;
        }
        send(devTools, "Emulation.setEmulatedMedia", Map.of(
                        "features", List.of(Map.of("name", "prefers-color-scheme", "value", colorScheme))),
                "Requested capture color scheme could not be applied.");
    }

    private void applyGeolocation(DevTools devTools) {
        if (request.options().geolocation().isBlank()) {
            return;
        }
        double[] point = geolocation();
        if (point.length == 0) {
            warn("Requested capture geolocation must be latitude,longitude.");
            return;
        }
        String origin = targetOrigin();
        if (!origin.isBlank()) {
            send(devTools, "Browser.grantPermissions", Map.of(
                            "origin", origin,
                            "permissions", List.of("geolocation")),
                    "Requested capture geolocation permission could not be granted.");
        }
        send(devTools, "Emulation.setGeolocationOverride", Map.of(
                        "latitude", point[0],
                        "longitude", point[1],
                        "accuracy", 100),
                "Requested capture geolocation could not be applied.");
    }

    private double[] geolocation() {
        String[] parts = request.options().geolocation().split("\\s*,\\s*");
        if (parts.length != 2) {
            return new double[0];
        }
        try {
            double latitude = Double.parseDouble(parts[0]);
            double longitude = Double.parseDouble(parts[1]);
            if (latitude < -90 || latitude > 90 || longitude < -180 || longitude > 180) {
                return new double[0];
            }
            return new double[] {latitude, longitude};
        } catch (NumberFormatException exception) {
            return new double[0];
        }
    }

    private void applyTimezone(DevTools devTools) {
        if (request.options().timezone().isBlank()) {
            return;
        }
        send(devTools, "Emulation.setTimezoneOverride", Map.of("timezoneId", request.options().timezone()),
                "Requested capture timezone could not be applied.");
    }

    private void applyServiceWorkerBlocking(DevTools devTools) {
        if (!request.options().blockServiceWorkers()) {
            return;
        }
        send(devTools, "Network.enable", Map.of(),
                "Requested capture service-worker blocking could not initialize network emulation.");
        send(devTools, "Network.setBypassServiceWorker", Map.of("bypass", true),
                "Requested capture service-worker blocking could not be applied.");
    }

    private void send(DevTools devTools, String command, Map<String, Object> parameters, String warning) {
        try {
            devTools.send(new Command<>(command, parameters));
        } catch (RuntimeException exception) {
            warn(warning);
        }
    }

    private void loadStorageState() {
        if (request.options().loadStoragePath().isBlank()) {
            return;
        }
        try {
            String origin = targetOrigin();
            if (!origin.isBlank()) {
                driver.navigate().to(origin);
            }
            BrowserStorageStateManager.load(driver, request.options().loadStoragePath());
        } catch (RuntimeException exception) {
            warn("Requested capture storage state could not be loaded.");
        }
    }

    private void saveRuntimeArtifacts() {
        saveStorageState();
        saveHar();
    }

    private void saveStorageState() {
        if (driver == null || request.options().saveStoragePath().isBlank()) {
            return;
        }
        try {
            BrowserStorageStateManager.save(driver, request.options().saveStoragePath());
        } catch (RuntimeException exception) {
            warn("Requested capture storage state could not be saved.");
        }
    }

    private void saveHar() {
        if (request.options().saveHarPath().isBlank()) {
            return;
        }
        if (!request.options().saveHarGlob().isBlank()) {
            warn("Capture HAR glob filtering is not yet supported; writing all observed network entries.");
        }
        try {
            Path path = Path.of(request.options().saveHarPath()).toAbsolutePath().normalize();
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            Files.writeString(path, BrowserObservabilityRecorder.drainNetworkHarJson(), StandardCharsets.UTF_8);
        } catch (IOException | RuntimeException exception) {
            warn("Requested capture HAR file could not be written.");
        }
    }

    private String targetOrigin() {
        try {
            URI uri = URI.create(request.targetUrl());
            String scheme = uri.getScheme();
            String authority = uri.getRawAuthority();
            if (scheme == null || authority == null
                    || (!"http".equalsIgnoreCase(scheme) && !"https".equalsIgnoreCase(scheme))) {
                return "";
            }
            int credentials = authority.lastIndexOf('@');
            if (credentials >= 0) {
                authority = authority.substring(credentials + 1);
            }
            return scheme + "://" + authority;
        } catch (RuntimeException exception) {
            return "";
        }
    }

    private void startCollector(WebDriver activeDriver) {
        try {
            collector = new CompositeBrowserEventCollector(List.of(
                    new BidiBrowserEventCollector(activeDriver, request.options().testIdAttributes()),
                    new PollingBrowserEventCollector(
                            activeDriver,
                            false,
                            request.options().testIdAttributes(),
                            eventSinkEndpoint(),
                            eventSinkToken())));
            collector.start(this::acceptSignal, this::warn);
        } catch (RuntimeException exception) {
            if (collector != null) {
                collector.close();
            }
            warn("WebDriver BiDi initialization failed; the compatibility listener will be used.");
            collector = new PollingBrowserEventCollector(
                    activeDriver,
                    true,
                    request.options().testIdAttributes(),
                    eventSinkEndpoint(),
                    eventSinkToken());
            collector.start(this::acceptSignal, this::warn);
        }
    }

    private void startEventSink() {
        try {
            eventSink = new BrowserEventSink(this::acceptSignal, this::warn);
            eventSink.start();
        } catch (RuntimeException exception) {
            eventSink = null;
            warn("The browser event sink could not start; fallback capture may miss fast navigations.");
        }
    }

    private String eventSinkEndpoint() {
        return eventSink == null ? "" : eventSink.endpoint();
    }

    private String eventSinkToken() {
        return eventSink == null ? "" : eventSink.eventToken();
    }

    void acceptSignal(BrowserSignal signal) {
        if (signal == null) {
            return;
        }
        switch (signal.kind()) {
            case "control" -> handleControl(signal);
            case "checkpoint" -> handleCheckpoint(signal);
            default -> {
                if (!paused && pipeline != null) {
                    pipeline.accept(signal);
                }
            }
        }
    }

    private void handleControl(BrowserSignal signal) {
        String action = signal.dataString("action").trim().toUpperCase(Locale.ROOT);
        switch (action) {
            case "PAUSE" -> paused = true;
            case "RESUME" -> paused = false;
            case "STOP" -> {
                if (!uiStopRequested) {
                    uiStopRequested = true;
                    stop(false);
                }
            }
            default -> warn("An unknown browser recording control was ignored.");
        }
    }

    private void handleCheckpoint(BrowserSignal signal) {
        String description = signal.dataString("description");
        Checkpoint.CheckpointKind kind = checkpointKind(signal.dataString("kind"));
        try {
            checkpoint(description.isBlank() ? "Captured browser checkpoint" : description, kind);
        } catch (IllegalStateException exception) {
            warn("A browser checkpoint was ignored because capture is not active.");
        }
    }

    private BrowserMetadata browserMetadata(WebDriver activeDriver) {
        Map<String, String> safeCapabilities = new LinkedHashMap<>();
        String browserName = request.browser().name().toLowerCase();
        String browserVersion = "";
        String platform = "";
        if (activeDriver instanceof HasCapabilities hasCapabilities) {
            Capabilities capabilities = hasCapabilities.getCapabilities();
            browserName = value(capabilities.getBrowserName(), browserName);
            browserVersion = value(capabilities.getBrowserVersion(), "");
            platform = value(capabilities.getPlatformName(), platform);
            putCapability(safeCapabilities, "browserName", capabilities.getBrowserName());
            putCapability(safeCapabilities, "browserVersion", capabilities.getBrowserVersion());
            putCapability(safeCapabilities, "platformName", capabilities.getPlatformName());
            putCapability(safeCapabilities, "acceptInsecureCerts",
                    capabilities.getCapability("acceptInsecureCerts"));
            putCapability(safeCapabilities, "webSocketUrl",
                    capabilities.getCapability("webSocketUrl") == null ? null : "enabled");
        }
        putCapability(safeCapabilities, "shaft:targetLanguage", request.options().targetLanguage());
        putCapability(safeCapabilities, "shaft:testIdAttribute", request.options().testIdAttribute());
        putCapability(safeCapabilities, "shaft:viewportSize", request.options().viewportSize());
        putCapability(safeCapabilities, "shaft:language", request.options().language());
        putCapability(safeCapabilities, "shaft:proxyServer", request.options().proxyServer().isBlank()
                ? null : "configured");
        putCapability(safeCapabilities, "shaft:userDataDir", request.options().userDataDirectory() == null
                ? null : "configured");
        return new BrowserMetadata(
                browserName,
                browserVersion,
                platform,
                sessionId + "-browser",
                safeCapabilities);
    }

    private void closeCollectorAndPipeline() {
        if (collector != null) {
            try {
                collector.close();
            } catch (RuntimeException ignored) {
                warn("The browser event collector had already stopped.");
            } finally {
                collector = null;
            }
        }
        if (eventSink != null) {
            try {
                eventSink.close();
            } catch (RuntimeException ignored) {
                warn("The browser event sink had already stopped.");
            } finally {
                eventSink = null;
            }
        }
        if (pipeline != null) {
            try {
                pipeline.close();
            } catch (RuntimeException ignored) {
                warn("Pending browser events could not all be finalized.");
            }
        }
    }

    private void closeBrowser() {
        if (shaftDriver != null) {
            try {
                shaftDriver.quit();
            } catch (RuntimeException ignored) {
                warn("The managed browser had already stopped before recorder cleanup.");
            } finally {
                shaftDriver = null;
                driver = null;
            }
        }
    }

    private void cleanupProfile() {
        if (!temporaryProfileDirectory) {
            return;
        }
        Path profilesRoot = request.runtimeDirectory().resolve("profiles").toAbsolutePath().normalize();
        if (!profileDirectory.startsWith(profilesRoot)) {
            warn("The temporary browser profile path was rejected during cleanup.");
            return;
        }
        try {
            if (Files.exists(profileDirectory)) {
                try (var paths = Files.walk(profileDirectory)) {
                    paths.sorted(java.util.Comparator.reverseOrder()).forEach(path -> {
                        try {
                            Files.deleteIfExists(path);
                        } catch (IOException exception) {
                            throw new ProfileCleanupException(exception);
                        }
                    });
                }
            }
        } catch (IOException | ProfileCleanupException exception) {
            warn("The temporary browser profile could not be completely removed.");
        }
    }

    private void deleteCaptureFiles() {
        try {
            Files.deleteIfExists(request.outputPath());
            Files.deleteIfExists(request.outputPath().getParent().resolve(privacyPolicy.externalDataPath()));
        } catch (IOException exception) {
            warn("Discard completed, but one local capture artifact could not be removed.");
        }
    }

    private String safeWindowHandle() {
        try {
            return driver.getWindowHandle();
        } catch (WebDriverException exception) {
            return "";
        }
    }

    private void warn(String warning) {
        if (warning != null && !warning.isBlank() && !warnings.contains(warning)) {
            warnings.add(warning);
        }
    }

    private void ensureActive() {
        if (state != CaptureStatus.State.ACTIVE) {
            throw new IllegalStateException("SHAFT Capture is not active.");
        }
    }

    private static void putCapability(Map<String, String> target, String name, Object value) {
        if (value != null && !String.valueOf(value).isBlank()) {
            target.put(name, String.valueOf(value));
        }
    }

    private static String value(Object value, String fallback) {
        return value == null || String.valueOf(value).isBlank() ? fallback : String.valueOf(value);
    }

    private static Checkpoint.CheckpointKind checkpointKind(String value) {
        try {
            return Checkpoint.CheckpointKind.valueOf(
                    value == null ? "" : value.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException exception) {
            return Checkpoint.CheckpointKind.USER_MARKER;
        }
    }

    private static final class ProfileCleanupException extends RuntimeException {
        private ProfileCleanupException(IOException cause) {
            super(cause);
        }
    }

    private record DeviceProfile(int width, int height, double pixelRatio, String userAgent) {
    }
}
