package com.shaft.capture.runtime;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
import tools.jackson.databind.node.StringNode;
import com.shaft.capture.collector.BidiActivityGate;
import com.shaft.capture.collector.BidiBrowserEventCollector;
import com.shaft.capture.collector.BrowserEventCollector;
import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.collector.CompositeBrowserEventCollector;
import com.shaft.capture.collector.PollingBrowserEventCollector;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.model.CaptureReadiness;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.network.BodyRef;
import com.shaft.capture.network.CaptureNetworkRecorder;
import com.shaft.capture.privacy.CapturePrivacyClassifier;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.capture.storage.NetworkBodyStore;
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
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;

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
    private CaptureNetworkRecorder networkRecorder;
    private final Path networkBodiesDirectory;
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
        // Computed deterministically here (rather than only inside startNetworkRecorder()) so that
        // saveHar() can always resolve where CaptureNetworkRecorder would have written bodies, even
        // though that path depends on the same sessionId generated above.
        networkBodiesDirectory = request.outputPath().getParent()
                .resolve(sessionId + "-network-bodies").toAbsolutePath().normalize();
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
            store.start(startSession(browserMetadata(driver)));
            pipeline = new CaptureEventPipeline(
                    store,
                    request.outputPath(),
                    privacyPolicy,
                    value -> currentUrl = value,
                    this::warn);
            startEventSink();
            startCollector(driver);
            startNetworkRecorder(driver);
            driver.navigate().to(request.targetUrl());
            if (driver instanceof JavascriptExecutor javascript) {
                javascript.executeScript(
                        com.shaft.capture.collector.BrowserEventScript.fallbackInstallation(
                                request.options().testIdAttributes(),
                                eventSinkEndpoint(),
                                eventSinkToken(),
                                eventSinkStepsEndpoint(),
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

    private CaptureSession startSession(BrowserMetadata metadata) {
        CaptureSession session = CaptureSession.start(sessionId, startedAt, metadata);
        if (request.options().sessionGoal().isBlank()) {
            return session;
        }
        Map<String, tools.jackson.databind.JsonNode> extensions = new LinkedHashMap<>(session.extensions());
        extensions.put("sessionGoal", StringNode.valueOf(request.options().sessionGoal()));
        return new CaptureSession(
                session.schemaVersion(),
                session.sessionId(),
                session.status(),
                session.startedAt(),
                session.endedAt(),
                session.browser(),
                session.events(),
                session.checkpoints(),
                session.dataReferences(),
                session.redactionSummary(),
                extensions);
    }

    synchronized CaptureStatus status() {
        CaptureReadiness readiness = readiness();
        List<NetworkTransaction> transactions = networkTransactions();
        List<String> lastEndpoints = transactions.isEmpty()
                ? List.of()
                : transactions.stream()
                    .skip(Math.max(0, transactions.size() - 5))
                    .map(NetworkTransaction::url)
                    .toList();
        // Status reporting must never throw: a discarded or externally removed session file
        // otherwise turns every later capture_status/capture_stop into an error while
        // capture_start still reports "already active", wedging the whole lifecycle (issue #3429).
        int eventCount = 0;
        int pendingSignalCount = 0;
        if (pipeline != null) {
            try {
                eventCount = pipeline.eventCount();
                pendingSignalCount = pipeline.pendingSignalCount();
            } catch (RuntimeException ignored) {
                warn("The capture session file is unavailable; event counts may be stale.");
            }
        }
        return new CaptureStatus(
                state,
                sessionId,
                request.browser().name().toLowerCase(),
                currentUrl,
                eventCount,
                readiness.state(),
                readiness.warnings().isEmpty() ? warnings : readiness.warnings(),
                request.outputPath().toString(),
                false,
                ProcessHandle.current().pid(),
                startedAt,
                transactions.size(),
                lastEndpoints,
                pendingSignalCount);
    }

    /**
     * Returns the list of captured network transactions from the active session.
     *
     * @return ordered network transaction summaries, or an empty list when none have been captured
     */
    synchronized List<NetworkTransaction> networkTransactions() {
        if (store == null) {
            return List.of();
        }
        try {
            return store.networkTransactions();
        } catch (RuntimeException exception) {
            return List.of();
        }
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

    /**
     * Returns the current server-side step list so external control channels
     * (CaptureControlServer/CaptureControlClient) can source the recorder UI step list from the
     * session store instead of page-scoped browser storage.
     *
     * @return ordered safe step summaries, or an empty list before the session has started
     */
    synchronized List<com.shaft.capture.model.CaptureStep> steps() {
        if (store == null) {
            return List.of();
        }
        try {
            return store.steps();
        } catch (RuntimeException exception) {
            return List.of();
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
        // Teardown must run with a clear interrupt flag: an interrupted thread makes NIO
        // session-store writes throw ClosedByInterruptException and WebDriver.quit() abort,
        // orphaning the browser. The flag is restored afterwards for the caller.
        boolean wasInterrupted = Thread.interrupted();
        try {
            state = CaptureStatus.State.STOPPING;
            saveRuntimeArtifacts();
            closeCollectorAndPipeline();
            // closeCollectorAndPipeline() may have interrupted this thread if a collector's own
            // executor thread is (indirectly) the caller; clear again before quitting the browser.
            wasInterrupted |= Thread.interrupted();
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
        } finally {
            if (wasInterrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }

    synchronized CaptureStatus interrupt() {
        // Same interrupt hygiene as stop(): shutdown paths (JVM exit, manager close) can reach
        // this on an already-interrupted thread, where NIO writes and WebDriver.quit() would fail.
        boolean wasInterrupted = Thread.interrupted();
        try {
            closeCollectorAndPipeline();
            wasInterrupted |= Thread.interrupted();
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
        } finally {
            if (wasInterrupted) {
                Thread.currentThread().interrupt();
            }
        }
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

    /**
     * Returns the directory API-capture network bodies would be persisted to for this session, so
     * tests can plant {@link NetworkBodyStore}-written fixture bodies at the exact path
     * {@link #saveHar()} reads back from without launching a real browser/{@code CaptureNetworkRecorder}.
     */
    Path networkBodiesDirectoryForTesting() {
        return networkBodiesDirectory;
    }

    /**
     * Returns the live SHAFT driver wrapping the recorded browser while the session is ACTIVE, or
     * {@code null} otherwise. Lets MCP element tools drive the recorded browser during
     * agent-performed codegen sessions; driven interactions are captured like user ones.
     */
    synchronized SHAFT.GUI.WebDriver activeShaftDriver() {
        return state == CaptureStatus.State.ACTIVE ? shaftDriver : null;
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
        try {
            String harJson = buildHarJson();
            String glob = request.options().saveHarGlob();
            if (!glob.isBlank()) {
                HarGlobFilterResult filtered = filterHarByGlob(harJson, glob);
                harJson = filtered.json();
                warn("Capture HAR glob filter kept " + filtered.kept() + " and dropped " + filtered.dropped()
                        + " of " + (filtered.kept() + filtered.dropped()) + " observed network entries.");
            }
            Path path = Path.of(request.options().saveHarPath()).toAbsolutePath().normalize();
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            Files.writeString(path, harJson, StandardCharsets.UTF_8);
        } catch (IOException | RuntimeException exception) {
            warn("Requested capture HAR file could not be written.");
        }
    }

    /**
     * Builds the HAR-like JSON document {@link #saveHar()} writes, honoring
     * {@link CaptureStartOptions#saveHarContent()}.
     *
     * <p>The truncated-preview trace buffer ({@link BrowserObservabilityRecorder}) is always drained
     * (and therefore cleared) exactly once here, regardless of content mode, so per-thread trace state
     * never leaks into a later session recorded on the same thread.
     *
     * <p>When full-body content is requested and available, entries are sourced entirely from this
     * session's persisted {@code CaptureEvent.NetworkEvent}s (recorded by {@code CaptureNetworkRecorder},
     * with headers already redacted via {@code SecretHeaderReplacer}) rather than merged with the
     * truncated-preview trace entries. Both mechanisms observe the same DevTools traffic and the
     * network-event list is a strict superset in practice (it is the authoritative capture path), so
     * replacing rather than correlating avoids a fragile method+URL+timestamp matching heuristic
     * between two independently-populated event lists. The one documented limitation: transactions
     * that {@code CaptureNetworkRecorder}'s own filters (asset/first-party/glob/transaction-cap) drop
     * are absent from the full-body HAR even if the truncated-preview trace observed them, since the
     * preview trace has no equivalent filtering.
     *
     * @return the HAR-like JSON document to write
     */
    private String buildHarJson() {
        String previewHarJson = BrowserObservabilityRecorder.drainNetworkHarJson();
        String mode = request.options().saveHarContent().toLowerCase(Locale.ROOT);
        if (mode.isBlank() || "none".equals(mode)) {
            return previewHarJson;
        }
        if (!"full".equals(mode)) {
            warn("Capture HAR content mode `" + request.options().saveHarContent()
                    + "` is not supported; use `full` for complete request/response bodies. "
                    + "Falling back to truncated network trace previews.");
            return previewHarJson;
        }
        List<CaptureEvent.NetworkEvent> fullBodyEvents = fullBodyNetworkEvents();
        if (fullBodyEvents.isEmpty()) {
            warn("Capture HAR content mode `full` was requested, but no API-capture network "
                    + "transactions were recorded for this session (API capture may not be enabled); "
                    + "falling back to truncated network trace previews.");
            return previewHarJson;
        }
        return fullBodyHarJson(fullBodyEvents);
    }

    private List<CaptureEvent.NetworkEvent> fullBodyNetworkEvents() {
        if (store == null) {
            return List.of();
        }
        try {
            return store.read().events().stream()
                    .filter(CaptureEvent.NetworkEvent.class::isInstance)
                    .map(CaptureEvent.NetworkEvent.class::cast)
                    .toList();
        } catch (RuntimeException exception) {
            return List.of();
        }
    }

    /**
     * Builds a HAR-like JSON document whose entries carry full request/response bodies read back
     * from {@link NetworkBodyStore}, keeping the same top-level shape
     * {@link BrowserObservabilityRecorder#networkHarJson} produces (so {@link #filterHarByGlob} keeps
     * working unchanged) while adding nested {@code requestBody}/{@code responseBody} objects.
     */
    private String fullBodyHarJson(List<CaptureEvent.NetworkEvent> events) {
        NetworkBodyStore bodyStore = new NetworkBodyStore();
        ObjectMapper mapper = JsonMapper.builder().build();
        ArrayNode entries = mapper.createArrayNode();
        for (CaptureEvent.NetworkEvent event : events) {
            entries.add(harEntry(mapper, bodyStore, event));
        }
        ObjectNode creator = mapper.createObjectNode();
        creator.put("name", "SHAFT");
        creator.put("comment", "HAR-like browser network trace emitted by SHAFT observability "
                + "(full-body API capture)");
        ObjectNode log = mapper.createObjectNode();
        log.put("version", "1.2");
        log.set("creator", creator);
        log.set("entries", entries);
        ObjectNode root = mapper.createObjectNode();
        root.set("log", log);
        return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(root) + System.lineSeparator();
    }

    private ObjectNode harEntry(ObjectMapper mapper, NetworkBodyStore bodyStore, CaptureEvent.NetworkEvent event) {
        var httpRequest = event.request();
        var httpResponse = event.response();
        ObjectNode entry = mapper.createObjectNode();
        entry.put("method", httpRequest.method());
        entry.put("url", httpRequest.url());
        entry.put("status", httpResponse == null ? 0 : httpResponse.statusCode());
        entry.put("durationMs", networkTimingMillis(event.timing()));
        entry.put("requestSizeBytes", httpRequest.body() == null ? 0 : httpRequest.body().sizeBytes());
        entry.put("responseSizeBytes", httpResponse == null || httpResponse.body() == null
                ? 0 : httpResponse.body().sizeBytes());
        entry.set("requestHeaders", headersNode(mapper, httpRequest.headers()));
        entry.set("responseHeaders", headersNode(mapper, httpResponse == null ? Map.of() : httpResponse.headers()));
        entry.put("failureReason", event.failureReason());
        entry.set("requestBody", bodyNode(mapper, bodyStore, httpRequest.body()));
        entry.set("responseBody", bodyNode(mapper, bodyStore, httpResponse == null ? null : httpResponse.body()));
        return entry;
    }

    private static ObjectNode headersNode(ObjectMapper mapper, Map<String, String> headers) {
        ObjectNode node = mapper.createObjectNode();
        headers.forEach(node::put);
        return node;
    }

    /**
     * Reads a body's bytes from {@link #networkBodiesDirectory} and inlines them as {@code text} for
     * textual content types, or as base64 with {@code encoding: "base64"} for everything else (mirroring
     * the real HAR spec's {@code content.encoding} convention: present and {@code "base64"} for binary,
     * absent/blank for text).
     */
    private ObjectNode bodyNode(ObjectMapper mapper, NetworkBodyStore bodyStore, BodyRef bodyRef) {
        ObjectNode node = mapper.createObjectNode();
        if (bodyRef == null) {
            node.put("mimeType", "");
            node.put("size", 0L);
            node.put("truncated", false);
            node.put("encoding", "");
            node.put("text", "");
            return node;
        }
        byte[] bytes = bodyStore.readBytes(bodyRef, networkBodiesDirectory);
        node.put("mimeType", bodyRef.encoding());
        node.put("size", bodyRef.sizeBytes());
        node.put("truncated", bodyRef.truncated());
        if (bytes.length == 0) {
            node.put("encoding", "");
            node.put("text", "");
        } else if (isTextualContentType(bodyRef.encoding())) {
            node.put("encoding", "");
            node.put("text", new String(bytes, StandardCharsets.UTF_8));
        } else {
            node.put("encoding", "base64");
            node.put("text", Base64.getEncoder().encodeToString(bytes));
        }
        return node;
    }

    private static boolean isTextualContentType(String contentType) {
        if (contentType == null || contentType.isBlank()) {
            return true;
        }
        String lower = contentType.toLowerCase(Locale.ROOT);
        return lower.startsWith("text/") || lower.contains("json") || lower.contains("xml")
                || lower.contains("javascript") || lower.contains("html") || lower.contains("css")
                || lower.contains("urlencoded") || lower.contains("csv") || lower.contains("yaml");
    }

    private static long networkTimingMillis(com.shaft.capture.model.network.NetworkTiming timing) {
        if (timing == null) {
            return 0L;
        }
        return java.util.stream.Stream.of(
                        timing.blocked(), timing.dns(), timing.connect(),
                        timing.send(), timing.ttfb(), timing.receive())
                .filter(java.util.Objects::nonNull)
                .mapToLong(java.time.Duration::toMillis)
                .sum();
    }

    /**
     * Filters a HAR-like JSON document's {@code log.entries} to those whose {@code url} matches the
     * given glob, reusing {@link CaptureNetworkRecorder#matchesGlob(String, String)} so both live
     * network filtering and HAR export share one glob implementation.
     *
     * @param harJson HAR-like JSON produced by {@code BrowserObservabilityRecorder.drainNetworkHarJson()}
     * @param glob    non-blank URL glob
     * @return the filtered JSON alongside how many entries were kept/dropped
     */
    static HarGlobFilterResult filterHarByGlob(String harJson, String glob) {
        ObjectMapper mapper = JsonMapper.builder().build();
        JsonNode root = mapper.readTree(harJson);
        JsonNode entriesNode = root.path("log").path("entries");
        if (!(entriesNode instanceof ArrayNode entries) || !(root.path("log") instanceof ObjectNode log)) {
            return new HarGlobFilterResult(harJson, 0, 0);
        }
        ArrayNode kept = entries.arrayNode();
        for (JsonNode entry : entries) {
            if (CaptureNetworkRecorder.matchesGlob(entry.path("url").asText(""), glob)) {
                kept.add(entry);
            }
        }
        log.set("entries", kept);
        String json = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(root) + System.lineSeparator();
        return new HarGlobFilterResult(json, kept.size(), entries.size() - kept.size());
    }

    /**
     * Result of {@link #filterHarByGlob(String, String)}.
     *
     * @param json    filtered HAR JSON
     * @param kept    entries retained by the glob
     * @param dropped entries excluded by the glob
     */
    record HarGlobFilterResult(String json, int kept, int dropped) {
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
            BidiActivityGate bidiActivityGate = new BidiActivityGate();
            BidiBrowserEventCollector bidiCollector = new BidiBrowserEventCollector(
                    activeDriver,
                    request.options().testIdAttributes(),
                    eventSinkStepsEndpoint(),
                    eventSinkToken());
            // The polling collector reads bidiActivityGate to back off its own redundant script
            // round-trips once BiDi is proven healthy; this wrapper is what feeds that gate,
            // without BidiBrowserEventCollector itself needing to know the polling collector exists.
            BrowserEventCollector trackedBidiCollector = new BrowserEventCollector() {
                @Override
                public void start(Consumer<BrowserSignal> signalConsumer, Consumer<String> warningConsumer) {
                    bidiCollector.start(signal -> {
                        bidiActivityGate.recordActivity();
                        signalConsumer.accept(signal);
                    }, warningConsumer);
                }

                @Override
                public void close() {
                    bidiCollector.close();
                }
            };
            collector = new CompositeBrowserEventCollector(List.of(
                    trackedBidiCollector,
                    new PollingBrowserEventCollector(
                            activeDriver,
                            false,
                            request.options().testIdAttributes(),
                            eventSinkEndpoint(),
                            eventSinkToken(),
                            eventSinkStepsEndpoint(),
                            bidiActivityGate)));
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
                    eventSinkToken(),
                    eventSinkStepsEndpoint());
            collector.start(this::acceptSignal, this::warn);
        }
    }

    private void startNetworkRecorder(WebDriver activeDriver) {
        if (!apiCaptureEnabled()) {
            return;
        }
        if (!(activeDriver instanceof HasDevTools)) {
            warn("API capture is enabled but this driver does not support DevTools; continuing with UI-only capture.");
            return;
        }
        try {
            networkRecorder = new CaptureNetworkRecorder(
                    activeDriver, networkBodiesDirectory, resolvedNetworkCaptureOptions(), sessionId,
                    this::acceptNetworkEvent, this::warn);
            if (!networkRecorder.start()) {
                networkRecorder = null;
            }
        } catch (RuntimeException exception) {
            networkRecorder = null;
            warn("API capture could not start for this session; continuing with UI-only capture.");
        }
    }

    private boolean apiCaptureEnabled() {
        if (request.options().apiCapture()) {
            return true;
        }
        try {
            return SHAFT.Properties.capture != null && SHAFT.Properties.capture.enabled();
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private com.shaft.capture.network.NetworkCaptureOptions resolvedNetworkCaptureOptions() {
        NetworkCaptureOptions requested = request.options().networkOptions();
        if (requested != null && !isDefaultNetworkCaptureOptions(requested)) {
            return translateNetworkCaptureOptions(requested);
        }
        try {
            var capture = SHAFT.Properties.capture;
            if (capture == null) {
                return com.shaft.capture.network.NetworkCaptureOptions.defaults();
            }
            return new com.shaft.capture.network.NetworkCaptureOptions(
                    capture.includeAssets(),
                    globList(capture.urlIncludeGlobs()),
                    globList(capture.urlExcludeGlobs()),
                    capture.firstPartyOnly(),
                    capture.maxTransactions(),
                    capture.maxBodyBytes());
        } catch (RuntimeException exception) {
            return com.shaft.capture.network.NetworkCaptureOptions.defaults();
        }
    }

    private static boolean isDefaultNetworkCaptureOptions(NetworkCaptureOptions options) {
        NetworkCaptureOptions defaults = new NetworkCaptureOptions();
        return options.enabled == defaults.enabled
                && options.excludeAssets == defaults.excludeAssets
                && options.excludePattern.equals(defaults.excludePattern)
                && options.includePattern.equals(defaults.includePattern)
                && options.captureResponseBodies == defaults.captureResponseBodies
                && options.captureRequestBodies == defaults.captureRequestBodies;
    }

    private static com.shaft.capture.network.NetworkCaptureOptions translateNetworkCaptureOptions(
            NetworkCaptureOptions options) {
        return new com.shaft.capture.network.NetworkCaptureOptions(
                !options.excludeAssets,
                globList(options.includePattern),
                globList(options.excludePattern),
                true,
                com.shaft.capture.network.NetworkCaptureOptions.DEFAULT_MAX_TRANSACTIONS,
                com.shaft.capture.network.NetworkCaptureOptions.DEFAULT_MAX_BODY_BYTES);
    }

    private static List<String> globList(String value) {
        if (value == null || value.isBlank()) {
            return List.of();
        }
        return java.util.Arrays.stream(value.split("[,|]"))
                .map(String::trim)
                .filter(entry -> !entry.isBlank())
                .toList();
    }

    void acceptNetworkEvent(CaptureNetworkRecorder.RecordedTransaction transaction) {
        if (transaction == null || store == null) {
            return;
        }
        try {
            long sequence = store.nextSequence();
            com.shaft.capture.model.PageContext page = new com.shaft.capture.model.PageContext(
                    transaction.initiatorPageUrl(), "", safeWindowHandle(), List.of(), 0, 0);
            com.shaft.capture.model.EventContext context = new com.shaft.capture.model.EventContext(
                    sequence, Instant.now(), page,
                    com.shaft.capture.model.EventContext.ReplayStatus.NOT_REPLAYED, List.of(), Map.of());
            CaptureEvent.NetworkEvent event = new CaptureEvent.NetworkEvent(
                    context,
                    transaction.transactionId(),
                    transaction.resourceKind(),
                    transaction.request(),
                    transaction.response(),
                    transaction.timing(),
                    transaction.failureReason(),
                    transaction.initiatorPageUrl(),
                    null);
            store.append(event);
        } catch (RuntimeException exception) {
            warn("A network transaction could not be persisted to the capture session.");
        }
    }

    private void startEventSink() {
        try {
            eventSink = new BrowserEventSink(this::acceptSignal, this::warn);
            eventSink.stepsSupplier(() -> store == null ? java.util.List.of() : store.steps());
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

    private String eventSinkStepsEndpoint() {
        return eventSink == null ? "" : eventSink.stepsEndpoint();
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
                    // The overlay STOP arrives on a collector-owned thread (the polling executor or
                    // the sink's HTTP handler). Running stop() inline there is self-destructive:
                    // stop() closes that very collector, whose shutdownNow() interrupts the current
                    // thread, and every later NIO session-store write and the WebDriver quit() then
                    // abort with ClosedByInterruptException/InterruptedException — the session
                    // sticks in STOPPING (later marked INCOMPLETE) and the browser plus its driver
                    // process are orphaned. A dedicated thread owns the teardown instead.
                    Thread stopThread = new Thread(() -> {
                        try {
                            stop(false);
                        } catch (RuntimeException exception) {
                            warn("The browser Stop control could not stop the recording cleanly: "
                                    + exception.getMessage());
                        }
                    }, "shaft-capture-ui-stop");
                    stopThread.setDaemon(false);
                    stopThread.start();
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
        if (networkRecorder != null) {
            try {
                networkRecorder.close();
            } catch (RuntimeException ignored) {
                warn("API capture had already stopped.");
            } finally {
                networkRecorder = null;
            }
        }
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
            if (store != null) {
                // Through the store, so its vanished-file self-heal snapshot is cleared too and
                // can never resurrect an intentionally discarded session file.
                store.discard();
            } else {
                Files.deleteIfExists(request.outputPath());
            }
            Files.deleteIfExists(request.outputPath().getParent().resolve(privacyPolicy.externalDataPath()));
        } catch (IOException | java.io.UncheckedIOException exception) {
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
