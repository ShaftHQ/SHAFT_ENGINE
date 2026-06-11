package com.shaft.capture.runtime;

import com.shaft.capture.collector.BidiBrowserEventCollector;
import com.shaft.capture.collector.BrowserEventCollector;
import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.collector.CompositeBrowserEventCollector;
import com.shaft.capture.collector.PollingBrowserEventCollector;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.shaft.capture.privacy.CapturePrivacyClassifier;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import com.shaft.driver.SHAFT;
import com.shaft.listeners.TestNGListener;
import com.shaft.tools.io.internal.ProjectStructureManager;
import org.openqa.selenium.Capabilities;
import org.openqa.selenium.HasCapabilities;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.UnexpectedAlertBehaviour;
import org.openqa.selenium.UnhandledAlertException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.edge.EdgeOptions;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Owns one SHAFT-managed browser and its deterministic capture pipeline.
 */
class ManagedCaptureRecorder {
    private static final Object SHAFT_INITIALIZATION_LOCK = new Object();

    private final CaptureStartRequest request;
    private final CapturePrivacyPolicy privacyPolicy;
    private final String sessionId = UUID.randomUUID().toString();
    private final Instant startedAt = Instant.now();
    private final List<String> warnings = new CopyOnWriteArrayList<>();
    private final Path profileDirectory;
    private CaptureStatus.State state = CaptureStatus.State.STARTING;
    private SHAFT.GUI.WebDriver shaftDriver;
    private WebDriver driver;
    private BrowserEventCollector collector;
    private CaptureSessionStore store;
    private CaptureEventPipeline pipeline;
    private String currentUrl;

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
        profileDirectory = profilesRoot.resolve(sessionId).normalize();
        if (!profileDirectory.startsWith(profilesRoot)) {
            throw new IllegalArgumentException("Capture profile directory escaped the runtime root.");
        }
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
            startCollector(driver);
            driver.navigate().to(request.targetUrl());
            if (driver instanceof JavascriptExecutor javascript) {
                javascript.executeScript(
                        com.shaft.capture.collector.BrowserEventScript.fallbackInstallation());
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

    CaptureStatus status() {
        return new CaptureStatus(
                state,
                sessionId,
                request.browser().name().toLowerCase(),
                currentUrl,
                pipeline == null ? 0 : pipeline.eventCount(),
                warnings,
                request.outputPath().toString(),
                false,
                ProcessHandle.current().pid(),
                startedAt);
    }

    void checkpoint(String description, Checkpoint.CheckpointKind kind) {
        ensureActive();
        pipeline.checkpoint(description, kind);
    }

    CaptureStatus stop(boolean discard) {
        if (state != CaptureStatus.State.ACTIVE && state != CaptureStatus.State.STOPPING) {
            return status();
        }
        state = CaptureStatus.State.STOPPING;
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

    CaptureStatus interrupt() {
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

    boolean isBrowserAlive() {
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

    private void configureShaft() {
        SHAFT.Properties.clearForCurrentThread();
        SHAFT.Properties.platform.set()
                .executionAddress("local")
                .enableBiDi(true);
        SHAFT.Properties.web.set()
                .targetBrowserName(request.browser().name().toLowerCase())
                .headlessExecution(request.headless())
                .incognitoMode(false);
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

    private MutableCapabilities browserOptions() {
        String profileArgument = "--user-data-dir=" + profileDirectory;
        if (request.browser() == CaptureBrowser.EDGE) {
            EdgeOptions options = new EdgeOptions();
            options.addArguments(profileArgument, "--profile-directory=Default");
            options.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.IGNORE);
            return options;
        }
        ChromeOptions options = new ChromeOptions();
        options.addArguments(profileArgument, "--profile-directory=Default");
        options.setUnhandledPromptBehaviour(UnexpectedAlertBehaviour.IGNORE);
        return options;
    }

    private void startCollector(WebDriver activeDriver) {
        try {
            collector = new CompositeBrowserEventCollector(List.of(
                    new BidiBrowserEventCollector(activeDriver),
                    new PollingBrowserEventCollector(activeDriver, false)));
            collector.start(pipeline::accept, this::warn);
        } catch (RuntimeException exception) {
            if (collector != null) {
                collector.close();
            }
            warn("WebDriver BiDi initialization failed; the compatibility listener will be used.");
            collector = new PollingBrowserEventCollector(activeDriver);
            collector.start(pipeline::accept, this::warn);
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

    private static final class ProfileCleanupException extends RuntimeException {
        private ProfileCleanupException(IOException cause) {
            super(cause);
        }
    }
}
