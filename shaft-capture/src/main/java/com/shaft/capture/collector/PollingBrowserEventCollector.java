package com.shaft.capture.collector;

import tools.jackson.databind.ObjectMapper;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.NoSuchSessionException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;

import java.time.Duration;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;

/**
 * Classic WebDriver fallback for browsers where BiDi collection is unavailable.
 */
public final class PollingBrowserEventCollector implements BrowserEventCollector {
    private static final Duration POLL_INTERVAL = Duration.ofMillis(100);

    private final WebDriver driver;
    private final boolean reportFallbackWarning;
    private final List<String> testIdAttributes;
    private final String eventEndpoint;
    private final String eventToken;
    private final String stepsEndpoint;
    private final BooleanSupplier recentBidiActivity;
    private final ObjectMapper mapper = new ObjectMapper();
    private final ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor(runnable -> {
        Thread thread = new Thread(runnable, "shaft-capture-polling");
        thread.setDaemon(true);
        return thread;
    });
    private final AtomicBoolean warned = new AtomicBoolean();
    private volatile String lastUrl = "";
    private Set<String> lastWindows = Set.of();

    /**
     * Creates a classic WebDriver collector.
     *
     * @param driver active driver
     */
    public PollingBrowserEventCollector(WebDriver driver) {
        this(driver, true, List.of());
    }

    /**
     * Creates a classic WebDriver collector.
     *
     * @param driver active driver
     * @param reportFallbackWarning whether status should report reduced BiDi coverage
     */
    public PollingBrowserEventCollector(WebDriver driver, boolean reportFallbackWarning) {
        this(driver, reportFallbackWarning, List.of());
    }

    /**
     * Creates a classic WebDriver collector.
     *
     * @param driver active driver
     * @param reportFallbackWarning whether status should report reduced BiDi coverage
     * @param testIdAttributes locator test-id attributes
     */
    public PollingBrowserEventCollector(
            WebDriver driver,
            boolean reportFallbackWarning,
            List<String> testIdAttributes) {
        this(driver, reportFallbackWarning, testIdAttributes, "", "");
    }

    /**
     * Creates a classic WebDriver collector.
     *
     * @param driver active driver
     * @param reportFallbackWarning whether status should report reduced BiDi coverage
     * @param testIdAttributes locator test-id attributes
     * @param eventEndpoint optional loopback event endpoint
     * @param eventToken optional loopback event token
     */
    public PollingBrowserEventCollector(
            WebDriver driver,
            boolean reportFallbackWarning,
            List<String> testIdAttributes,
            String eventEndpoint,
            String eventToken) {
        this(driver, reportFallbackWarning, testIdAttributes, eventEndpoint, eventToken, "");
    }

    /**
     * Creates a classic WebDriver collector with a steps-rehydration endpoint so the recorder UI
     * can source its step list from the server-side session store across navigations, including
     * cross-origin ones, instead of page-scoped storage.
     *
     * @param driver active driver
     * @param reportFallbackWarning whether status should report reduced BiDi coverage
     * @param testIdAttributes locator test-id attributes
     * @param eventEndpoint optional loopback event endpoint
     * @param eventToken optional loopback event token
     * @param stepsEndpoint optional loopback steps query endpoint
     */
    public PollingBrowserEventCollector(
            WebDriver driver,
            boolean reportFallbackWarning,
            List<String> testIdAttributes,
            String eventEndpoint,
            String eventToken,
            String stepsEndpoint) {
        this(driver, reportFallbackWarning, testIdAttributes, eventEndpoint, eventToken, stepsEndpoint,
                () -> false);
    }

    /**
     * Creates a classic WebDriver collector that skips its own redundant script round-trips while
     * {@code recentBidiActivity} reports that a BiDi collector running alongside it is already
     * delivering the same signals, and resumes full-rate polling the moment BiDi goes quiet.
     *
     * @param driver active driver
     * @param reportFallbackWarning whether status should report reduced BiDi coverage
     * @param testIdAttributes locator test-id attributes
     * @param eventEndpoint optional loopback event endpoint
     * @param eventToken optional loopback event token
     * @param stepsEndpoint optional loopback steps query endpoint
     * @param recentBidiActivity reports true while BiDi is proven healthy
     */
    public PollingBrowserEventCollector(
            WebDriver driver,
            boolean reportFallbackWarning,
            List<String> testIdAttributes,
            String eventEndpoint,
            String eventToken,
            String stepsEndpoint,
            BooleanSupplier recentBidiActivity) {
        if (driver == null) {
            throw new IllegalArgumentException("Capture WebDriver is required.");
        }
        this.driver = driver;
        this.reportFallbackWarning = reportFallbackWarning;
        this.testIdAttributes = testIdAttributes == null ? List.of() : List.copyOf(testIdAttributes);
        this.eventEndpoint = eventEndpoint == null ? "" : eventEndpoint;
        this.eventToken = eventToken == null ? "" : eventToken;
        this.stepsEndpoint = stepsEndpoint == null ? "" : stepsEndpoint;
        this.recentBidiActivity = recentBidiActivity == null ? () -> false : recentBidiActivity;
    }

    @Override
    public void start(Consumer<BrowserSignal> signalConsumer, Consumer<String> warningConsumer) {
        if (!(driver instanceof JavascriptExecutor)) {
            throw new IllegalArgumentException("Capture fallback requires JavaScript execution.");
        }
        if (reportFallbackWarning) {
            warningConsumer.accept(
                    "WebDriver BiDi was unavailable; using the compatibility listener. "
                            + "Cross-origin frame and prompt coverage may be limited.");
        }
        executor.scheduleWithFixedDelay(
                () -> poll(signalConsumer, warningConsumer),
                0,
                POLL_INTERVAL.toMillis(),
                TimeUnit.MILLISECONDS);
    }

    @Override
    public void close() {
        executor.shutdownNow();
    }

    private void poll(Consumer<BrowserSignal> signalConsumer, Consumer<String> warningConsumer) {
        try {
            // While BiDi is proven healthy, this cycle still walks the same WebDriver-only cursors
            // (window handles, current URL) so lastWindows/lastUrl never go stale -- otherwise the
            // moment BiDi goes quiet, resuming full polling would misread the whole gap since the
            // last real check as new and replay a burst of already-reported window/navigation
            // signals. It just skips emitting duplicates of what BiDi already delivered, and skips
            // the executeScript round-trips entirely, since those are this collector's only actual
            // overhead on the page's renderer.
            boolean bidiHealthy = recentBidiActivity.getAsBoolean();

            Set<String> windows = new LinkedHashSet<>(driver.getWindowHandles());
            if (!bidiHealthy) {
                for (String opened : difference(windows, lastWindows)) {
                    signalConsumer.accept(BrowserSignal.generated(
                            "window_open", opened, Map.of(), Map.of()));
                }
                for (String closed : difference(lastWindows, windows)) {
                    signalConsumer.accept(BrowserSignal.generated(
                            "window_close", closed, Map.of(), Map.of()));
                }
            }
            lastWindows = Set.copyOf(windows);

            String contextId = driver.getWindowHandle();
            String currentUrl = driver.getCurrentUrl();
            if (!currentUrl.equals(lastUrl)) {
                if (!bidiHealthy) {
                    signalConsumer.accept(BrowserSignal.generated(
                            "navigation",
                            contextId,
                            Map.of("url", currentUrl, "title", driver.getTitle()),
                            Map.of("action", "OPEN")));
                }
                lastUrl = currentUrl;
            }

            if (bidiHealthy) {
                return;
            }

            JavascriptExecutor javascript = (JavascriptExecutor) driver;
            javascript.executeScript(BrowserEventScript.fallbackInstallation(
                    testIdAttributes,
                    eventEndpoint,
                    eventToken,
                    stepsEndpoint,
                    eventToken));
            Object drained = javascript.executeScript(BrowserEventScript.fallbackDrain());
            if (drained instanceof List<?> signals) {
                for (Object item : signals) {
                    accept(item, contextId, signalConsumer, warningConsumer);
                }
            }
        } catch (NoSuchSessionException exception) {
            close();
        } catch (WebDriverException exception) {
            if (warned.compareAndSet(false, true)) {
                warningConsumer.accept("The compatibility listener temporarily lost browser access.");
            }
        } catch (RuntimeException exception) {
            // Any other failure must not escape: an uncaught exception would silently cancel
            // this scheduled poll forever and every later browser interaction would be lost.
            if (warned.compareAndSet(false, true)) {
                warningConsumer.accept("The compatibility listener skipped one polling cycle.");
            }
        }
    }

    private void accept(
            Object item,
            String contextId,
            Consumer<BrowserSignal> signalConsumer,
            Consumer<String> warningConsumer) {
        try {
            signalConsumer.accept(BrowserSignal.fromJson(mapper.writeValueAsString(item), contextId));
        } catch (RuntimeException exception) {
            warningConsumer.accept("A malformed browser interaction signal was ignored.");
        }
    }

    private static Set<String> difference(Set<String> left, Set<String> right) {
        Set<String> result = new LinkedHashSet<>(left);
        result.removeAll(right);
        return result;
    }
}
