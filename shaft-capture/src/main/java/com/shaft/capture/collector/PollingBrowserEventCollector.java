package com.shaft.capture.collector;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import java.util.function.Consumer;

/**
 * Classic WebDriver fallback for browsers where BiDi collection is unavailable.
 */
public final class PollingBrowserEventCollector implements BrowserEventCollector {
    private static final Duration POLL_INTERVAL = Duration.ofMillis(100);

    private final WebDriver driver;
    private final boolean reportFallbackWarning;
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
        this(driver, true);
    }

    /**
     * Creates a classic WebDriver collector.
     *
     * @param driver active driver
     * @param reportFallbackWarning whether status should report reduced BiDi coverage
     */
    public PollingBrowserEventCollector(WebDriver driver, boolean reportFallbackWarning) {
        if (driver == null) {
            throw new IllegalArgumentException("Capture WebDriver is required.");
        }
        this.driver = driver;
        this.reportFallbackWarning = reportFallbackWarning;
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
            Set<String> windows = new LinkedHashSet<>(driver.getWindowHandles());
            for (String opened : difference(windows, lastWindows)) {
                signalConsumer.accept(BrowserSignal.generated(
                        "window_open", opened, Map.of(), Map.of()));
            }
            for (String closed : difference(lastWindows, windows)) {
                signalConsumer.accept(BrowserSignal.generated(
                        "window_close", closed, Map.of(), Map.of()));
            }
            lastWindows = Set.copyOf(windows);

            String contextId = driver.getWindowHandle();
            String currentUrl = driver.getCurrentUrl();
            if (!currentUrl.equals(lastUrl)) {
                lastUrl = currentUrl;
                signalConsumer.accept(BrowserSignal.generated(
                        "navigation",
                        contextId,
                        Map.of("url", currentUrl, "title", driver.getTitle()),
                        Map.of("action", "OPEN")));
            }

            JavascriptExecutor javascript = (JavascriptExecutor) driver;
            javascript.executeScript(BrowserEventScript.fallbackInstallation());
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
        }
    }

    private void accept(
            Object item,
            String contextId,
            Consumer<BrowserSignal> signalConsumer,
            Consumer<String> warningConsumer) {
        try {
            signalConsumer.accept(BrowserSignal.fromJson(mapper.writeValueAsString(item), contextId));
        } catch (JsonProcessingException | RuntimeException exception) {
            warningConsumer.accept("A malformed browser interaction signal was ignored.");
        }
    }

    private static Set<String> difference(Set<String> left, Set<String> right) {
        Set<String> result = new LinkedHashSet<>(left);
        result.removeAll(right);
        return result;
    }
}
