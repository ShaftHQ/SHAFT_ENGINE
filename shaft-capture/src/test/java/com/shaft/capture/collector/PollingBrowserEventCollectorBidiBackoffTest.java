package com.shaft.capture.collector;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.time.Duration;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression for issue #3385: the polling collector used to re-run its executeScript
 * install/drain round-trips, and re-emit window/navigation signals, on every 100ms tick
 * regardless of whether a BiDi collector running alongside it was already delivering the same
 * signals. Asserts it backs off both once the shared {@link BidiActivityGate} reports healthy,
 * while still tracking window/URL cursors so a later BiDi outage resumes full polling without
 * replaying a stale catch-up burst for windows BiDi already reported.
 */
class PollingBrowserEventCollectorBidiBackoffTest {
    @Test
    void backsOffScriptRoundTripsWhileBidiIsHealthyAndResumesCleanlyWhenItGoesQuiet() throws Exception {
        RecordingDriver driver = new RecordingDriver();
        BidiActivityGate gate = new BidiActivityGate(Duration.ofMillis(300));
        List<BrowserSignal> signals = new CopyOnWriteArrayList<>();
        PollingBrowserEventCollector collector =
                new PollingBrowserEventCollector(driver, false, List.of(), "", "", "", gate);
        try {
            collector.start(signals::add, warning -> { });
            waitFor(() -> driver.executeScriptCalls.get() >= 2, Duration.ofSeconds(2));
            int executeScriptCallsBeforeHealthy = driver.executeScriptCalls.get();

            // Simulate a BiDi collector delivering a signal, then a new window opening -- BiDi
            // would already report this window, so polling must not duplicate it.
            gate.recordActivity();
            driver.openWindow("popup-1");
            Thread.sleep(250);

            assertEquals(executeScriptCallsBeforeHealthy, driver.executeScriptCalls.get(),
                    "executeScript must not run again while BiDi is proven healthy.");
            assertTrue(signals.stream().noneMatch(PollingBrowserEventCollectorBidiBackoffTest::isPopupWindowOpen),
                    "A window_open signal for popup-1 must not be emitted while BiDi is healthy "
                            + "and already covering it.");

            // Let the gate's grace period lapse so the collector treats BiDi as quiet again.
            waitFor(() -> driver.executeScriptCalls.get() > executeScriptCallsBeforeHealthy,
                    Duration.ofSeconds(2));

            assertTrue(signals.stream().noneMatch(PollingBrowserEventCollectorBidiBackoffTest::isPopupWindowOpen),
                    "Resuming full polling after a BiDi outage must not replay popup-1 as a new "
                            + "window; its cursor was already tracked while BiDi was healthy.");
        } finally {
            collector.close();
        }
    }

    private static boolean isPopupWindowOpen(BrowserSignal signal) {
        return "window_open".equals(signal.kind()) && "popup-1".equals(signal.browsingContextId());
    }

    private static void waitFor(BooleanSupplier condition, Duration timeout) throws InterruptedException {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(20);
        }
        assertTrue(condition.getAsBoolean());
    }

    /**
     * Minimal driver stub that counts executeScript/getWindowHandles calls and lets the test
     * simulate a new window opening mid-poll.
     */
    private static final class RecordingDriver implements WebDriver, JavascriptExecutor {
        private final AtomicInteger executeScriptCalls = new AtomicInteger();
        private final Set<String> windows = Collections.synchronizedSet(new LinkedHashSet<>(List.of("main")));

        void openWindow(String handle) {
            windows.add(handle);
        }

        @Override
        public Set<String> getWindowHandles() {
            return new LinkedHashSet<>(windows);
        }

        @Override
        public Object executeScript(String script, Object... args) {
            executeScriptCalls.incrementAndGet();
            return null;
        }

        @Override
        public Object executeAsyncScript(String script, Object... args) {
            return null;
        }

        @Override
        public void get(String url) {
        }

        @Override
        public String getCurrentUrl() {
            return "http://example.test/";
        }

        @Override
        public String getTitle() {
            return "";
        }

        @Override
        public List<WebElement> findElements(By by) {
            return List.of();
        }

        @Override
        public WebElement findElement(By by) {
            throw new org.openqa.selenium.NoSuchElementException("not used");
        }

        @Override
        public String getPageSource() {
            return "";
        }

        @Override
        public void close() {
        }

        @Override
        public void quit() {
        }

        @Override
        public String getWindowHandle() {
            return "main";
        }

        @Override
        public TargetLocator switchTo() {
            throw new UnsupportedOperationException("not used");
        }

        @Override
        public Navigation navigate() {
            throw new UnsupportedOperationException("not used");
        }

        @Override
        public Options manage() {
            throw new UnsupportedOperationException("not used");
        }
    }
}
