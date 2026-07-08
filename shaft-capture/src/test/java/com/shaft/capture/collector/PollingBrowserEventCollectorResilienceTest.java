package com.shaft.capture.collector;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.time.Duration;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The polling collector runs on a scheduled executor, where an uncaught exception silently
 * cancels all future polls: one unexpected runtime failure used to stop event collection for the
 * rest of the session while the browser stayed interactive (issue #3365).
 */
class PollingBrowserEventCollectorResilienceTest {
    @Test
    void pollingSurvivesUnexpectedRuntimeFailures() throws Exception {
        AtomicInteger polls = new AtomicInteger();
        List<String> warnings = new CopyOnWriteArrayList<>();
        WebDriver driver = new ThrowingDriver(polls);
        PollingBrowserEventCollector collector =
                new PollingBrowserEventCollector(driver, false, List.of());
        try {
            collector.start(signal -> {
            }, warnings::add);
            waitFor(() -> polls.get() >= 3, Duration.ofSeconds(5));
        } finally {
            collector.close();
        }

        assertTrue(polls.get() >= 3,
                "Polling must keep running after an unexpected runtime failure in one cycle.");
        assertEquals(List.of("The compatibility listener skipped one polling cycle."), warnings,
                "The unexpected failure must be reported exactly once.");
    }

    private static void waitFor(java.util.function.BooleanSupplier condition, Duration timeout)
            throws InterruptedException {
        long deadline = System.nanoTime() + timeout.toNanos();
        while (!condition.getAsBoolean() && System.nanoTime() < deadline) {
            Thread.sleep(50);
        }
        assertTrue(condition.getAsBoolean());
    }

    /**
     * Minimal driver stub whose every command fails with a plain RuntimeException — the class of
     * failure the collector's WebDriverException handling never covered.
     */
    private static final class ThrowingDriver implements WebDriver, JavascriptExecutor {
        private final AtomicInteger polls;

        private ThrowingDriver(AtomicInteger polls) {
            this.polls = polls;
        }

        @Override
        public Set<String> getWindowHandles() {
            polls.incrementAndGet();
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public Object executeScript(String script, Object... args) {
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public Object executeAsyncScript(String script, Object... args) {
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public void get(String url) {
        }

        @Override
        public String getCurrentUrl() {
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public String getTitle() {
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public List<WebElement> findElements(By by) {
            return List.of();
        }

        @Override
        public WebElement findElement(By by) {
            throw new IllegalStateException("unexpected driver failure");
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
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public TargetLocator switchTo() {
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public Navigation navigate() {
            throw new IllegalStateException("unexpected driver failure");
        }

        @Override
        public Options manage() {
            throw new IllegalStateException("unexpected driver failure");
        }
    }
}
