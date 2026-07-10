package com.shaft.capture.runtime;

import com.shaft.capture.collector.BrowserSignal;
import com.shaft.capture.model.BrowserMetadata;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.privacy.CapturePrivacyPolicy;
import com.shaft.capture.storage.CaptureSessionStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.openqa.selenium.MutableCapabilities;

import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ManagedCaptureRecorderControlTest {
    @TempDir
    Path temp;

    @Test
    void browserUiSignalsPauseCheckpointResumeAndStopWithoutLaunchingBrowser() throws Exception {
        CaptureStartRequest request = request(CaptureBrowser.CHROME, CaptureStartOptions.defaults());
        ManagedCaptureRecorder recorder = new ManagedCaptureRecorder(request);
        CaptureSessionStore store = new CaptureSessionStore(request.outputPath());
        store.start(CaptureSession.start(
                "ui-session",
                Instant.parse("2026-01-02T03:04:05Z"),
                new BrowserMetadata("chrome", "149", "test", "browser", Map.of())));
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store,
                request.outputPath(),
                CapturePrivacyPolicy.defaults(),
                ignored -> { },
                ignored -> { });
        recorder.activeSessionForTesting(store, pipeline);

        recorder.acceptSignal(BrowserSignal.generated(
                "checkpoint",
                "window-1",
                Map.of("url", "https://example.test", "title", "Example"),
                Map.of("description", "Edited captured action 1: Click submit", "kind", "USER_MARKER")));
        recorder.acceptSignal(BrowserSignal.generated("control", "window-1", Map.of(), Map.of("action", "PAUSE")));
        recorder.acceptSignal(BrowserSignal.generated(
                "navigation",
                "window-1",
                Map.of("url", "https://example.test/paused", "title", "Paused"),
                Map.of("action", "OPEN")));

        assertEquals(0, recorder.status().eventCount());
        assertEquals(1, store.read().checkpoints().size());
        assertEquals("Edited captured action 1: Click submit",
                store.read().checkpoints().getFirst().description());

        recorder.acceptSignal(BrowserSignal.generated("control", "window-1", Map.of(), Map.of("action", "RESUME")));
        recorder.acceptSignal(BrowserSignal.generated(
                "navigation",
                "window-1",
                Map.of("url", "https://example.test/resumed", "title", "Resumed"),
                Map.of("action", "OPEN")));
        recorder.acceptSignal(BrowserSignal.generated("control", "window-1", Map.of(), Map.of("action", "STOP")));

        // The UI STOP control now completes on its own dedicated thread (never on the delivering
        // collector thread, whose own shutdown would interrupt the teardown mid-way), so the
        // final state is awaited rather than asserted synchronously.
        CaptureStatus status = awaitState(recorder, CaptureStatus.State.COMPLETED);
        assertEquals(CaptureStatus.State.COMPLETED, status.state());
        assertEquals(1, status.eventCount());
        assertEquals(CaptureSession.SessionStatus.COMPLETED, store.read().status());
        assertTrue(store.read().events().getFirst() instanceof CaptureEvent.NavigationEvent);
    }

    @Test
    void stopCompletesOnAnAlreadyInterruptedThreadAndRestoresTheInterruptFlag() throws Exception {
        // Regression for issue #3409: the overlay Stop button delivered its STOP control on a
        // collector-owned thread that stop() itself interrupts while closing that collector.
        // NIO session-store writes then failed with ClosedByInterruptException and
        // WebDriver.quit() aborted, leaving the session INCOMPLETE and the browser orphaned.
        CaptureStartRequest request = request(CaptureBrowser.CHROME, CaptureStartOptions.defaults());
        ManagedCaptureRecorder recorder = new ManagedCaptureRecorder(request);
        CaptureSessionStore store = new CaptureSessionStore(request.outputPath());
        store.start(CaptureSession.start(
                "interrupted-stop-session",
                Instant.parse("2026-01-02T03:04:05Z"),
                new BrowserMetadata("chrome", "149", "test", "browser", Map.of())));
        CaptureEventPipeline pipeline = new CaptureEventPipeline(
                store,
                request.outputPath(),
                CapturePrivacyPolicy.defaults(),
                ignored -> { },
                ignored -> { });
        recorder.activeSessionForTesting(store, pipeline);

        Thread.currentThread().interrupt();
        CaptureStatus status;
        try {
            status = recorder.stop(false);
        } finally {
            // The flag must have been restored for the caller; consume it so JUnit's own
            // machinery is not affected by a lingering interrupt.
            assertTrue(Thread.interrupted(), "stop() must restore the caller's interrupt flag");
        }

        assertEquals(CaptureStatus.State.COMPLETED, status.state());
        assertEquals(CaptureSession.SessionStatus.COMPLETED, store.read().status());
    }

    private static CaptureStatus awaitState(ManagedCaptureRecorder recorder, CaptureStatus.State expected)
            throws InterruptedException {
        long deadline = System.currentTimeMillis() + 5_000;
        CaptureStatus status = recorder.status();
        while (status.state() != expected && System.currentTimeMillis() < deadline) {
            Thread.sleep(25);
            status = recorder.status();
        }
        return status;
    }

    @Test
    void browserOptionsCarryCodegenSessionMetadataWithoutLaunchingBrowser() throws Exception {
        CaptureStartOptions options = new CaptureStartOptions(
                "java",
                "data-pw",
                "chrome-beta",
                "Desktop",
                "1024,768",
                "",
                "",
                true,
                false,
                "",
                "",
                "en-GB",
                "",
                "http://proxy.example:8080",
                "localhost",
                "",
                "",
                Duration.ofSeconds(2),
                "agent-user-agent",
                temp.resolve("profile"));

        MutableCapabilities chrome = new ManagedCaptureRecorder(request(CaptureBrowser.CHROME, options)).browserOptions();
        MutableCapabilities edge = new ManagedCaptureRecorder(request(CaptureBrowser.EDGE, options)).browserOptions();

        assertEquals(true, chrome.getCapability("acceptInsecureCerts"));
        assertEquals(true, edge.getCapability("acceptInsecureCerts"));
        assertArguments(chrome.getCapability("goog:chromeOptions"));
        assertArguments(edge.getCapability("ms:edgeOptions"));
        assertNotNull(chrome.getCapability("unhandledPromptBehavior"));
        assertNotNull(edge.getCapability("unhandledPromptBehavior"));
    }

    @Test
    void browserOptionsApplyDevicePresetAsChromiumMobileEmulationWithoutLaunchingBrowser() {
        CaptureStartOptions options = new CaptureStartOptions(
                "",
                "",
                "",
                "Pixel 7",
                "",
                "",
                "",
                false,
                false,
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                "",
                Duration.ZERO,
                "",
                temp.resolve("profile"));

        MutableCapabilities chrome = new ManagedCaptureRecorder(request(CaptureBrowser.CHROME, options)).browserOptions();
        MutableCapabilities edge = new ManagedCaptureRecorder(request(CaptureBrowser.EDGE, options)).browserOptions();

        assertMobileEmulation(chrome.getCapability("goog:chromeOptions"));
        assertMobileEmulation(edge.getCapability("ms:edgeOptions"));
    }

    private CaptureStartRequest request(CaptureBrowser browser, CaptureStartOptions options) {
        return new CaptureStartRequest(
                "https://example.test",
                browser,
                temp.resolve(browser.name().toLowerCase()).resolve("capture.json"),
                temp.resolve(browser.name().toLowerCase()).resolve("runtime"),
                true,
                options);
    }

    @SuppressWarnings("unchecked")
    private static void assertArguments(Object capability) {
        assertTrue(capability instanceof Map<?, ?>);
        List<String> arguments = (List<String>) ((Map<?, ?>) capability).get("args");
        assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith("--user-data-dir=")));
        assertTrue(arguments.contains("--profile-directory=Default"));
        assertTrue(arguments.contains("--user-agent=agent-user-agent"));
        assertTrue(arguments.contains("--lang=en-GB"));
        assertTrue(arguments.contains("--proxy-server=http://proxy.example:8080"));
        assertTrue(arguments.contains("--proxy-bypass-list=localhost"));
    }

    @SuppressWarnings("unchecked")
    private static void assertMobileEmulation(Object capability) {
        assertTrue(capability instanceof Map<?, ?>);
        Map<String, Object> mobileEmulation = (Map<String, Object>) ((Map<?, ?>) capability).get("mobileEmulation");
        Map<String, Object> deviceMetrics = (Map<String, Object>) mobileEmulation.get("deviceMetrics");
        assertEquals(412, deviceMetrics.get("width"));
        assertEquals(915, deviceMetrics.get("height"));
        assertEquals(2.625, deviceMetrics.get("pixelRatio"));
        assertEquals(true, deviceMetrics.get("touch"));
        assertEquals(true, deviceMetrics.get("mobile"));
        assertTrue(String.valueOf(mobileEmulation.get("userAgent")).contains("Pixel 7"));
    }
}
