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

import java.lang.reflect.Field;
import java.lang.reflect.Method;
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
        set(recorder, "store", store);
        set(recorder, "pipeline", pipeline);
        set(recorder, "state", CaptureStatus.State.ACTIVE);

        accept(recorder, BrowserSignal.generated(
                "checkpoint",
                "window-1",
                Map.of("url", "https://example.test", "title", "Example"),
                Map.of("description", "Edited captured action 1: Click submit", "kind", "USER_MARKER")));
        accept(recorder, BrowserSignal.generated("control", "window-1", Map.of(), Map.of("action", "PAUSE")));
        accept(recorder, BrowserSignal.generated(
                "navigation",
                "window-1",
                Map.of("url", "https://example.test/paused", "title", "Paused"),
                Map.of("action", "OPEN")));

        assertEquals(0, recorder.status().eventCount());
        assertEquals(1, store.read().checkpoints().size());
        assertEquals("Edited captured action 1: Click submit",
                store.read().checkpoints().getFirst().description());

        accept(recorder, BrowserSignal.generated("control", "window-1", Map.of(), Map.of("action", "RESUME")));
        accept(recorder, BrowserSignal.generated(
                "navigation",
                "window-1",
                Map.of("url", "https://example.test/resumed", "title", "Resumed"),
                Map.of("action", "OPEN")));
        accept(recorder, BrowserSignal.generated("control", "window-1", Map.of(), Map.of("action", "STOP")));

        CaptureStatus status = recorder.status();
        assertEquals(CaptureStatus.State.COMPLETED, status.state());
        assertEquals(1, status.eventCount());
        assertEquals(CaptureSession.SessionStatus.COMPLETED, store.read().status());
        assertTrue(store.read().events().getFirst() instanceof CaptureEvent.NavigationEvent);
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

        MutableCapabilities chrome = capabilities(new ManagedCaptureRecorder(request(CaptureBrowser.CHROME, options)));
        MutableCapabilities edge = capabilities(new ManagedCaptureRecorder(request(CaptureBrowser.EDGE, options)));

        assertEquals(true, chrome.getCapability("acceptInsecureCerts"));
        assertEquals(true, edge.getCapability("acceptInsecureCerts"));
        assertArguments(chrome.getCapability("goog:chromeOptions"));
        assertArguments(edge.getCapability("ms:edgeOptions"));
        assertNotNull(chrome.getCapability("unhandledPromptBehavior"));
        assertNotNull(edge.getCapability("unhandledPromptBehavior"));
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

    private static void accept(ManagedCaptureRecorder recorder, BrowserSignal signal) throws Exception {
        Method method = ManagedCaptureRecorder.class.getDeclaredMethod("acceptSignal", BrowserSignal.class);
        method.setAccessible(true);
        method.invoke(recorder, signal);
    }

    private static MutableCapabilities capabilities(ManagedCaptureRecorder recorder) throws Exception {
        Method method = ManagedCaptureRecorder.class.getDeclaredMethod("browserOptions");
        method.setAccessible(true);
        return (MutableCapabilities) method.invoke(recorder);
    }

    private static void set(Object target, String name, Object value) throws Exception {
        Field field = ManagedCaptureRecorder.class.getDeclaredField(name);
        field.setAccessible(true);
        field.set(target, value);
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
}
