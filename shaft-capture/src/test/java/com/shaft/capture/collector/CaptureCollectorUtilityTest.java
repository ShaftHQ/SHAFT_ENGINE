package com.shaft.capture.collector;

import com.shaft.capture.format.CaptureFormatException;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.WebDriver;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureCollectorUtilityTest {
    @Test
    void browserSignalParsesJsonAndGeneratedSignalsDefensively() {
        BrowserSignal parsed = BrowserSignal.fromJson("""
                {
                  "kind": "Input",
                  "timestamp": "1700000000000",
                  "page": {"url": "https://example.test", "width": 1440},
                  "target": {"id": "username"},
                  "data": {
                    "text": "alice",
                    "count": "7",
                    "checked": "true",
                    "keys": ["CONTROL", "", "A"]
                  }
                }
                """, "window-1");
        BrowserSignal generated = BrowserSignal.generated(
                "navigation",
                "window-2",
                Map.of("url", "https://example.test/home"),
                Map.of("action", "OPEN"));

        assertEquals("input", parsed.kind());
        assertEquals(Instant.ofEpochMilli(1_700_000_000_000L), parsed.timestamp());
        assertEquals("window-1", parsed.browsingContextId());
        assertEquals("alice", parsed.dataString("text"));
        assertEquals(7, parsed.dataInt("count", 0));
        assertTrue(parsed.dataBoolean("checked"));
        assertEquals(List.of("CONTROL", "A"), parsed.dataStrings("keys"));
        assertEquals("navigation", generated.kind());
        assertEquals("OPEN", generated.dataString("action"));
        assertThrows(CaptureFormatException.class, () -> BrowserSignal.fromJson("{", "window-1"));
    }

    @Test
    void browserEventScriptExposesBundledInstallAndDrainSnippets() throws Exception {
        String preload = BrowserEventScript.preloadFunction();

        assertTrue(preload.contains("shaft"));
        assertTrue(BrowserEventScript.fallbackInstallation().contains("return ("));
        assertTrue(BrowserEventScript.fallbackDrain().contains("__shaftCaptureQueue"));
        assertTrue(BrowserEventScript.preloadFunction(List.of("data-pw"))
                .contains("const testIdAttributes = [\"data-pw\"];"));
        assertTrue(BrowserEventScript.fallbackInstallation(List.of("data-\"quoted\\path"))
                .contains("\"data-\\\"quoted\\\\path\""));
        assertTrue(preload.contains("SHAFT Capture"));
        assertTrue(preload.contains("kind: \"control\""));
        assertTrue(preload.contains("kind: \"checkpoint\""));
        assertTrue(preload.contains("Edit captured action"));
        assertTrue(preload.contains("Pause recording"));
        assertTrue(preload.contains("Stop recording"));
        assertTrue(preload.contains("globalThis.top === globalThis"));

        Constructor<BrowserEventScript> constructor = BrowserEventScript.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        InvocationTargetException failure = assertThrows(InvocationTargetException.class, constructor::newInstance);
        assertInstanceOf(IllegalStateException.class, failure.getCause());
    }

    @Test
    void compositeCollectorStartsAndClosesInOrderAndCleansStartedCollectorsOnFailure() {
        List<String> calls = new ArrayList<>();
        RecordingCollector first = new RecordingCollector("first", calls, false);
        RecordingCollector second = new RecordingCollector("second", calls, false);
        CompositeBrowserEventCollector composite = new CompositeBrowserEventCollector(List.of(first, second));

        composite.start(signal -> { }, warning -> { });
        composite.close();

        assertEquals(List.of("start:first", "start:second", "close:second", "close:first"), calls);

        List<String> failureCalls = new ArrayList<>();
        RecordingCollector started = new RecordingCollector("started", failureCalls, false);
        RecordingCollector failing = new RecordingCollector("failing", failureCalls, true);
        CompositeBrowserEventCollector failingComposite = new CompositeBrowserEventCollector(List.of(started, failing));

        assertThrows(IllegalStateException.class, () -> failingComposite.start(signal -> { }, warning -> { }));
        assertEquals(List.of("start:started", "start:failing", "close:started"), failureCalls);
        assertThrows(IllegalArgumentException.class, () -> new CompositeBrowserEventCollector(List.of()));
    }

    @Test
    void protocolCollectorsValidateDriversAndAcceptCustomTestIdAttributes() {
        WebDriver driver = noOpDriver();

        assertThrows(IllegalArgumentException.class, () -> new BidiBrowserEventCollector(null));
        assertThrows(IllegalArgumentException.class,
                () -> new PollingBrowserEventCollector(null, true, List.of("data-pw")));
        assertInstanceOf(BidiBrowserEventCollector.class,
                new BidiBrowserEventCollector(driver, List.of("data-pw")));
        assertInstanceOf(PollingBrowserEventCollector.class,
                new PollingBrowserEventCollector(driver, false, List.of("data-pw")));
    }

    private static WebDriver noOpDriver() {
        return (WebDriver) Proxy.newProxyInstance(
                WebDriver.class.getClassLoader(),
                new Class<?>[] {WebDriver.class},
                (proxy, method, args) -> defaultValue(method.getReturnType()));
    }

    private static Object defaultValue(Class<?> type) {
        if (type == boolean.class) {
            return false;
        }
        if (type == int.class || type == long.class || type == short.class || type == byte.class) {
            return 0;
        }
        if (type == float.class || type == double.class) {
            return 0.0;
        }
        return null;
    }

    private record RecordingCollector(String name, List<String> calls, boolean failOnStart)
            implements BrowserEventCollector {
        @Override
        public void start(
                java.util.function.Consumer<BrowserSignal> signalConsumer,
                java.util.function.Consumer<String> warningConsumer) {
            calls.add("start:" + name);
            signalConsumer.accept(BrowserSignal.generated("test", "context", Map.of(), Map.of()));
            if (failOnStart) {
                throw new IllegalStateException("start failed");
            }
        }

        @Override
        public void close() {
            calls.add("close:" + name);
        }
    }
}
