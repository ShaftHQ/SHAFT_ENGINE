package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.ObjectMapper;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

public class Rfc8259SiblingEscapeJsonTest {
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final String ESC = "\u001B[31mfailed";

    @Test(description = "TraceEventRecorder JSON must RFC 8259-escape ESC in action messages")
    public void traceEventRecorderMustEscapeEscInActionMessage() throws Exception {
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            TraceEventRecorder.Event event = TraceEventRecorder.start("element", "CLICK", By.id("pay"), null);
            TraceEventRecorder.finish(event, "failed", ESC, null, Map.of(), List.of());
            String json = TraceEventRecorderTestProbe.json();
            assertControlEscaped(json);
            JSON.readTree(json);
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "BrowserObservabilityRecorder JSON must RFC 8259-escape ESC in console messages")
    public void browserObservabilityRecorderMustEscapeEscInConsoleMessage() {
        try {
            BrowserObservabilityRecorder.recordConsole("browser", "ERROR", ESC, 1L);
            String json = BrowserObservabilityRecorder.drainConsoleJson();
            assertControlEscaped(json);
            JSON.readTree(json);
        } finally {
            BrowserObservabilityRecorder.clear();
        }
    }

    @Test(description = "FailureDiagnosticsReporter JSON must RFC 8259-escape ESC in log text")
    public void failureDiagnosticsReporterMustEscapeEscInLogText() throws Exception {
        try {
            SHAFT.Properties.reporting.set().diagnosticsBundleEnabled(true);
            String json = FailureDiagnosticsReporter.renderDiagnosticsJson(info(), "log " + ESC, List.of());
            assertControlEscaped(json);
            JSON.readTree(json);
        } finally {
            Properties.clearForCurrentThread();
        }
    }

    @Test(description = "FailureBriefReporter JSON must RFC 8259-escape ESC in log text")
    public void failureBriefReporterMustEscapeEscInLogText() throws Exception {
        String json = FailureBriefReporter.renderBriefJson(info(), "log " + ESC, List.of());
        assertControlEscaped(json);
        JSON.readTree(json);
    }

    private static void assertControlEscaped(String json) {
        for (int i = 0; i < json.length(); i++) {
            char c = json.charAt(i);
            if (c < 0x20 && c != '\n' && c != '\r' && c != '\t') {
                Assert.fail("Unescaped RFC 8259 control character U+" + String.format("%04X", (int) c)
                        + " at index " + i + " in " + json);
            }
        }
        Assert.assertTrue(json.contains("\\u001b") || json.contains("\\u001B"), json);
        Assert.assertFalse(json.contains("\u001B"), json);
    }

    private static TestExecutionInfo info() throws Exception {
        Method method = Rfc8259SiblingEscapeJsonTest.class.getDeclaredMethod("failingScenario");
        return new TestExecutionInfo("id-escapeJson", "customer.LoginTest", "failingScenario", "escape " + ESC,
                "escape json", method, new RuntimeException("boom"), false);
    }

    @SuppressWarnings("unused")
    private static void failingScenario() {
        throw new UnsupportedOperationException("test marker");
    }
}
