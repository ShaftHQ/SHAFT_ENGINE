package com.shaft.tools.io.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/** Real-browser proof that native Playwright traces converge into the portable SHAFT archive. */
public class PlaywrightTraceParityAcceptanceTest {
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test(groups = "playwright-trace-acceptance")
    public void nativeTraceShouldRemainPortableAcrossPlaywrightBrowserEngines() throws Exception {
        String browser = System.getProperty("shaft.trace.acceptance.browser", "chromium");
        configure(browser);
        TestExecutionInfo info = info();
        Path traceDirectory = FailureTraceReporter.traceDirectory(info);
        SHAFT.GUI.Playwright driver = null;
        try {
            driver = new SHAFT.GUI.Playwright();
            driver.getNativeDriver().setContent("""
                    <!doctype html><html><body>
                    <button id="change" onclick="this.textContent='after-action';console.log('trace parity log')">
                      before-action
                    </button>
                    </body></html>
                    """);
            driver.element().click(By.id("change"));
            Assert.assertEquals(driver.getNativeDriver().locator("#change").textContent(), "after-action");

            driver.getNativeDriver().setDefaultTimeout(500);
            try {
                driver.element().click(By.id("missing-trace-target"));
                Assert.fail("The missing target must exercise Playwright's native error record.");
            } catch (RuntimeException expected) {
                Assert.assertTrue(expected.getMessage().contains("missing-trace-target"), expected.getMessage());
            }

            FailureTraceReporter.attachOnFailure(info, "playwright trace parity log", List.of());
            assertArchive(traceDirectory.resolve("shaft-trace.zip"), browser);
        } finally {
            if (driver != null) driver.quit();
            PlaywrightTraceManager.clearLastTracePath();
            TraceEventRecorder.clear();
            BrowserObservabilityRecorder.clear();
            Properties.clearForCurrentThread();
            deleteDirectory(traceDirectory);
        }
    }

    private static void configure(String browser) {
        Properties.clearForCurrentThread();
        SHAFT.Properties.web.set().headlessExecution(true);
        SHAFT.Properties.playwright.set()
                .browserName(browser)
                .channel("")
                .connectionMode("local")
                .defaultTimeoutMilliseconds(3000)
                .navigationTimeoutMilliseconds(5000)
                .tracingEnabled(true)
                .tracingScreenshots(true)
                .tracingSnapshots(true)
                .tracingSources(true);
        SHAFT.Properties.reporting.set()
                .traceEnabled(true)
                .traceMode("failure")
                .traceIncludeFullPageSnapshots(true)
                .traceIncludeNativePageSource(true)
                .traceIncludeNetwork(true)
                .traceIncludeConsole(true);
    }

    private static void assertArchive(Path archive, String browser) throws Exception {
        Assert.assertTrue(Files.isRegularFile(archive), archive.toString());
        try (ZipFile zip = new ZipFile(archive.toFile())) {
            JsonNode root = JSON.readTree(read(zip, "shaft-trace.json"));
            JsonNode evidence = root.path("evidence");
            JsonNode playwright = evidence.path("playwright");
            Assert.assertEquals(playwright.path("status").asText(), "available", root.toPrettyString());
            Assert.assertTrue(playwright.path("actions").size() >= 2, playwright.toPrettyString());
            Assert.assertTrue(playwright.path("snapshots").size() >= 2, playwright.toPrettyString());
            Assert.assertTrue(allActionsHaveSourceState(playwright.path("actions")), playwright.toPrettyString());
            Assert.assertTrue(playwright.path("actions").toString().contains("sourceReason"),
                    playwright.toPrettyString());
            Assert.assertTrue(anyActionText(playwright.path("actions"), "error"), playwright.toPrettyString());
            Assert.assertTrue(anyActionArray(playwright.path("actions"), "logs"), playwright.toPrettyString());
            Assert.assertTrue(evidence.path("actions").toString().contains("playwrightCallId"),
                    evidence.path("actions").toPrettyString());
            Assert.assertEquals(root.path("snapshot").path("provider").asText(), "playwright");
            Assert.assertEquals(root.path("snapshot").path("fidelity").asText(), "structural");
            Assert.assertTrue(root.path("environment").path("browser").asText().isBlank()
                            || root.path("environment").path("browser").asText().equalsIgnoreCase(browser),
                    root.path("environment").toPrettyString());
            Assert.assertTrue(hasNativeTrace(root.path("session").path("artifacts")),
                    root.path("session").path("artifacts").toPrettyString());
            Assert.assertNotNull(zip.getEntry("SHAFT Trace Report.html"));
            Assert.assertNotNull(zip.getEntry("shaft-network.har"));
        }
    }

    private static boolean hasNativeTrace(JsonNode artifacts) {
        for (JsonNode artifact : artifacts) {
            if ("native-trace".equals(artifact.path("kind").asText()) && !artifact.path("omitted").asBoolean()) {
                return true;
            }
        }
        return false;
    }

    private static boolean allActionsHaveSourceState(JsonNode actions) {
        for (JsonNode action : actions) {
            String status = action.path("sourceStatus").asText();
            if ("available".equals(status) && action.path("source").asText().isBlank()) return false;
            if ("unavailable".equals(status) && action.path("sourceReason").asText().isBlank()) return false;
            if (!"available".equals(status) && !"unavailable".equals(status)) return false;
        }
        return !actions.isEmpty();
    }

    private static boolean anyActionText(JsonNode actions, String field) {
        for (JsonNode action : actions) {
            if (!action.path(field).asText().isBlank()) return true;
        }
        return false;
    }

    private static boolean anyActionArray(JsonNode actions, String field) {
        for (JsonNode action : actions) {
            if (!action.path(field).isEmpty()) return true;
        }
        return false;
    }

    private static String read(ZipFile zip, String path) throws IOException {
        ZipEntry entry = zip.getEntry(path);
        Assert.assertNotNull(entry, path);
        try (var input = zip.getInputStream(entry)) {
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static TestExecutionInfo info() throws Exception {
        Method marker = PlaywrightTraceParityAcceptanceTest.class.getDeclaredMethod(
                "nativeTraceShouldRemainPortableAcrossPlaywrightBrowserEngines");
        return new TestExecutionInfo("playwright-trace-parity", "customer.PlaywrightTraceParityTest",
                "nativeTrace", "nativeTrace", "Playwright trace parity", marker,
                new AssertionError("Playwright trace parity acceptance"), false);
    }

    private static void deleteDirectory(Path directory) throws IOException {
        if (!Files.exists(directory)) return;
        try (var paths = Files.walk(directory)) {
            for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) Files.deleteIfExists(path);
        }
    }
}
