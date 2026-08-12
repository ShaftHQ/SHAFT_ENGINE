package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.driver.SHAFT;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/** Explicit headless acceptance for the generated single-file trace viewer. */
public class TraceViewerBrowserAcceptanceTest {
    private static final String BLOCKED_RESOURCE = "https://blocked.invalid/private.png";

    @Test(groups = "trace-viewer-browser-acceptance")
    public void generatedViewerShouldRemainOfflineAndShareNavigableRangeState() throws Exception {
        Path chrome = chromeExecutable();
        ViewerFixture fixture = generateViewerFixture();
        Path html = fixture.html();
        Path screenshot = Path.of(System.getProperty("shaft.trace.viewer.screenshot",
                "target/trace-viewer-browser-acceptance.png")).toAbsolutePath().normalize();
        Files.createDirectories(screenshot.getParent());
        List<String> pageErrors = new ArrayList<>();
        List<String> externalRequests = new ArrayList<>();
        try (Playwright playwright = Playwright.create();
             Browser browser = playwright.chromium().launch(new BrowserType.LaunchOptions()
                     .setExecutablePath(chrome).setHeadless(true))) {
            Page page = browser.newPage(new Browser.NewPageOptions().setViewportSize(1440, 1000));
            page.onPageError(pageErrors::add);
            page.onRequest(request -> {
                String url = request.url();
                if (!url.startsWith("file:") && !url.startsWith("data:")) {
                    externalRequests.add(url);
                }
            });

            page.navigate(html.toUri() + "#action-action-1?start=50&end=200");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK"));
            Assert.assertEquals(page.locator("#range-start").inputValue(), "50");
            Assert.assertEquals(page.locator("#range-end").inputValue(), "200");

            page.locator("#show-all-range").click();
            page.locator("button[data-tab=network]").click();
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "2 network exchanges");
            Assert.assertEquals(page.locator("#network-method-filter option").count(), 3);
            Assert.assertEquals(page.locator("#network-status-filter option").count(), 3);
            Assert.assertEquals(page.locator("#network-sort-method").getAttribute("aria-sort"), "ascending");
            Assert.assertEquals(page.locator("#network-rows tr").count(), 2);
            Assert.assertEquals(page.locator("#network-rows tr").first().locator("td").nth(2).textContent(), "GET");
            Assert.assertEquals(page.locator("#network-rows tr").first().locator("td").nth(5).textContent(), "17 B");
            Assert.assertEquals(page.locator("#network-rows tr").nth(1).locator("td").nth(5).textContent(), "30 B");

            page.locator("#network-sort-size button").click();
            Assert.assertEquals(page.locator("#network-rows tr").first().locator("td").nth(5).textContent(), "17 B");
            Assert.assertEquals(page.locator("#network-panel th[aria-sort]").count(), 1);
            Assert.assertEquals(page.locator("#network-sort-size").getAttribute("aria-sort"), "ascending");
            page.locator("#network-sort-size button").press("Enter");
            Assert.assertEquals(page.locator("#network-rows tr").first().locator("td").nth(5).textContent(), "30 B");
            Assert.assertEquals(page.locator("#network-sort-size").getAttribute("aria-sort"), "descending");

            page.locator("#network-method-filter").selectOption("POST");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "1 network exchange");
            Assert.assertEquals(page.locator("#network-rows tr td").nth(2).textContent(), "POST");
            page.locator("#network-method-filter").selectOption("");
            page.locator("#network-status-filter").selectOption("503");
            Assert.assertEquals(page.locator("#network-rows tr").count(), 1);
            Assert.assertEquals(page.locator("#network-rows tr td").nth(3).textContent(), "503");
            page.locator("#network-status-filter").selectOption("");
            page.locator("#network-text-filter").fill("retry later");
            Assert.assertEquals(page.locator("#network-rows tr").count(), 1);
            Assert.assertEquals(page.locator("#network-hint").textContent(),
                    "Use View request details to inspect headers and body preview.");
            Assert.assertEquals(page.locator("#network-rows tr button").textContent(), "View request details");
            page.locator("#network-rows tr button").focus();
            page.locator("#network-rows tr button").press("Enter");
            Assert.assertTrue(page.locator("#network-detail").textContent().contains("request-value"));
            Assert.assertTrue(page.locator("#network-detail").textContent().contains("retry later"));
            page.locator("#network-text-filter").fill("no-such-exchange");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "0 network exchanges");
            Assert.assertTrue(page.locator("#network-hint").textContent().contains("match"));
            page.locator("#network-text-filter").fill("");
            page.evaluate("""
                    () => {
                      network.push({method:'PATCH', url:'legacy://untimed', status:0,
                        requestSizeBytes:4, failureReason:'legacy entry'});
                      renderNetwork();
                    }
                    """);
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "3 network exchanges");
            var legacyRow = page.locator("#network-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("legacy://untimed"));
            Assert.assertEquals(legacyRow.count(), 1, "Untimed legacy evidence must remain visible.");
            Assert.assertEquals(legacyRow.locator("td").nth(4).textContent(), "Unknown");
            Assert.assertEquals(legacyRow.locator("td").nth(5).textContent(), "Unknown");
            Assert.assertFalse(String.valueOf(legacyRow.getAttribute("class")).contains("inwindow"));
            page.locator("#network-sort-time button").click();
            page.locator("#network-sort-time button").click();
            Assert.assertEquals(page.locator("#network-rows tr").last().locator("td").nth(6).textContent(),
                    "legacy://untimed", "Missing sort values stay last in descending order.");

            page.locator("button[data-tab=console]").click();
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "3 console messages");
            Assert.assertEquals(page.locator("#console-source-filter option").count(), 4);
            Assert.assertEquals(page.locator("#console-level-filter option").count(), 3);
            Assert.assertEquals(page.locator("#console-sort-time").getAttribute("aria-sort"), "ascending");
            page.locator("#console-level-filter").selectOption("ERROR");
            Assert.assertEquals(page.locator("#console-rows tr").count(), 1);
            Assert.assertTrue(page.locator("#console-rows tr").textContent().contains("checkout failed"));
            Assert.assertEquals(page.locator("#console-hint").textContent(),
                    "Use View message details to inspect the structured message.");
            Assert.assertEquals(page.locator("#console-rows tr button").textContent(), "View message details");
            page.locator("#console-rows tr button").focus();
            page.locator("#console-rows tr button").press("Space");
            Assert.assertFalse(page.locator("#console-detail").isHidden());
            Assert.assertTrue(page.locator("#console-detail").textContent().contains("browser"));
            Assert.assertTrue(page.locator("#console-detail").textContent().contains("<img id=console-injection>"));
            Assert.assertEquals(page.locator("#console-injection").count(), 0,
                    "Console detail must render hostile text without creating markup.");
            @SuppressWarnings("unchecked")
            Map<String, Object> consoleDetail = (Map<String, Object>) page.evaluate(
                    "JSON.parse(document.getElementById('console-detail').textContent)");
            Assert.assertEquals(consoleDetail.get("source"), "browser");
            Assert.assertEquals(consoleDetail.get("level"), "ERROR");
            Assert.assertEquals(consoleDetail.get("message"), "<img id=console-injection> checkout failed");
            Assert.assertEquals(((Number) consoleDetail.get("timestamp")).longValue(), fixture.consoleBaseTime());
            page.locator("#console-level-filter").selectOption("");
            page.locator("#console-source-filter").selectOption("driver");
            Assert.assertEquals(page.locator("#console-rows tr").count(), 1);
            Assert.assertTrue(page.locator("#console-rows tr").textContent().contains("retry scheduled"));
            page.locator("#console-source-filter").selectOption("");
            page.locator("#console-text-filter").fill("no-such-message");
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "0 console messages");
            Assert.assertTrue(page.locator("#console-hint").textContent().contains("match"));
            page.locator("#console-text-filter").fill("");
            page.locator("#console-sort-level button").press("Enter");
            Assert.assertEquals(page.locator("#console-panel th[aria-sort]").count(), 1);
            Assert.assertEquals(page.locator("#console-sort-level").getAttribute("aria-sort"), "ascending");
            Assert.assertEquals(page.locator("#console-rows tr").first().locator("td").nth(2).textContent(),
                    "ERROR");
            page.locator("#console-sort-level button").click();
            Assert.assertEquals(page.locator("#console-sort-level").getAttribute("aria-sort"), "descending");
            Assert.assertEquals(page.locator("#console-rows tr td:nth-child(4)").allTextContents(),
                    List.of("retry scheduled", "alpha scheduled", "<img id=console-injection> checkout failed"));
            page.locator("#console-sort-source button").click();
            Assert.assertEquals(page.locator("#console-rows tr td:nth-child(2)").allTextContents(),
                    List.of("browser", "driver", "worker"));
            page.locator("#console-sort-message button").click();
            Assert.assertEquals(page.locator("#console-panel th[aria-sort]").count(), 1);
            Assert.assertEquals(page.locator("#console-rows tr td:nth-child(4)").allTextContents(),
                    List.of("<img id=console-injection> checkout failed", "alpha scheduled", "retry scheduled"));
            page.evaluate("""
                    () => {
                      consoleEvents.push({});
                      consoleSourceFilter.innerHTML = '<option value="">All sources</option>';
                      consoleLevelFilter.innerHTML = '<option value="">All levels</option>';
                      populateConsoleFilters();
                      renderConsole();
                    }
                    """);
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "4 console messages");
            page.locator("#console-source-filter").selectOption("Unknown");
            Assert.assertEquals(page.locator("#console-rows tr").count(), 1);
            page.locator("#console-source-filter").selectOption("");
            page.locator("#console-level-filter").selectOption("Unknown");
            Assert.assertEquals(page.locator("#console-rows tr").count(), 1);
            page.locator("#console-level-filter").selectOption("");
            var legacyConsoleRow = page.locator("#console-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("Unknown"));
            Assert.assertEquals(legacyConsoleRow.count(), 1);
            Assert.assertEquals(legacyConsoleRow.locator("td").allTextContents(),
                    List.of("Unknown", "Unknown", "Unknown", "Unknown", "View message details"));
            Assert.assertFalse(String.valueOf(legacyConsoleRow.getAttribute("class")).contains("inwindow"));
            page.locator("#console-sort-time button").click();
            page.locator("#console-sort-time button").click();
            Assert.assertEquals(page.locator("#console-rows tr td:nth-child(4)").allTextContents().subList(0, 3),
                    List.of("alpha scheduled", "<img id=console-injection> checkout failed", "retry scheduled"),
                    "Equal timestamps retain their original order when sorting descending.");
            Assert.assertEquals(page.locator("#console-rows tr").last().locator("td").allTextContents(),
                    List.of("Unknown", "Unknown", "Unknown", "Unknown", "View message details"));

            page.navigate(html.toUri() + "#action-action-1");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK"));
            Assert.assertNotEquals(page.locator("#range-start").inputValue(),
                    page.locator("#range-end").inputValue(), "A legacy action link must select its action interval.");
            Assert.assertEquals(page.locator("#trace-filmstrip button[role=option]").count(), 3);

            page.locator("#trace-filmstrip button").nth(1).click();
            Assert.assertTrue(page.locator("#details-title").textContent().contains("TEXT"));
            page.goBack();
            Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK"), page.url());
            page.goForward();
            Assert.assertTrue(page.locator("#details-title").textContent().contains("TEXT"), page.url());
            page.locator("#trace-filmstrip button").first().focus();
            page.locator("#trace-filmstrip button").first().press("ArrowRight");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("TEXT"));
            Assert.assertEquals(page.locator("#trace-filmstrip button:focus").getAttribute("data-action-id"),
                    "action-2");
            page.locator("#trace-filmstrip button:focus").press("ArrowRight");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("NO EVIDENCE"));
            Assert.assertEquals(page.locator("#trace-filmstrip button:focus").getAttribute("data-action-id"),
                    "action-3");
            page.locator("#trace-filmstrip button:focus").press("ArrowLeft");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("TEXT"));
            Assert.assertEquals(page.locator("#trace-filmstrip button:focus").getAttribute("data-action-id"),
                    "action-2");

            page.evaluate("location.hash = '#action-action-1?start=10'");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK"));
            Assert.assertNotEquals(page.locator("#range-start").inputValue(),
                    page.locator("#range-end").inputValue(), "A one-sided range must fall back to the action interval.");
            page.evaluate("location.hash = '#action-action-1?start=invalid&end=20'");
            Assert.assertNotEquals(page.locator("#range-start").inputValue(),
                    page.locator("#range-end").inputValue(), "A malformed range must fall back to the action interval.");
            page.evaluate("location.hash = '#action-action-1?start=200&end=50'");
            Assert.assertEquals(page.locator("#range-start").inputValue(), "50");
            Assert.assertEquals(page.locator("#range-end").inputValue(), "200");
            page.evaluate("location.hash = '#action-action-1?start=-10&end=999999'");
            Assert.assertEquals(page.locator("#range-start").inputValue(), "0");
            Assert.assertEquals(page.locator("#range-end").inputValue(),
                    page.locator("#range-end").getAttribute("max"));

            page.locator("#trace-filmstrip button").nth(2).click();
            page.locator("button[data-tab=comparison]").click();
            Assert.assertTrue(page.locator("#comparison-before-empty").isVisible());
            Assert.assertTrue(page.locator("#comparison-action-empty").isVisible());
            Assert.assertTrue(page.locator("#comparison-after-empty").isVisible());

            page.locator("#trace-filmstrip button").first().click();
            page.locator("button[data-tab=comparison]").click();
            Assert.assertTrue(page.frameLocator("#comparison-before").locator("body").textContent().contains("before one"));
            Assert.assertTrue(page.frameLocator("#comparison-after").locator("body").textContent().contains("after one"));
            Assert.assertNull(page.frameLocator("#comparison-before").locator("img").getAttribute("src"));
            Assert.assertNull(page.frameLocator("#comparison-after").locator("img").getAttribute("src"));
            Assert.assertTrue(page.locator("#comparison-action").isVisible());
            Assert.assertTrue(page.locator("#comparison-action").getAttribute("src")
                    .startsWith("data:image/png;base64,"));
            Assert.assertTrue((Boolean) page.locator("#comparison-action")
                    .evaluate("image => image.complete && image.naturalWidth > 0 && image.naturalHeight > 0"));

            page.locator("button[data-tab=timeline]").click();
            page.evaluate("""
                    () => {
                      const trace = JSON.parse(document.getElementById('trace-data').textContent);
                      const action = trace.actions[0];
                      const actionTimes = trace.actions.map(item => Date.parse(item.startTime));
                      const networkStart = trace.network[0].timestamp - trace.network[0].durationMs;
                      const base = Math.min(networkStart, ...actionTimes);
                      const actionStart = Date.parse(action.startTime) - base;
                      const start = actionStart + 10;
                      const end = actionStart + Math.max(11, action.durationMs - 10);
                      for (const [id, value] of [['range-start', start], ['range-end', end]]) {
                        const input = document.getElementById(id);
                        input.value = value;
                        input.dispatchEvent(new Event('input', {bubbles:true}));
                      }
                    }
                    """);
            Assert.assertTrue(page.locator("#trace-filmstrip button").first().getAttribute("class").contains("inwindow"));
            Assert.assertTrue(page.locator("#action-list button").first().getAttribute("class").contains("inwindow"));
            Assert.assertTrue(page.locator(".timeline-entry").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("CLICK")).first()
                    .getAttribute("class").contains("inwindow"));

            page.locator("button[data-tab=network]").click();
            int historyBeforeRangeInput = ((Number) page.evaluate("history.length")).intValue();
            page.evaluate("""
                    () => {
                      const trace = JSON.parse(document.getElementById('trace-data').textContent);
                      const event = trace.network[0];
                      const actionTimes = trace.actions.map(action => Date.parse(action.startTime));
                      const networkStart = event.timestamp - event.durationMs;
                      const base = Math.min(networkStart, ...actionTimes);
                      const start = Math.max(0, networkStart - base + 50);
                      const end = Math.max(start, event.timestamp - base - 10);
                      for (const [id, value] of [['range-start', start], ['range-end', end]]) {
                        const input = document.getElementById(id);
                        input.value = value;
                        input.dispatchEvent(new Event('input', {bubbles:true}));
                      }
                    }
                    """);
            Assert.assertEquals(((Number) page.evaluate("history.length")).intValue(), historyBeforeRangeInput,
                    "Transient range input must not create history entries.");
            page.locator("#range-end").dispatchEvent("change");
            Assert.assertEquals(((Number) page.evaluate("history.length")).intValue(), historyBeforeRangeInput + 1,
                    "A committed range change must create one history entry.");
            Assert.assertTrue(page.locator("#network-rows tr").first().getAttribute("class").contains("inwindow"));
            Assert.assertTrue(page.locator("#trace-filmstrip button").first().getAttribute("class").contains("inwindow"));
            page.locator("button[data-tab=timeline]").click();
            Assert.assertTrue(page.locator(".timeline-entry").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("POST")).first()
                    .getAttribute("class").contains("inwindow"));
            page.locator("button[data-tab=console]").click();
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "1 console message",
                    "Timed console messages remain point events; untimed legacy evidence remains visible.");
            Assert.assertEquals(page.locator("#console-rows tr td").allTextContents(),
                    List.of("Unknown", "Unknown", "Unknown", "Unknown", "View message details"));

            String selectedRange = String.valueOf(page.locator("#range-label").evaluate("element => element.value"));
            int historyBeforeShowAll = ((Number) page.evaluate("history.length")).intValue();
            page.locator("#show-all-range").click();
            Assert.assertEquals(((Number) page.evaluate("history.length")).intValue(), historyBeforeShowAll + 1);
            Assert.assertEquals(page.locator("#trace-filmstrip button.inwindow").count(), 3);
            page.goBack();
            Assert.assertEquals(String.valueOf(page.locator("#range-label").evaluate("element => element.value")),
                    selectedRange);
            page.goForward();
            Assert.assertEquals(page.locator("#trace-filmstrip button.inwindow").count(), 3);
            page.screenshot(new Page.ScreenshotOptions().setPath(screenshot).setFullPage(true));
            Assert.assertTrue(pageErrors.isEmpty(), "Page errors: " + pageErrors);
            Assert.assertTrue(externalRequests.isEmpty(), "External requests: " + externalRequests);
        } finally {
            deleteTraceFixture();
        }
        Assert.assertTrue(Files.size(screenshot) > 10_000, "Rendered screenshot should contain the populated viewer.");
    }

    private static ViewerFixture generateViewerFixture() throws Exception {
        WebDriver driver = Mockito.mock(WebDriver.class,
                Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        Mockito.when(driver.getCurrentUrl()).thenReturn("https://example.test/checkout");
        Mockito.when(((JavascriptExecutor) driver).executeScript(Mockito.anyString()))
                .thenReturn(snapshot("before one"), snapshot("after one"), snapshot("before two"), snapshot("after two"));
        byte[] png = Base64.getDecoder().decode(
                "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=");
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true).traceIncludeScreenshots(true)
                    .traceIncludeNetwork(true).traceIncludeConsole(true);
            var first = TraceEventRecorder.start("element", "CLICK", "#checkout", driver);
            TraceEventRecorder.recordScreenshot(first, png);
            Thread.sleep(80);
            TraceEventRecorder.finish(first, "passed", "clicked", null, Map.of(), List.of());
            var second = TraceEventRecorder.start("validation", "TEXT", "#confirmation", driver);
            TraceEventRecorder.recordScreenshot(second, png);
            TraceEventRecorder.finish(second, "failed", "mismatch", new AssertionError("expected receipt"),
                    Map.of("expected", "paid", "actual", "pending"), List.of());
            TraceEventRecorder.record("evidence", "NO EVIDENCE", "passed", "", null,
                    "optional evidence omitted", null, Map.of(), List.of());
            BrowserObservabilityRecorder.recordNetwork(new BrowserObservabilityRecorder.NetworkObservation(
                    "POST", "https://example.test/payment", 200, Map.of(), Map.of(),
                    200, 10, 20, "", "ok"));
            BrowserObservabilityRecorder.recordNetwork(new BrowserObservabilityRecorder.NetworkObservation(
                    "GET", "https://example.test/orders", 503, Map.of("x-request", "request-value"),
                    Map.of("x-response", "response-value"), 45, 5, 12, "upstream unavailable", "retry later"));
            long consoleBaseTime = System.currentTimeMillis();
            BrowserObservabilityRecorder.recordConsole("browser", "ERROR",
                    "<img id=console-injection> checkout failed",
                    consoleBaseTime);
            BrowserObservabilityRecorder.recordConsole("driver", "INFO", "retry scheduled",
                    consoleBaseTime);
            BrowserObservabilityRecorder.recordConsole("worker", "INFO", "alpha scheduled",
                    consoleBaseTime + 1);

            Method marker = TraceViewerBrowserAcceptanceTest.class.getDeclaredMethod("marker");
            TestExecutionInfo info = new TestExecutionInfo("trace-viewer-browser-acceptance", "customer.CheckoutTest",
                    "traceViewer", "traceViewer", "trace viewer acceptance", marker,
                    new AssertionError("checkout failed"), false);
            FailureTraceReporter.attachOnFailure(info, "trace viewer acceptance", List.of());
            Path archive = FailureTraceReporter.traceDirectory(info).resolve("shaft-trace.zip");
            Path html = Path.of("target", "trace-viewer-browser-acceptance.html").toAbsolutePath().normalize();
            extract(archive, "SHAFT Trace Report.html", html);
            return new ViewerFixture(html, consoleBaseTime);
        } finally {
            TraceEventRecorder.clear();
            BrowserObservabilityRecorder.clear();
            Properties.clearForCurrentThread();
        }
    }

    private static String snapshot(String label) {
        return "<html><body><main>" + label + "</main><img src=\"" + BLOCKED_RESOURCE + "\"></body></html>";
    }

    private record ViewerFixture(Path html, long consoleBaseTime) {
    }

    private static void extract(Path archive, String entryName, Path target) throws IOException {
        try (ZipFile zip = new ZipFile(archive.toFile())) {
            ZipEntry entry = zip.getEntry(entryName);
            if (entry == null) {
                throw new IOException("Trace archive is missing " + entryName);
            }
            try (InputStream input = zip.getInputStream(entry)) {
                Files.copy(input, target, StandardCopyOption.REPLACE_EXISTING);
            }
        }
    }

    private static Path chromeExecutable() {
        String configured = System.getProperty("shaft.trace.viewer.chrome", "");
        if (configured.isBlank()) {
            throw new IllegalStateException("Set -Dshaft.trace.viewer.chrome to a Chromium executable.");
        }
        Path chrome = Path.of(configured).toAbsolutePath().normalize();
        if (!Files.isRegularFile(chrome)) {
            throw new IllegalStateException("Chromium executable does not exist: " + chrome);
        }
        return chrome;
    }

    private static void deleteTraceFixture() throws IOException {
        Path root = Path.of("target", "shaft-traces", "trace-viewer-browser-acceptance");
        if (Files.exists(root)) {
            try (var paths = Files.walk(root)) {
                for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) {
                    Files.deleteIfExists(path);
                }
            }
        }
    }

    @SuppressWarnings("unused")
    private static void marker() {
        // Reflection-only fixture marker used as the synthetic TestNG source method.
    }
}
