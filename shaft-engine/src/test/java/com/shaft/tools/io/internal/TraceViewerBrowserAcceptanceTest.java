package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.shaft.driver.SHAFT;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.testng.Assert;
import org.testng.annotations.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;

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
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test(groups = "trace-viewer-browser-acceptance")
    public void generatedViewerShouldRemainOfflineAndShareNavigableRangeState() throws Exception {
        Path chrome = chromeExecutable();
        ViewerFixture fixture = generateViewerFixture();
        Path html = fixture.html();
        try (ZipFile zip = new ZipFile(fixture.archive().toFile())) {
            Assert.assertEquals(readZipEntry(zip, "trace-viewer-native.zip"), "bounded native trace fixture");
            JsonNode traceJson = JSON.readTree(readZipEntry(zip, "shaft-trace.json"));
            JsonNode schemaArtifacts = traceJson.path("session").path("artifacts");
            JsonNode indexArtifacts = JSON.readTree(Files.readString(fixture.index())).path("artifacts");
            Assert.assertEquals(indexArtifacts, schemaArtifacts,
                    "Available artifact references must remain identical in the schema and canonical index.");
            for (JsonNode attachment : traceJson.path("attachments")) {
                Assert.assertFalse(attachment.asText().contains(fixture.nativeTrace().toString()),
                        "The trace schema must not expose the native trace's host filesystem path.");
            }
            Assert.assertFalse(traceJson.path("attachments").toString().contains("Playwright Trace (raw)"),
                    "The artifact graph is the sole owner of native trace handoff metadata.");
        }
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
            page.locator("#network-sort-status button").click();
            Assert.assertEquals(page.locator("#network-rows tr td:nth-child(4)").allTextContents(),
                    List.of("200", "503"));
            page.locator("#network-sort-status button").click();
            Assert.assertEquals(page.locator("#network-rows tr td:nth-child(4)").allTextContents(),
                    List.of("503", "200"));
            page.locator("#network-sort-duration button").click();
            Assert.assertEquals(page.locator("#network-rows tr td:nth-child(5)").allTextContents(),
                    List.of("45ms", "200ms"));
            page.evaluate("""
                    () => {
                      network.push({type:'<img id="network-type-injection"> WebSocket', method:'GET',
                        url:'ws://example.test/socket', status:101,
                        requestSizeBytes:0, responseSizeBytes:0, durationMs:5, timestamp:traceEnd});
                      renderNetwork();
                    }
                    """);
            page.locator("#network-sort-type button").click();
            Assert.assertEquals(page.locator("#network-rows tr td:nth-child(2)").allTextContents(),
                    List.of("<img id=\"network-type-injection\"> WebSocket", "HTTP", "HTTP"));
            Assert.assertEquals(page.locator("#network-type-injection").count(), 0,
                    "Network type must render hostile text without creating markup.");
            page.evaluate("() => { network.pop(); renderNetwork(); }");

            page.locator("#network-method-filter").selectOption("POST");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "1 network exchange");
            Assert.assertEquals(page.locator("#network-rows tr td").nth(2).textContent(), "POST");
            page.locator("#network-method-filter").selectOption("");
            page.locator("#network-status-filter").selectOption("503");
            Assert.assertEquals(page.locator("#network-rows tr").count(), 1);
            Assert.assertEquals(page.locator("#network-rows tr td").nth(3).textContent(), "503");
            page.locator("#network-status-filter").selectOption("");
            page.locator("#network-method-filter").selectOption("GET");
            page.locator("#network-status-filter").selectOption("503");
            page.locator("#network-text-filter").fill("retry later");
            Assert.assertEquals(page.locator("#network-rows tr").count(), 1);
            page.locator("#network-method-filter").selectOption("POST");
            Assert.assertEquals(page.locator("#network-rows tr").count(), 0,
                    "Method, status, text, and range predicates compose with AND semantics.");
            page.locator("#network-method-filter").selectOption("GET");
            Assert.assertEquals(page.locator("#network-hint").textContent(),
                    "Use View request details to inspect headers and body preview.");
            Assert.assertEquals(page.locator("#network-rows tr button").textContent(), "View request details");
            page.locator("#network-rows tr button").focus();
            page.locator("#network-rows tr button").press("Enter");
            Assert.assertTrue(page.locator("#network-detail").textContent().contains("request-value"));
            Assert.assertTrue(page.locator("#network-detail").textContent().contains("retry later"));
            @SuppressWarnings("unchecked")
            Map<String, Object> networkDetail = (Map<String, Object>) page.evaluate(
                    "JSON.parse(document.getElementById('network-detail').textContent)");
            Assert.assertEquals(networkDetail.get("failureReason"), "upstream unavailable");
            Assert.assertEquals(((Number) networkDetail.get("requestSizeBytes")).intValue(), 5);
            Assert.assertEquals(((Number) networkDetail.get("responseSizeBytes")).intValue(), 12);
            Assert.assertTrue(String.valueOf(networkDetail.get("requestHeaders")).contains("network-injection"));
            Assert.assertTrue(String.valueOf(networkDetail.get("responseHeaders")).contains("response-value"));
            Assert.assertEquals(page.locator("#network-injection").count(), 0,
                    "Network detail must render hostile text without creating markup.");
            page.locator("#network-method-filter").selectOption("");
            page.locator("#network-status-filter").selectOption("");
            page.locator("#network-text-filter").fill("503");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "0 network exchanges",
                    "Network text search must not duplicate the dedicated status filter.");
            page.locator("#network-text-filter").fill("timestamp");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "0 network exchanges",
                    "Search matches advertised values, not JSON property names.");
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
            page.locator("#console-text-filter").fill("browser");
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "0 console messages",
                    "Console text search must not duplicate the dedicated source filter.");
            page.locator("#console-text-filter").fill("message");
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "0 console messages",
                    "Console search matches values, not JSON property names.");
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
            page.evaluate("""
                    () => {
                      network.push({method:'DELETE', url:'https://example.test/out-of-range', status:204,
                        requestSizeBytes:1, responseSizeBytes:0, durationMs:1, timestamp:traceEnd});
                      populateNetworkFilters();
                      renderNetwork();
                    }
                    """);
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "3 network exchanges");
            Assert.assertEquals(page.locator("#network-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("out-of-range")).count(), 0);
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
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "3 network exchanges");
            Assert.assertEquals(page.locator("#network-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("out-of-range")).count(), 0,
                    "A timed exchange outside the selected range must be excluded.");
            Assert.assertTrue(page.locator("#network-rows tr").first().getAttribute("class").contains("inwindow"));
            page.locator("#network-method-filter").selectOption("DELETE");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "0 network exchanges",
                    "A matching field filter must not bypass the selected time range.");
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
            page.locator("button[data-tab=network]").click();
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "1 network exchange");
            Assert.assertEquals(page.locator("#network-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("out-of-range")).count(), 1);
            page.locator("#network-method-filter").selectOption("");
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "4 network exchanges");
            page.locator("button[data-tab=console]").click();
            page.goBack();
            Assert.assertEquals(String.valueOf(page.locator("#range-label").evaluate("element => element.value")),
                    selectedRange);
            page.goForward();
            Assert.assertEquals(page.locator("#trace-filmstrip button.inwindow").count(), 3);
            page.evaluate("""
                    () => {
                      window.__networkBackup = network.splice(0);
                      window.__consoleBackup = consoleEvents.splice(0);
                      renderNetwork();
                      renderConsole();
                    }
                    """);
            page.locator("button[data-tab=network]").click();
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "0 network exchanges");
            Assert.assertEquals(page.locator("#network-hint").textContent(), "No network exchanges were recorded.");
            page.locator("button[data-tab=console]").click();
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "0 console messages");
            Assert.assertEquals(page.locator("#console-hint").textContent(), "No console messages were recorded.");
            page.evaluate("""
                    () => {
                      network.push(...window.__networkBackup);
                      consoleEvents.push(...window.__consoleBackup);
                      renderNetwork();
                      renderConsole();
                    }
                    """);
            page.evaluate("""
                    () => {
                      const addMobile = (id, category, name, offset, metadata = {}, locator = '<mobile>') =>
                        actions.push({id, backend:'APPIUM', category, name, status:'passed', locator,
                          startTime:new Date(baseTime + offset).toISOString(), durationMs:5, metadata});
                      addMobile('mobile-app', 'mobile/app', 'activate', 10, {result:'RUNNING_IN_FOREGROUND'});
                      addMobile('mobile-context', 'mobile/context', 'native', 20,
                        {contextBefore:'WEBVIEW_1', contextAfter:'NATIVE_APP'});
                      addMobile('mobile-device', 'mobile/device', 'orientation', 30, {result:'LANDSCAPE'});
                      addMobile('mobile-logs', 'mobile/logs', 'stop', 40);
                      addMobile('mobile-performance', 'mobile/performance', 'clear', 50, {clearedCount:'2'});
                      addMobile('mobile-recording', 'mobile/recording', 'stop-and-save', traceDuration,
                        {decodedBytes:'64'});
                      addMobile('mobile-evidence', 'mobile/evidence', 'capture', 60,
                        {artifactCount:'3', omissionCount:'1', note:'<img id=mobile-injection>'});
                      const failedEvidence = actions.find(action => action.id === 'mobile-evidence');
                      failedEvidence.status = 'failed';
                      failedEvidence.exception = {type:'java.lang.IllegalStateException', message:'capture failed'};
                      actions.push({id:'mobile-legacy', backend:'APPIUM', category:'mobile/custom',
                        name:'legacy-action', status:'passed', locator:'<legacy>', metadata:{}});
                    }
                    """);
            page.locator("button[data-tab=mobile]").click();
            Assert.assertEquals(page.locator("#mobile-category-filter button").allTextContents(),
                    List.of("All", "App", "Context", "Device", "Logs", "Performance", "Recording", "Evidence"));
            Assert.assertEquals(page.locator("#mobile-result-count").textContent(), "8 mobile actions");
            Assert.assertEquals(page.locator("#mobile-rows tr").last().locator("td").first().textContent(), "Unknown");
            Assert.assertEquals(page.locator("#mobile-injection").count(), 0,
                    "Mobile metadata must render hostile text without creating markup.");
            List<List<String>> mobileCategories = List.of(
                    List.of("mobile/app", "App", "activate"),
                    List.of("mobile/context", "Context", "native"),
                    List.of("mobile/device", "Device", "orientation"),
                    List.of("mobile/logs", "Logs", "stop"),
                    List.of("mobile/performance", "Performance", "clear"));
            for (List<String> category : mobileCategories) {
                page.locator("#mobile-category-filter button[data-mobile-category='" + category.get(0) + "']").click();
                Assert.assertEquals(page.locator("#mobile-result-count").textContent(), "1 mobile action");
                Assert.assertEquals(page.locator("#mobile-rows tr td").nth(1).textContent(), category.get(1));
                Assert.assertEquals(page.locator("#mobile-rows tr td").nth(2).textContent(), category.get(2));
            }
            page.locator("#mobile-category-filter button[data-mobile-category='mobile/recording']").click();
            Assert.assertEquals(page.locator("#mobile-result-count").textContent(), "1 mobile action");
            page.evaluate("""
                    () => {
                      const end = document.getElementById('range-end');
                      end.value = Math.max(0, traceDuration - 1);
                      end.dispatchEvent(new Event('input', {bubbles:true}));
                    }
                    """);
            Assert.assertEquals(page.locator("#mobile-result-count").textContent(), "0 mobile actions");
            Assert.assertEquals(page.locator("#mobile-hint").textContent(),
                    "No mobile actions match the selected range and category.");
            page.locator("#show-all-range").click();
            Assert.assertEquals(page.locator("#mobile-result-count").textContent(), "1 mobile action");
            page.locator("#mobile-category-filter button[data-mobile-category='mobile/evidence']").click();
            Assert.assertEquals(page.locator("#mobile-category-filter button[aria-pressed=true]").count(), 1);
            Assert.assertNotEquals(page.locator("#mobile-category-filter button[aria-pressed=true]")
                    .evaluate("button => getComputedStyle(button).boxShadow"), "none",
                    "The active mobile category needs a visible non-color selection cue.");
            Assert.assertNotEquals(page.locator("#mobile-category-filter button[aria-pressed=true]")
                            .evaluate("button => getComputedStyle(button).boxShadow"),
                    page.locator("#mobile-category-filter button[aria-pressed=false]").first()
                            .evaluate("button => getComputedStyle(button).boxShadow"),
                    "The active mobile category must remain visibly distinct from inactive categories.");
            Assert.assertTrue(page.locator("#mobile-rows tr").getAttribute("class").contains("failed"));
            Assert.assertEquals(page.locator("#mobile-rows tr td").nth(3).textContent(), "failed");
            page.locator("#mobile-rows tr button").press("Enter");
            Assert.assertEquals(page.locator("#details-title").textContent(), "Action: capture");
            Assert.assertTrue(page.url().contains("action-mobile-evidence"));
            Assert.assertTrue(page.locator("#mobile-detail").textContent().contains("artifactCount"));
            Assert.assertTrue(page.locator("#mobile-detail").textContent().contains("java.lang.IllegalStateException"));
            @SuppressWarnings("unchecked")
            Map<String, Object> mobileDetail = (Map<String, Object>) page.evaluate(
                    "JSON.parse(document.getElementById('mobile-detail').textContent)");
            Assert.assertEquals(mobileDetail.get("status"), "failed");
            page.evaluate("""
                    () => {
                      const removed = actions.splice(0);
                      window.__mobileBackup = removed.filter(action =>
                        String(action.category || '').startsWith('mobile/'));
                      actions.push(...removed.filter(action =>
                        !String(action.category || '').startsWith('mobile/')));
                      renderMobile();
                    }
                    """);
            Assert.assertEquals(page.locator("#mobile-result-count").textContent(), "0 mobile actions");
            Assert.assertEquals(page.locator("#mobile-hint").textContent(), "No mobile actions were recorded.");
            page.evaluate("() => { actions.push(...window.__mobileBackup); renderMobile(); }");
            page.locator("button[data-tab=artifacts]").focus();
            page.locator("button[data-tab=artifacts]").press("Enter");
            Assert.assertEquals(page.locator("button[data-tab=artifacts]")
                    .evaluate("button => button === document.activeElement"), true);
            Assert.assertEquals(page.locator("#artifact-result-count").textContent(), "4 trace artifacts");
            Assert.assertEquals(page.locator("#artifact-rows tr").count(), 4);
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(1)").allTextContents(),
                    List.of("shaft-network.har", "screenshots/action-1.png", "screenshots/action-2.png",
                            "trace-viewer-native.zip"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(2)").allTextContents(),
                    List.of("network", "screenshot", "screenshot", "native-trace"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(3)").allTextContents(),
                    List.of("application/json", "image/png", "image/png", "application/zip"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(4)").allTextContents(),
                    List.of("Available", "Available", "Available", "Available"));
            Assert.assertTrue(page.locator("#native-trace-handoff").textContent().contains("show-trace"));
            Assert.assertTrue(page.locator("#native-trace-handoff").textContent().contains("trace-viewer-native.zip"));
            page.evaluate("""
                    () => {
                      const nativeTrace = artifacts.find(artifact => artifact.kind === 'native-trace');
                      window.__nativeArtifact = structuredClone(nativeTrace);
                      nativeTrace.omitted = true;
                      nativeTrace.metadata = {omissionReason:'Omitted because SHAFT could not read the native Playwright trace.'};
                      truncation.push(nativeTrace.path);
                      renderArtifacts();
                      renderSummary();
                    }
                    """);
            Assert.assertEquals(page.locator("#artifact-rows tr").last().locator("td").nth(3).textContent(), "Omitted");
            Assert.assertTrue(page.locator("#truncation-detail").textContent().contains("could not read"));
            Assert.assertEquals(page.locator("#native-trace-handoff").textContent(),
                    "Native Playwright trace omitted: Omitted because SHAFT could not read the native Playwright trace.");
            page.evaluate("""
                    () => {
                      const nativeTrace = artifacts.find(artifact => artifact.kind === 'native-trace');
                      nativeTrace.omitted = true;
                      nativeTrace.path = '<img id="artifact-path-injection">.zip';
                      nativeTrace.kind = '<img id="artifact-injection"> native-trace';
                      nativeTrace.mimeType = '<img id="artifact-mime-injection">';
                      nativeTrace.metadata = {omissionReason:'<img id="artifact-reason-injection">'};
                      truncation.pop();
                      renderArtifacts();
                      renderSummary();
                    }
                    """);
            Assert.assertTrue(page.locator("#truncation-banner").isHidden());
            Assert.assertEquals(page.locator("#truncation-detail").textContent(), "");
            Assert.assertEquals(page.locator("#artifact-injection").count(), 0);
            Assert.assertEquals(page.locator("#artifact-path-injection").count(), 0);
            Assert.assertEquals(page.locator("#artifact-mime-injection").count(), 0);
            Assert.assertEquals(page.locator("#artifact-reason-injection").count(), 0);
            Assert.assertTrue(page.locator("#artifact-rows tr").last().textContent().contains("artifact-injection"));
            Assert.assertTrue(page.locator("#artifact-rows tr").last().textContent()
                    .contains("artifact-reason-injection"));
            page.evaluate("() => { window.__artifactBackup = artifacts.splice(0); renderArtifacts(); }");
            Assert.assertEquals(page.locator("#artifact-result-count").textContent(), "0 trace artifacts");
            Assert.assertEquals(page.locator("#artifact-hint").textContent(),
                    "No artifact graph was recorded for this trace.");
            Assert.assertTrue(page.locator("#native-trace-handoff").isHidden());
            Assert.assertEquals(page.locator("#native-trace-handoff").textContent(), "");
            page.evaluate("""
                    () => {
                      artifacts.push(...window.__artifactBackup);
                      artifacts[artifacts.length - 1] = window.__nativeArtifact;
                      renderArtifacts();
                      renderSummary();
                    }
                    """);
            Assert.assertTrue(page.locator("#native-trace-handoff").textContent().contains("is available"));
            Assert.assertTrue(page.locator("#truncation-banner").isHidden());
            page.screenshot(new Page.ScreenshotOptions().setPath(screenshot).setFullPage(true));
            page.navigate(fixture.legacyHtml().toUri().toString());
            page.locator("button[data-tab=artifacts]").click();
            Assert.assertEquals(page.locator("#artifact-result-count").textContent(), "0 trace artifacts");
            Assert.assertEquals(page.locator("#artifact-hint").textContent(),
                    "No artifact graph was recorded for this trace.");
            Assert.assertEquals(page.locator("button[data-tab=timeline]").count(), 1,
                    "A session-less v1 trace must retain the legacy viewer panels.");
            page.locator("button[data-tab=timeline]").click();
            Assert.assertTrue(page.locator("#timeline-list .timeline-entry").count() > 0,
                    "The session-less v1 trace must retain usable legacy timeline content.");
            Assert.assertTrue(page.locator("#timeline-list").textContent().contains("CLICK"));
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
        Path nativeTrace = Path.of("target", "trace-viewer-native.zip").toAbsolutePath().normalize();
        Files.createDirectories(nativeTrace.getParent());
        Files.writeString(nativeTrace, "bounded native trace fixture");
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(nativeTrace);
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
                    "GET", "https://example.test/orders", 503,
                    Map.of("x-request", "request-value <img id=network-injection>"),
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
            Path legacyHtml = legacyViewer(html);
            return new ViewerFixture(html, consoleBaseTime, archive,
                    FailureTraceReporter.traceDirectory(info).resolve("index.json"), nativeTrace, legacyHtml);
        } finally {
            TraceEventRecorder.clear();
            BrowserObservabilityRecorder.clear();
            Files.deleteIfExists(nativeTrace);
            Properties.clearForCurrentThread();
        }
    }

    private static String snapshot(String label) {
        return "<html><body><main>" + label + "</main><img src=\"" + BLOCKED_RESOURCE + "\"></body></html>";
    }

    private record ViewerFixture(Path html, long consoleBaseTime, Path archive, Path index, Path nativeTrace,
                                 Path legacyHtml) {
    }

    private static Path legacyViewer(Path currentHtml) throws Exception {
        String html = Files.readString(currentHtml);
        String marker = "<pre hidden id=\"trace-data\">";
        int payloadStart = html.indexOf(marker) + marker.length();
        int payloadEnd = html.indexOf("</pre>", payloadStart);
        String encoded = html.substring(payloadStart, payloadEnd);
        String decoded = encoded.replace("&lt;", "<").replace("&gt;", ">").replace("&amp;", "&");
        JsonNode legacy = JSON.readTree(decoded);
        ((tools.jackson.databind.node.ObjectNode) legacy).remove("session");
        String legacyJson = JSON.writeValueAsString(legacy)
                .replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
        Path target = currentHtml.resolveSibling("trace-viewer-browser-acceptance-v1.html");
        Files.writeString(target, html.substring(0, payloadStart) + legacyJson + html.substring(payloadEnd));
        return target;
    }

    private static String readZipEntry(ZipFile zip, String entryName) throws IOException {
        ZipEntry entry = zip.getEntry(entryName);
        if (entry == null) {
            throw new IOException("Trace archive is missing " + entryName);
        }
        try (InputStream input = zip.getInputStream(entry)) {
            return new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
        }
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
        Files.deleteIfExists(Path.of("target", "trace-viewer-browser-acceptance-v1.html"));
    }

    @SuppressWarnings("unused")
    private static void marker() {
        // Reflection-only fixture marker used as the synthetic TestNG source method.
    }
}
