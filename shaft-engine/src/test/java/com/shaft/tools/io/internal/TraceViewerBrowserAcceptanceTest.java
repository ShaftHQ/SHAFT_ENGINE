package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Page;
import com.microsoft.playwright.Playwright;
import com.microsoft.playwright.options.ColorScheme;
import com.microsoft.playwright.options.ReducedMotion;
import com.shaft.driver.SHAFT;
import com.shaft.gui.playwright.internal.PlaywrightTraceManager;
import com.shaft.listeners.internal.TestExecutionInfo;
import com.shaft.properties.internal.Properties;
import io.qameta.allure.Allure;
import io.qameta.allure.model.Attachment;
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
import java.util.zip.ZipOutputStream;

/** Explicit headless acceptance for the generated single-file trace viewer. */
public class TraceViewerBrowserAcceptanceTest {
    private static final String BLOCKED_RESOURCE = "https://blocked.invalid/private.png";
    private static final ObjectMapper JSON = new ObjectMapper();

    @Test(groups = "trace-viewer-browser-acceptance")
    public void realPlaywrightSnapshotRecordsShouldRenderWithOfflineResourcesAndInertScripts() throws Exception {
        Path archive = Files.createTempFile("playwright-snapshot-record", ".zip");
        Path html = Files.createTempFile("playwright-snapshot-record", ".html");
        try {
            try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(archive))) {
                zipEntry(output, "test.trace", "{\"version\":8,\"type\":\"context-options\",\"origin\":\"testRunner\"}\n");
                zipEntry(output, "0-trace.trace", """
                        {"version":8,"type":"context-options","origin":"library","contextId":"context@1"}
                        {"type":"frame-snapshot","snapshot":{"callId":"call@0","snapshotName":"child-before","pageId":"page@1","frameId":"child@1","frameUrl":"https://wrong.test/","html":["HTML",{},["BODY",{},["P",{},"wrong frame"]]],"viewport":{"width":200,"height":100},"timestamp":5,"wallTime":5,"collectionTime":1,"resourceOverrides":[],"isMainFrame":false}}
                        {"type":"frame-snapshot","snapshot":{"callId":"call@old","snapshotName":"old","pageId":"page@1","frameId":"frame@1","frameUrl":"https://example.test/app/","html":["HTML",{},["BODY",{},["IMG",{"id":"referenced-image","src":"pixel.png"}]]],"timestamp":8,"resourceOverrides":[],"isMainFrame":true}}
                        {"type":"frame-snapshot","snapshot":{"callId":"call@1","snapshotName":"before@call@1","pageId":"page@1","frameId":"frame@1","frameUrl":"https://example.test/app/","doctype":"html","html":["HTML",{},["HEAD",{},["META",{"http-equiv":"refresh","content":"0;url=https://blocked.invalid/refresh"}]],["BODY",{},["STYLE",{},"#snapshot-proof > span{color:rgb(1,2,3)}"],["MAIN",{"id":"snapshot-proof"},["SPAN",{},"offline snapshot rendered ✓ café العربية"]],["LINK",{"rel":"stylesheet","href":"site.css"}],["A",{"id":"snapshot-link","href":"captured.html"},"captured link"],["IMG",{"id":"snapshot-image","src":"pixel.png","srcset":"https://blocked.invalid/leak.png 2x"}],[[1,0]],["SCRIPT",{},"parent.__capturedScriptRan=true"]]],"viewport":{"width":800,"height":600},"timestamp":20,"wallTime":20,"collectionTime":1,"resourceOverrides":[],"isMainFrame":true}}
                        """);
                zipEntry(output, "0-trace.network", """
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":10,"request":{"method":"GET","url":"https://example.test/app/site.css"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"text/css","_sha1":"style.css"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":7,"request":{"method":"GET","url":"https://example.test/app/pixel.png"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"image/png","_sha1":"old-pixel.png"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":11,"request":{"method":"GET","url":"https://example.test/app/pixel.png"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"image/png","_sha1":"pixel.png"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":11.5,"request":{"method":"GET","url":"https://example.test/app/pixel.png?token=raw-url-secret"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"image/png","_sha1":"pixel.png"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":12,"request":{"method":"GET","url":"https://example.test/app/imported.css"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"text/css","_sha1":"imported.css"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":12.5,"request":{"method":"GET","url":"https://example.test/app/supported.css"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"text/css","_sha1":"supported.css"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":13,"request":{"method":"GET","url":"https://example.test/app/captured.html"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"text/html","_sha1":"captured.html"}}}}
                        {"type":"resource-snapshot","snapshot":{"_frameref":"frame@1","_monotonicTime":30,"request":{"method":"GET","url":"https://example.test/app/site.css"},"response":{"status":200,"statusText":"OK","headers":[],"content":{"mimeType":"text/css","_sha1":"future.css"}}}}
                        """);
                zipEntry(output, "resources/style.css", "@import url(imported.css) print;:root{--token:raw-css-secret;}"
                        + "@import \"supported.css\" supports(selector(:has(*)));"
                        + "#snapshot-proof{background-color:rgb(4,5,6);background-image:url(pixel.png?token=raw-url-secret)}"
                        + "</style><meta http-equiv=refresh content=\"0;url=https://blocked.invalid/css-refresh\">");
                zipEntry(output, "resources/imported.css",
                        "#snapshot-link{color:rgb(7,8,9);--api-key:raw-imported-secret}");
                zipEntry(output, "resources/supported.css", "#snapshot-link{background-color:rgb(10,11,12)}");
                zipEntry(output, "resources/captured.html", "<img src=https://blocked.invalid/navigated>");
                zipEntry(output, "resources/old-pixel.png", "not a png");
                zipEntry(output, "resources/future.css", "#snapshot-proof{display:none}");
                output.putNextEntry(new ZipEntry("resources/pixel.png"));
                output.write(Base64.getDecoder().decode(
                        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="));
                output.closeEntry();
            }
            PlaywrightTraceArchiveLoader.LoadedArchive loaded = PlaywrightTraceArchiveLoader.load(archive);
            String renderedDocument = PlaywrightTraceOfflineAdapter.snapshotDocument(loaded, "before@call@1");
            Assert.assertTrue(renderedDocument.contains("#snapshot-proof > span{color:rgb(1,2,3)}"),
                    renderedDocument);
            Assert.assertTrue(renderedDocument.contains("data:image/png;base64,"), renderedDocument);
            Assert.assertTrue(renderedDocument.contains("background-image:url(data:image/png;base64,"), renderedDocument);
            Assert.assertFalse(renderedDocument.toLowerCase().contains("</style><meta"), renderedDocument);
            Assert.assertFalse(renderedDocument.contains("raw-css-secret"), renderedDocument);
            Assert.assertFalse(renderedDocument.contains("raw-imported-secret"), renderedDocument);
            Assert.assertFalse(renderedDocument.contains("raw-url-secret"), renderedDocument);
            Assert.assertFalse(renderedDocument.contains("display:none"), renderedDocument);
            Assert.assertTrue(renderedDocument.contains("#snapshot-proof{background-color:rgb(4,5,6)"),
                    renderedDocument);
            Files.writeString(html, PlaywrightTraceOfflineAdapter.render(loaded, "before@call@1"));

            List<String> externalRequests = new ArrayList<>();
            try (Playwright playwright = Playwright.create();
                 Browser browser = playwright.chromium().launch(new BrowserType.LaunchOptions()
                         .setExecutablePath(chromeExecutable()).setHeadless(true))) {
                Page page = browser.newPage();
                page.onRequest(request -> {
                    if (!request.url().startsWith("file:") && !request.url().startsWith("data:"))
                        externalRequests.add(request.url());
                });
                page.navigate(html.toUri().toString());
                var frame = page.frameLocator("#playwright-snapshot");
                Assert.assertEquals(frame.locator("#snapshot-proof").textContent(),
                        "offline snapshot rendered ✓ café العربية");
                Assert.assertEquals(frame.locator("#snapshot-proof span").evaluate("e => getComputedStyle(e).color"),
                        "rgb(1, 2, 3)");
                Assert.assertEquals(frame.locator("#snapshot-proof")
                        .evaluate("e => getComputedStyle(e).backgroundColor"), "rgb(4, 5, 6)");
                Assert.assertTrue(((String) frame.locator("#snapshot-proof")
                        .evaluate("e => getComputedStyle(e).backgroundImage")).startsWith("url(\"data:image/png;base64,"));
                Assert.assertEquals(frame.locator("#snapshot-link").evaluate("e => getComputedStyle(e).color"),
                        "rgb(0, 0, 238)");
                Assert.assertEquals(frame.locator("#snapshot-link")
                        .evaluate("e => getComputedStyle(e).backgroundColor"), "rgb(10, 11, 12)");
                frame.locator("#snapshot-link").click();
                Assert.assertEquals(frame.locator("#snapshot-proof").textContent(),
                        "offline snapshot rendered ✓ café العربية");
                Assert.assertEquals(frame.locator("#snapshot-image")
                        .evaluate("e => e.complete && e.naturalWidth === 1"), true);
                Assert.assertEquals(frame.locator("#referenced-image")
                        .evaluate("e => e.complete && e.naturalWidth === 1"), true);
                Assert.assertEquals(page.evaluate("() => Boolean(window.__capturedScriptRan)"), false);
                Assert.assertTrue(externalRequests.isEmpty(), "Offline adapter requested: " + externalRequests);
            }
        } finally {
            Files.deleteIfExists(archive);
            Files.deleteIfExists(html);
        }
    }

    private static void zipEntry(ZipOutputStream output, String name, String value) throws IOException {
        output.putNextEntry(new ZipEntry(name));
        output.write(value.getBytes(java.nio.charset.StandardCharsets.UTF_8));
        output.closeEntry();
    }

    @Test(groups = "trace-viewer-browser-acceptance")
    public void generatedViewerShouldRemainOfflineAndShareNavigableRangeState() throws Exception {
        Path chrome = chromeExecutable();
        ViewerFixture fixture = generateViewerFixture();
        Path html = fixture.html();
        try (ZipFile zip = new ZipFile(fixture.archive().toFile())) {
            byte[] nativeArchive = zip.getInputStream(zip.getEntry("trace-viewer-native.zip")).readAllBytes();
            Assert.assertEquals(new String(nativeArchive, 0, 2, java.nio.charset.StandardCharsets.US_ASCII), "PK");
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

            page.locator("button[data-tab=nativeEvidence]").click();
            Assert.assertEquals(page.locator("#native-evidence-rows tr").count(), 2);
            Assert.assertEquals(page.locator("#native-evidence-rows tr td:first-child").allTextContents(),
                    List.of("Selected SHAFT action", "Native only"));
            Assert.assertTrue(page.locator("#native-evidence-rows").textContent().contains("CheckoutTest.java:42:7"));
            Assert.assertTrue(page.locator("#native-evidence-rows").textContent().contains("attempting native click"));
            page.locator("#native-evidence-rows tr").nth(1).locator("button").click();
            Assert.assertTrue(page.locator("#details-title").textContent().contains("Native only wait"));
            Assert.assertTrue(page.frameLocator("#comparison-before").locator("body").textContent()
                    .contains("native-only before"));
            Assert.assertTrue(page.frameLocator("#comparison-after").locator("body").textContent()
                    .contains("native-only after"));
            page.locator("#range-start").evaluate("input => { input.value = 0; input.dispatchEvent(new Event('input', {bubbles:true})); }");
            Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK"));
            page.locator("button[data-tab=nativeEvidence]").click();
            Assert.assertEquals(page.locator("#native-evidence-rows tr td:first-child").allTextContents(),
                    List.of("Selected SHAFT action", "Native only"));
            page.navigate(html.toUri() + "#action-action-1?start=50&end=200");
            page.locator("button[data-tab=comparison]").click();
            Assert.assertTrue(page.frameLocator("#comparison-before").locator("body").textContent()
                    .contains("native before"));
            Assert.assertTrue(page.frameLocator("#comparison-input").locator("body").textContent()
                    .contains("native input"));
            Assert.assertTrue(page.frameLocator("#comparison-after").locator("body").textContent()
                    .contains("native after"));
            page.locator("button[data-tab=webSockets]").click();
            Assert.assertEquals(page.locator("#websocket-result-count").textContent(), "3 WebSocket events");
            Assert.assertEquals(page.locator("#websocket-rows tr td:first-child").allTextContents(),
                    List.of("created", "frame", "closed"));
            Assert.assertEquals(page.locator("#websocket-direction-filter option").count(), 3);
            page.locator("#websocket-direction-filter").selectOption("received");
            Assert.assertEquals(page.locator("#websocket-result-count").textContent(), "1 WebSocket event");
            Assert.assertTrue(page.locator("#websocket-rows").textContent().contains("hello from socket"));
            Assert.assertEquals(page.locator("#websocket-injection").count(), 0);
            page.locator("#websocket-rows button").press("Enter");
            Assert.assertTrue(page.locator("#websocket-detail").textContent().contains("socket-1"));
            page.locator("#websocket-direction-filter").selectOption("");

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
            Assert.assertTrue(page.frameLocator("#comparison-before").locator("body").textContent().contains("native before"));
            Assert.assertTrue(page.frameLocator("#comparison-input").locator("body").textContent().contains("native input"));
            Assert.assertTrue(page.frameLocator("#comparison-after").locator("body").textContent().contains("native after"));
            Assert.assertTrue(page.locator("#comparison-action").isHidden(),
                    "The native action-state snapshot should take precedence over the SHAFT screenshot.");

            page.locator("button[data-tab=timeline]").click();
            page.evaluate("""
                    () => {
                      const trace = JSON.parse(document.getElementById('trace-data').textContent);
                      const evidence = trace.evidence || trace;
                      const action = evidence.actions[0];
                      const actionTimes = evidence.actions.map(item => Date.parse(item.startTime));
                      const networkStart = evidence.network[0].timestamp - evidence.network[0].durationMs;
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

            page.locator("button[data-tab=browserObservability]").click();
            Assert.assertTrue(page.locator("#tab-content").textContent().contains("warnings"));

            page.locator("button[data-tab=network]").click();
            page.evaluate("""
                    () => {
                      network.push({method:'DELETE', url:'https://example.test/out-of-range', status:204,
                        requestSizeBytes:1, responseSizeBytes:0, durationMs:1, timestamp:traceEnd});
                      populateNetworkFilters();
                      renderNetwork();
                    }
                    """);
            Assert.assertEquals(page.locator("#network-result-count").textContent(), "2 network exchanges");
            Assert.assertEquals(page.locator("#network-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("out-of-range")).count(), 0);
            int historyBeforeRangeInput = ((Number) page.evaluate("history.length")).intValue();
            page.evaluate("""
                    () => {
                      const trace = JSON.parse(document.getElementById('trace-data').textContent);
                      const evidence = trace.evidence || trace;
                      const event = evidence.network[0];
                      const actionTimes = evidence.actions.map(action => Date.parse(action.startTime));
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
            Assert.assertEquals(page.locator("#artifact-result-count").textContent(), "6 trace artifacts");
            Assert.assertEquals(page.locator("#artifact-rows tr").count(), 6);
            List<String> artifactPaths = page.locator("#artifact-rows tr td:nth-child(1)").allTextContents();
            Assert.assertEquals(artifactPaths.getFirst(), "shaft-network.har");
            Assert.assertTrue(artifactPaths.subList(1, 5).stream().allMatch(path -> path.startsWith("resources/")));
            Assert.assertEquals(artifactPaths.getLast(), "trace-viewer-native.zip");
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(2)").allTextContents(),
                    List.of("network", "screenshot", "screenshot", "dom-snapshot", "dom-snapshot",
                            "native-trace"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(3)").allTextContents(),
                    List.of("application/json", "image/png", "image/png", "text/html", "text/html",
                            "application/zip"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(4)").allTextContents(),
                    List.of("Available", "Available", "Available", "Available", "Available", "Available"));
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

            Assert.assertEquals(page.locator("main").count(), 1, "The viewer needs one primary landmark.");
            Assert.assertEquals(page.locator("h1").count(), 1, "The viewer needs one page heading.");
            Assert.assertEquals(page.locator("#action-search").getAttribute("aria-label"), "Search actions");
            page.locator("#action-tabs button").first().click();
            page.keyboard().press("Tab");
            Assert.assertTrue((Boolean) page.locator("#action-tabs button").nth(1)
                    .evaluate("button => button.matches(':focus-visible')"));
            Assert.assertEquals(page.locator("#action-tabs button").nth(1)
                    .evaluate("button => getComputedStyle(button).outlineWidth"), "3px");
            Assert.assertEquals(page.locator("#action-tabs button").nth(1)
                    .evaluate("button => getComputedStyle(button).outlineOffset"), "2px");
            Assert.assertEquals(page.locator("#action-tabs button").nth(1)
                    .evaluate("button => getComputedStyle(button).outlineColor"),
                    page.locator("#action-tabs button").nth(1)
                            .evaluate("button => getComputedStyle(button).color"));
            Number largeRenderMillis = (Number) page.evaluate("""
                    () => {
                      window.__largeTraceStart = actions.length;
                      for (let index = 0; index < 5000; index++) {
                        actions.push({id:`large-${index}`, name:`Large action ${index}`, category:'element',
                          status:'passed', durationMs:1, metadata:{}});
                      }
                      const start = performance.now();
                      renderActions();
                      return performance.now() - start;
                    }
                    """);
            Assert.assertTrue(largeRenderMillis.doubleValue() < 5_000,
                    "A 5,000-action trace must become interactive within five seconds: " + largeRenderMillis);
            Assert.assertEquals(page.locator("#action-list button").count(),
                    ((Number) page.evaluate("window.__largeTraceStart + 5000")).intValue());
            Number largeSearchMillis = (Number) page.evaluate("""
                    () => {
                      actionSearch.value = 'Large action 4999';
                      const start = performance.now();
                      renderActions();
                      return performance.now() - start;
                    }
                    """);
            Assert.assertTrue(largeSearchMillis.doubleValue() < 1_000,
                    "Filtering a large trace must remain responsive: " + largeSearchMillis);
            Assert.assertEquals(page.locator("#action-list button").count(), 1);
            Assert.assertTrue(page.locator("#action-list button").first().textContent()
                    .contains("Large action 4999"));
            page.evaluate("() => { actions.splice(window.__largeTraceStart); actionSearch.value=''; renderActions(); }");
            String lightBackground = String.valueOf(page.locator("body")
                    .evaluate("body => getComputedStyle(body).backgroundColor"));
            page.emulateMedia(new Page.EmulateMediaOptions().setColorScheme(ColorScheme.DARK)
                    .setReducedMotion(ReducedMotion.REDUCE));
            Assert.assertTrue((Boolean) page.evaluate("matchMedia('(prefers-color-scheme: dark)').matches"));
            Assert.assertTrue((Boolean) page.evaluate("matchMedia('(prefers-reduced-motion: reduce)').matches"));
            Assert.assertNotEquals(page.locator("body").evaluate("body => getComputedStyle(body).backgroundColor"),
                    lightBackground, "Dark mode must switch the report surface tokens.");
            Assert.assertEquals(page.locator(".action").first()
                    .evaluate("element => getComputedStyle(element).transitionDuration"), "0s");
            page.setViewportSize(390, 844);
            Assert.assertTrue((Boolean) page.evaluate(
                    "document.documentElement.scrollWidth <= window.innerWidth"),
                    "The phone layout must not introduce page-level horizontal overflow.");
            Assert.assertEquals(((Number) page.locator(".trace-layout")
                    .evaluate("element => getComputedStyle(element).gridTemplateColumns.split(' ').length"))
                    .intValue(), 1, "The phone layout must collapse to one content column.");
            page.setViewportSize(1440, 1000);
            page.emulateMedia(new Page.EmulateMediaOptions().setColorScheme(ColorScheme.LIGHT)
                    .setReducedMotion(ReducedMotion.NO_PREFERENCE));
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
            page.locator("button[data-tab=browserObservability]").click();
            Assert.assertTrue(page.locator("#tab-content").textContent().contains("warnings"));
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
        try (MockedStatic<PlaywrightTraceManager> traceManager = Mockito.mockStatic(PlaywrightTraceManager.class)) {
            traceManager.when(PlaywrightTraceManager::getLastTracePath).thenReturn(nativeTrace);
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure")
                    .traceIncludeDomSnapshots(true).traceIncludeScreenshots(true)
                    .traceIncludeNetwork(true).traceIncludeConsole(true);
            var first = TraceEventRecorder.startForBackend("element", "CLICK", "#checkout",
                    com.shaft.gui.capabilities.AutomationBackend.MICROSOFT_PLAYWRIGHT);
            TraceEventRecorder.recordScreenshot(first, png);
            Thread.sleep(80);
            TraceEventRecorder.finish(first, "passed", "clicked", null, Map.of(), List.of());
            var second = TraceEventRecorder.start("validation", "TEXT", "#confirmation", driver);
            TraceEventRecorder.recordScreenshot(second, png);
            TraceEventRecorder.finish(second, "failed", "mismatch", new AssertionError("expected receipt"),
                    Map.of("expected", "paid", "actual", "pending"), List.of());
            long firstStart = java.time.Instant.parse(TraceEventRecorder.snapshot().getFirst().startTime())
                    .toEpochMilli();
            try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(nativeTrace))) {
                zipEntry(output, "0-trace.trace", "{\"version\":8,\"type\":\"context-options\","
                        + "\"origin\":\"library\",\"wallTime\":" + firstStart + ",\"monotonicTime\":100}\n"
                        + "{\"type\":\"before\",\"callId\":\"call@1\",\"startTime\":100,"
                        + "\"class\":\"Frame\",\"method\":\"click\",\"title\":\"Native checkout click\","
                        + "\"params\":{},\"stepId\":\"step@1\",\"beforeSnapshot\":\"before@call@1\","
                        + "\"pageId\":\"page@1\",\"stack\":[{\"file\":\"CheckoutTest.java\","
                        + "\"line\":42,\"column\":7}]}\n"
                        + "{\"type\":\"log\",\"callId\":\"call@1\","
                        + "\"message\":\"attempting native click\"}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@1\",\"endTime\":110,"
                        + "\"inputSnapshot\":\"input@call@1\",\"afterSnapshot\":\"after@call@1\"}\n"
                        + nativeSnapshotRecord("before@call@1", "native before", 101)
                        + nativeSnapshotRecord("input@call@1", "native input", 105)
                        + nativeSnapshotRecord("after@call@1", "native after", 109)
                        + "{\"type\":\"before\",\"callId\":\"call@native-only\",\"startTime\":5000,"
                        + "\"class\":\"Page\",\"method\":\"waitForTimeout\","
                        + "\"title\":\"Native only wait\",\"params\":{},\"stepId\":\"step@native\","
                        + "\"beforeSnapshot\":\"before@native-only\"}\n"
                        + "{\"type\":\"after\",\"callId\":\"call@native-only\",\"endTime\":5010,"
                        + "\"afterSnapshot\":\"after@native-only\"}\n"
                        + nativeSnapshotRecord("before@native-only", "native-only before", 5001)
                        + nativeSnapshotRecord("after@native-only", "native-only after", 5009));
            }
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
            BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.captureSession();
            BrowserObservabilityRecorder.recordWebSocket(owner,
                    new BrowserObservabilityRecorder.WebSocketObservation("socket-1", "wss://example.test/socket",
                            "", "created", 0, "", "", 0, "available", ""));
            BrowserObservabilityRecorder.recordWebSocket(owner,
                    new BrowserObservabilityRecorder.WebSocketObservation("socket-1", "wss://example.test/socket",
                            "received", "frame", 1, "<img id=websocket-injection> hello from socket", "", 38,
                            "available", ""));
            BrowserObservabilityRecorder.recordWebSocket(owner,
                    new BrowserObservabilityRecorder.WebSocketObservation("socket-1", "wss://example.test/socket",
                            "", "closed", 0, "", "", 0, "available", ""));

            Method marker = TraceViewerBrowserAcceptanceTest.class.getDeclaredMethod("marker");
            TestExecutionInfo info = new TestExecutionInfo("trace-viewer-browser-acceptance", "customer.CheckoutTest",
                    "traceViewer", "traceViewer", "trace viewer acceptance", marker,
                    new AssertionError("checkout failed"), false);
            FailureTraceReporter.attachOnFailure(info, "trace viewer acceptance", List.of());
            Path archive = FailureTraceReporter.traceDirectory(info).resolve("shaft-trace.zip");
            List<Attachment> currentAttachments = new ArrayList<>();
            Allure.getLifecycle().updateTestCase(result -> currentAttachments.addAll(result.getAttachments()));
            Attachment viewerAttachment = currentAttachments.stream()
                    .filter(attachment -> "text/html".equals(attachment.getType()))
                    .filter(attachment -> attachment.getName().contains("SHAFT Trace Viewer"))
                    .reduce((firstAttachment, secondAttachment) -> secondAttachment)
                    .orElseThrow(() -> new AssertionError("The Allure trace viewer attachment was not published."));
            Path html = allureAttachment(viewerAttachment.getSource());
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

    private static Path allureAttachment(String source) {
        for (Path candidate : List.of(Path.of("allure-results", source),
                Path.of("shaft-engine", "allure-results", source),
                Path.of("target", "allure-results", source))) {
            Path absolute = candidate.toAbsolutePath().normalize();
            if (Files.isRegularFile(absolute)) {
                return absolute;
            }
        }
        throw new AssertionError("The Allure trace viewer attachment bytes are unavailable: " + source);
    }

    private static String nativeSnapshotRecord(String name, String label, int timestamp) {
        return "{\"type\":\"frame-snapshot\",\"snapshot\":{\"callId\":\"call@1\","
                + "\"snapshotName\":\"" + name + "\",\"pageId\":\"page@1\","
                + "\"frameId\":\"frame@1\",\"frameUrl\":\"https://example.test/checkout\","
                + "\"html\":[\"HTML\",{},[\"BODY\",{},\"" + label + "\"]],\"timestamp\":"
                + timestamp + ",\"isMainFrame\":true}}\n";
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
        var legacyObject = (tools.jackson.databind.node.ObjectNode) legacy;
        JsonNode evidence = legacy.path("evidence");
        legacyObject.put("schemaVersion", "1.0");
        legacyObject.set("actions", evidence.path("actions"));
        legacyObject.set("network", evidence.path("network"));
        legacyObject.set("console", evidence.path("console"));
        legacyObject.set("browserObservability", evidence.path("browserObservability"));
        legacyObject.remove("evidence");
        legacyObject.remove("session");
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
