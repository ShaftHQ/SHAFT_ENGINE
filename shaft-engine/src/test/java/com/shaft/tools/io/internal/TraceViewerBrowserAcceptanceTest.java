package com.shaft.tools.io.internal;

import com.microsoft.playwright.Browser;
import com.microsoft.playwright.BrowserContext;
import com.microsoft.playwright.BrowserType;
import com.microsoft.playwright.Mouse;
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
            page.context().setOffline(true);
            page.onPageError(pageErrors::add);
            page.onRequest(request -> {
                String url = request.url();
                if (!url.startsWith("file:") && !url.startsWith("data:")) {
                    externalRequests.add(url);
                }
            });

            openViewer(page, html.toUri() + "#action-action-1?start=50&end=200");
            Assert.assertTrue(html.toFile().length() > 0 && Files.readString(html).contains("data-encoding=\"gzip+base64\""),
                    "The trace payload must be embedded compressed.");
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
            openViewer(page, html.toUri() + "#action-action-1?start=50&end=200");
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
            Assert.assertTrue(page.locator("#network-request-headers").textContent().contains("x-request"));
            Assert.assertTrue(page.locator("#network-response-headers").textContent().contains("response-value"));
            Assert.assertEquals(page.locator("#network-response-body").textContent(), "retry later");
            Assert.assertTrue(page.locator("#network-request-body").textContent().contains("5 B"),
                    "An uncaptured request body must say what was recorded instead.");
            Assert.assertTrue(page.locator("#network-body-truncated").isHidden());
            Assert.assertTrue(page.locator("#network-detail-general").textContent().contains("upstream unavailable"));
            Assert.assertEquals(page.locator("#network-rows tr td").nth(1)
                    .evaluate("cell => getComputedStyle(cell).whiteSpace + '/' + getComputedStyle(cell.closest('table')).tableLayout"),
                    "nowrap/auto", "Short network columns must not wrap mid-token.");
            page.locator("#network-panel").screenshot(new com.microsoft.playwright.Locator.ScreenshotOptions()
                    .setPath(sibling(screenshot, "-network")));
            @SuppressWarnings("unchecked")
            Map<String, Object> networkDetail = (Map<String, Object>) page.evaluate(
                    "JSON.parse(document.getElementById('network-detail-raw').textContent)");
            Assert.assertEquals(networkDetail.get("failureReason"), "upstream unavailable");
            Assert.assertEquals(((Number) networkDetail.get("requestSizeBytes")).intValue(), 5);
            Assert.assertEquals(((Number) networkDetail.get("responseSizeBytes")).intValue(), 12);
            Assert.assertTrue(String.valueOf(networkDetail.get("requestHeaders")).contains("network-injection"));
            Assert.assertTrue(String.valueOf(networkDetail.get("responseHeaders")).contains("response-value"));
            Assert.assertEquals(page.locator("#network-injection").count(), 0,
                    "Network detail must render hostile text without creating markup.");
            page.evaluate("""
                    () => showNetworkDetail({method:'GET', url:'https://example.test/api', status:200,
                      responseHeaders:{'Content-Type':'application/json; charset=utf-8'}, bodyPreview:'{"order":{"id":7}}'})
                    """);
            Assert.assertEquals(page.locator("#network-response-body").getAttribute("data-kind"), "json");
            Assert.assertTrue(page.locator("#network-response-body").textContent().contains("\n  \"order\": {"),
                    "JSON bodies must be pretty-printed.");
            Assert.assertTrue(page.locator("#network-detail-general").textContent().contains("application/json"));
            page.evaluate("""
                    () => showNetworkDetail({method:'GET', url:'https://example.test/pixel.png', status:200,
                      responseHeaders:{'content-type':'image/png'},
                      bodyPreview:'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII='})
                    """);
            Assert.assertTrue(page.locator("#network-response-image").isVisible(), "Image bodies must preview inline.");
            Assert.assertTrue(String.valueOf(page.locator("#network-response-image").getAttribute("src"))
                    .startsWith("data:image/png;base64,"));
            page.evaluate("() => showNetworkDetail({method:'GET', url:'https://example.test/big', status:200, bodyPreview:'x'.repeat(2048)})");
            Assert.assertTrue(page.locator("#network-body-truncated").isVisible(), "Truncated previews must be marked.");
            page.locator("#network-detail-close").click();
            Assert.assertTrue(page.locator("#network-detail").isHidden());
            page.locator("#network-sort-contentType button").click();
            Assert.assertEquals(page.locator("#network-sort-contentType").getAttribute("aria-sort"), "ascending");
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
            Assert.assertEquals(page.locator("#network-rows tr").last().locator("td").nth(7).textContent(),
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

            openViewer(page, html.toUri() + "#action-action-1");
            page.reload();
            page.waitForFunction("() => window.shaftTraceReady === true");
            Assert.assertEquals(((Number) page.evaluate("network.length")).intValue(), 2,
                    "Same-document navigation must not leak earlier mutation fixtures into range acceptance.");
            page.evaluate("""
                    () => {
                      network.push({method:'PATCH', url:'legacy://untimed', status:0,
                        requestSizeBytes:4, failureReason:'legacy entry'});
                      consoleEvents.push({});
                    }
                    """);
            Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK"));
            Assert.assertNotEquals(page.locator("#range-start").inputValue(),
                    page.locator("#range-end").inputValue(), "A legacy action link must select its action interval.");
            Assert.assertEquals(page.locator("#trace-filmstrip button[role=option]").count(), 2,
                    "By default the filmstrip shows only actions with captured screenshots.");
            page.locator("#filmstrip-show-all").check();
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
            for (String side : List.of("before", "action", "after")) {
                page.locator("#snapshot-tabs button[data-snapshot=" + side + "]").click();
                Assert.assertTrue(page.locator("#comparison-" + side + "-empty").isVisible(), side);
            }
            page.locator("#snapshot-tabs button[data-snapshot=action]").click();

            page.locator("#trace-filmstrip button").first().click();
            page.locator("button[data-tab=comparison]").click();
            Assert.assertTrue(page.frameLocator("#comparison-before").locator("body").textContent().contains("native before"));
            Assert.assertTrue(page.frameLocator("#comparison-input").locator("body").textContent().contains("native input"));
            Assert.assertTrue(page.frameLocator("#comparison-after").locator("body").textContent().contains("native after"));
            Assert.assertTrue(page.locator("#comparison-action").isHidden(),
                    "The native action-state snapshot should take precedence over the SHAFT screenshot.");

            page.locator("button[data-tab=log]").click();
            Assert.assertTrue(page.locator("#actionability-steps").textContent().contains("attempting native click"),
                    "The Log tab lists the selected action's actionability steps.");
            Assert.assertTrue(page.locator("#test-log").textContent().contains("trace viewer acceptance"));
            page.locator("#trace-filmstrip button").nth(1).click();
            page.locator("button[data-tab=call]").click();
            Assert.assertTrue(page.locator("#call-details").textContent().contains("#confirmation"));
            Assert.assertTrue(page.locator("#call-details").textContent().contains("paid"));
            Assert.assertTrue(page.locator("#call-empty").isHidden());
            page.locator("button[data-tab=errors]").click();
            Assert.assertTrue(page.locator("#error-list").textContent().contains("expected receipt"),
                    page.locator("#error-list").textContent());
            Assert.assertEquals(page.locator("#trace-error-markers button").count(), 1,
                    "Each failed timed action is marked on the timeline.");
            page.locator("#trace-filmstrip button").first().click();
            page.locator("#trace-error-markers button").click();
            Assert.assertTrue(page.locator("#details-title").textContent().contains("TEXT"));
            page.locator("button[data-tab=source]").click();
            Assert.assertTrue(page.locator("#source-hint").textContent().startsWith("The source file was not embedded"),
                    "A frame-only source context must say the file is missing instead of rendering it as code.");
            Assert.assertEquals(page.locator("#source-lines li").count(), 0);
            page.evaluate("""
                    () => {
                      trace.source = {file:'CheckoutTest.java', frame:'customer.CheckoutTest.pay(CheckoutTest.java:3)',
                        line:'3', fileContent:'package customer;\\npublic class CheckoutTest {\\n  void pay() { int total = 1; }\\n}'};
                      trace.exception.stacktrace = 'java.lang.AssertionError: checkout failed\\n\\tat customer.CheckoutTest.pay(CheckoutTest.java:2)\\n\\tat org.testng.Runner.run(Runner.java:9)';
                    }
                    """);
            page.locator("button[data-tab=errors]").click();
            page.locator("#error-list .error-source").first().click();
            Assert.assertEquals(page.locator("button[data-tab=source]").getAttribute("class"), "selected");
            Assert.assertEquals(page.locator("#source-lines [aria-current=true]").getAttribute("id"), "source-line-3",
                    "Jumping from an error highlights its source line.");
            Assert.assertTrue(page.locator("#source-line-3").getAttribute("class").contains("failed"));
            Assert.assertTrue(page.locator("#source-lines .tok-kw").count() > 0, "Source must be syntax-highlighted.");
            Assert.assertEquals(page.locator("#source-frames button").count(), 2);
            Assert.assertTrue(page.locator("#source-frames button").nth(1).isDisabled(),
                    "Frames whose source is not embedded cannot be navigated.");
            page.locator("#source-frames button").first().click();
            Assert.assertEquals(page.locator("#source-lines [aria-current=true]").getAttribute("id"), "source-line-2",
                    "Each embedded stack frame is navigable.");
            page.locator("#source-panel").screenshot(new com.microsoft.playwright.Locator.ScreenshotOptions()
                    .setPath(sibling(screenshot, "-source")));
            page.locator("button[data-tab=errors]").click();
            page.locator("#errors-panel").screenshot(new com.microsoft.playwright.Locator.ScreenshotOptions()
                    .setPath(sibling(screenshot, "-errors")));
            page.locator("#trace-filmstrip button").first().click();

            page.locator("button[data-tab=timeline]").click();
            page.evaluate("""
                    () => {
                      const trace = JSON.parse(window.shaftTraceText);
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
            Assert.assertTrue(page.locator("#network-rows tr").count() >= 1,
                    "The selected action range must retain its overlapping network evidence.");
            Assert.assertEquals(page.locator("#network-rows tr").filter(
                    new com.microsoft.playwright.Locator.FilterOptions().setHasText("out-of-range")).count(), 0);
            int historyBeforeRangeInput = ((Number) page.evaluate("history.length")).intValue();
            page.evaluate("""
                    () => {
                      const trace = JSON.parse(window.shaftTraceText);
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
            Assert.assertEquals(page.locator("#artifact-result-count").textContent(), "5 trace artifacts (6 references)",
                    "Artifacts with the same digest must collapse into one row.");
            Assert.assertEquals(page.locator("#artifact-rows tr").count(), 5);
            List<String> artifactNames = page.locator("#artifact-rows .artifact-name").allTextContents();
            Assert.assertEquals(artifactNames.getFirst(), "shaft-network.har");
            Assert.assertTrue(artifactNames.subList(1, 4).stream()
                    .allMatch(name -> name.matches("(screenshot|dom-snapshot) [0-9a-f]{8}\\.(png|html)")), artifactNames.toString());
            Assert.assertEquals(artifactNames.getLast(), "trace-viewer-native.zip");
            Assert.assertTrue(page.locator("#artifact-rows .artifact-name").nth(1).getAttribute("title")
                    .matches("resources/[0-9a-f]{64}\\.png"), "The full path stays available on hover.");
            Assert.assertEquals(page.locator("#artifact-rows .artifact-name").nth(1)
                    .evaluate("element => getComputedStyle(element).whiteSpace"), "nowrap",
                    "Artifact names must not wrap mid-token.");
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(2)").allTextContents(),
                    List.of("network", "screenshot", "dom-snapshot", "dom-snapshot", "native-trace"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(3)").allTextContents(),
                    List.of("application/json", "image/png", "text/html", "text/html", "application/zip"));
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(4)").allTextContents(),
                    List.of("Available", "Available", "Available", "Available", "Available"));
            Assert.assertTrue(page.locator("#artifact-rows tr td:nth-child(5)").allTextContents().stream()
                    .allMatch(size -> size.endsWith(" B")));
            Assert.assertTrue(page.locator("#artifact-rows tr td:nth-child(6)").allTextContents().stream()
                    .allMatch(digest -> digest.matches("[0-9a-f]{12}")));
            String screenshotUsers = page.locator("#artifact-rows tr").nth(1).locator("td").nth(6).textContent();
            Assert.assertTrue(screenshotUsers.contains("action-1") && screenshotUsers.contains("action-2"),
                    "A shared screenshot row lists every action using it: " + screenshotUsers);
            page.locator("#artifact-kind-filter").selectOption("dom-snapshot");
            Assert.assertEquals(page.locator("#artifact-rows tr").count(), 2);
            page.locator("#artifact-kind-filter").selectOption("");
            page.locator("#artifact-sort-kind button").click();
            Assert.assertEquals(page.locator("#artifact-sort-kind").getAttribute("aria-sort"), "ascending");
            Assert.assertEquals(page.locator("#artifact-rows tr td:nth-child(2)").allTextContents(),
                    List.of("dom-snapshot", "dom-snapshot", "native-trace", "network", "screenshot"));
            page.locator("#artifact-sort-kind button").click();
            page.evaluate("() => { artifactSort = {key:'', direction:'ascending'}; renderArtifacts(); }");
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
            keepGeneratedHtml(html);

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
            Assert.assertTrue(largeRenderMillis.doubleValue() < 2_000,
                    "A 5,000-action trace must become interactive within two seconds: " + largeRenderMillis);
            Assert.assertEquals(page.locator("#action-list .action").count(),
                    ((Number) page.evaluate("RENDER_CHUNK")).intValue(), "Large action lists render in windows.");
            Assert.assertTrue(page.locator("#action-list .list-more button").textContent().contains("not shown"));
            page.locator("#action-list .list-more button").click();
            Assert.assertEquals(page.locator("#action-list .action").count(),
                    2 * ((Number) page.evaluate("RENDER_CHUNK")).intValue());
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
            Assert.assertEquals(page.locator("#action-list .action").count(), 1);
            Assert.assertTrue(page.locator("#action-list .action").first().textContent()
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
            page.screenshot(new Page.ScreenshotOptions().setPath(sibling(screenshot, "-dark")).setFullPage(true));
            page.setViewportSize(390, 844);
            page.screenshot(new Page.ScreenshotOptions().setPath(sibling(screenshot, "-narrow")).setFullPage(true));
            Assert.assertTrue((Boolean) page.evaluate(
                    "document.documentElement.scrollWidth <= window.innerWidth"),
                    "The phone layout must not introduce page-level horizontal overflow.");
            Assert.assertEquals(((Number) page.locator(".trace-layout")
                    .evaluate("element => getComputedStyle(element).gridTemplateColumns.split(' ').length"))
                    .intValue(), 1, "The phone layout must collapse to one content column.");
            page.setViewportSize(1440, 1000);
            page.emulateMedia(new Page.EmulateMediaOptions().setColorScheme(ColorScheme.LIGHT)
                    .setReducedMotion(ReducedMotion.NO_PREFERENCE));
            openViewer(page, fixture.legacyHtml().toUri().toString());
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
            TraceEventRecorder.recordVisualComparison("Visual comparison", visualPng(java.awt.Color.WHITE),
                    visualPng(java.awt.Color.ORANGE), visualPng(java.awt.Color.RED));
            TraceEventRecorder.record("evidence", "NO EVIDENCE", "passed", "", null,
                    "optional evidence omitted", null, Map.of(), List.of());
            BrowserObservabilityRecorder.recordNetwork(new BrowserObservabilityRecorder.NetworkObservation(
                    "POST", "https://example.test/payment", 200, Map.of("Content-Type", "application/json"), Map.of(),
                    200, 10, 20, "", "ok", "{\"card\":\"4111\",\"password\":\"hunter2\"}"));
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
            Allure.getLifecycle().updateTest(result -> currentAttachments.addAll(result.getAttachments()));
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

    private static byte[] visualPng(java.awt.Color color) throws IOException {
        java.awt.image.BufferedImage image = new java.awt.image.BufferedImage(120, 60,
                java.awt.image.BufferedImage.TYPE_INT_RGB);
        var graphics = image.createGraphics();
        graphics.setColor(color);
        graphics.fillRect(0, 0, 120, 60);
        graphics.dispose();
        java.io.ByteArrayOutputStream output = new java.io.ByteArrayOutputStream();
        javax.imageio.ImageIO.write(image, "png", output);
        return output.toByteArray();
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
        String decoded = TraceViewerHtml.embeddedJson(html);
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
        Path target = currentHtml.resolveSibling("trace-viewer-browser-acceptance-v1.html");
        Files.writeString(target, TraceViewerHtml.withEmbeddedJson(html, JSON.writeValueAsString(legacy)));
        return target;
    }

    @Test(groups = "trace-viewer-browser-acceptance")
    public void thousandActionTraceShouldOpenOfflineAndBecomeInteractiveWithinTwoSeconds() throws Exception {
        Method marker = TraceViewerBrowserAcceptanceTest.class.getDeclaredMethod("marker");
        TestExecutionInfo info = new TestExecutionInfo("trace-viewer-large-acceptance", "customer.LargeTraceTest",
                "largeTrace", "largeTrace", "large trace acceptance", marker,
                new AssertionError("large trace failed"), false);
        Path directory = FailureTraceReporter.traceDirectory(info);
        Path html = Path.of("target", "trace-viewer-large-acceptance.html").toAbsolutePath().normalize();
        try {
            SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");
            for (int index = 0; index < 1_000; index++) {
                TraceEventRecorder.record("element", "CLICK " + index, index == 999 ? "failed" : "passed",
                        "#item-" + index, null, "action " + index, null, Map.of(), List.of());
            }
            FailureTraceReporter.attachOnFailure(info, "large trace acceptance", List.of());
            extract(directory.resolve("shaft-trace.zip"), "SHAFT Trace Report.html", html);
            try (Playwright playwright = Playwright.create();
                 Browser browser = playwright.chromium().launch(new BrowserType.LaunchOptions()
                         .setExecutablePath(chromeExecutable()).setHeadless(true))) {
                Page page = browser.newPage(new Browser.NewPageOptions().setViewportSize(1440, 1000));
                page.context().setOffline(true);
                List<String> pageErrors = new ArrayList<>();
                page.onPageError(pageErrors::add);
                openViewer(page, html.toUri().toString());
                double readyMillis = ((Number) page.evaluate("window.shaftTraceReadyMs")).doubleValue();
                Assert.assertTrue(readyMillis < 2_000, "A 1,000-action trace must be interactive in under 2 s: " + readyMillis);
                Assert.assertEquals(((Number) page.evaluate("actions.length")).intValue(), 1_000);
                Assert.assertEquals(page.locator("#action-list .action").count(), 1_000,
                        "The selected (last, failed) action's window must be rendered.");
                Assert.assertTrue(page.locator("#details-title").textContent().contains("CLICK 999"));
                page.locator("#action-search").fill("CLICK 12");
                Assert.assertTrue(page.locator("#action-list .action").count() >= 1);
                Assert.assertTrue(pageErrors.isEmpty(), "Page errors: " + pageErrors);
            }
        } finally {
            TraceEventRecorder.clear();
            Properties.clearForCurrentThread();
            Files.deleteIfExists(html);
            if (Files.exists(directory)) {
                try (var paths = Files.walk(directory)) {
                    for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) {
                        Files.deleteIfExists(path);
                    }
                }
            }
        }
    }

    @Test(groups = "trace-viewer-browser-acceptance")
    public void snapshotsFilmstripRangeAndAttachmentsShouldMatchPlaywrightInteractions() throws Exception {
        Path chrome = chromeExecutable();
        ViewerFixture fixture = generateViewerFixture();
        Path screenshot = Path.of(System.getProperty("shaft.trace.viewer.screenshot",
                "target/trace-viewer-browser-acceptance.png")).toAbsolutePath().normalize();
        Files.createDirectories(screenshot.getParent());
        List<String> pageErrors = new ArrayList<>();
        List<String> externalRequests = new ArrayList<>();
        try (Playwright playwright = Playwright.create();
             Browser browser = playwright.chromium().launch(new BrowserType.LaunchOptions()
                     .setExecutablePath(chrome).setHeadless(true))) {
            BrowserContext context = browser.newContext(new Browser.NewContextOptions().setViewportSize(1440, 1000));
            context.setOffline(true);
            context.onRequest(request -> {
                String url = request.url();
                if (!url.startsWith("file:") && !url.startsWith("data:") && !url.startsWith("blob:")) {
                    externalRequests.add(url);
                }
            });
            Page page = context.newPage();
            page.onPageError(pageErrors::add);
            openViewer(page, fixture.html().toUri().toString());

            // #6726: captured frames only by default, accessible labels, keyboard navigation, opt-in for every action.
            Assert.assertEquals(page.locator("#trace-filmstrip button[role=option]").count(), 2);
            Assert.assertEquals(page.locator("#trace-filmstrip .filmstrip-missing").count(), 0);
            Assert.assertTrue(page.locator("#trace-filmstrip button").first().getAttribute("aria-label")
                    .matches("CLICK at \\+\\d+\\.\\d{3}s"), page.locator("#trace-filmstrip button").first().getAttribute("aria-label"));
            Assert.assertEquals(page.locator("#trace-filmstrip").evaluate("e => getComputedStyle(e).scrollSnapType"),
                    "x");
            page.locator("#trace-filmstrip button").first().focus();
            page.keyboard().press("End");
            Assert.assertEquals(page.locator("#trace-filmstrip button:focus").getAttribute("data-action-id"), "action-2");
            page.keyboard().press("Home");
            Assert.assertEquals(page.locator("#trace-filmstrip button:focus").getAttribute("data-action-id"), "action-1");
            page.locator("#filmstrip-show-all").check();
            Assert.assertEquals(page.locator("#trace-filmstrip button[role=option]").count(), 3);
            Assert.assertTrue(page.locator("#trace-filmstrip button").nth(2).getAttribute("aria-label")
                    .endsWith("no screenshot"));
            page.locator("#filmstrip-show-all").uncheck();

            // #6717: hover magnification and snapshot preview.
            page.locator("#trace-filmstrip button").first().hover();
            Assert.assertTrue(page.locator("#hover-preview.magnified img").isVisible(),
                    "Hovering a filmstrip frame magnifies its screenshot.");
            page.mouse().move(5, 5);
            Assert.assertTrue(page.locator("#hover-preview").isHidden());

            // #6716: Before / Action / After for click, fill and navigation with target and click-point highlights.
            page.evaluate("""
                    () => {
                      const page = (body) => `<html><body>${body}</body></html>`;
                      const form = '<main><input name="email" data-testid="email-field"><button id="pay">Pay now</button>'
                        + '<button class="secondary">Pay now later</button><a href="#">Help</a></main>';
                      const add = (id, name, locator, offset, before, after) => actions.push({id, backend:'SELENIUM',
                        category:'element', name, status:'passed', locator, url:'https://example.test/checkout',
                        startTime:new Date(baseTime + offset).toISOString(), durationMs:4, metadata:{},
                        domSnapshotBefore:page(before), domSnapshotAfter:page(after)});
                      add('pr-b-click', 'CLICK pay', 'By.id: pay', 1, form, '<main><p id="done">Paid</p></main>');
                      add('pr-b-fill', 'TYPE email', 'By.name: email', 2, form, form.replace('<input', '<input value="a@b.c"'));
                      add('pr-b-nav', 'NAVIGATE', '', 3, '<main>old page</main>', '<main>new page</main>');
                      renderActions();
                    }
                    """);
            for (String id : List.of("pr-b-click", "pr-b-fill")) {
                page.evaluate("id => selectAction(actions.find(action => action.id === id))", id);
                page.locator("button[data-tab=comparison]").click();
                Assert.assertEquals(page.locator("#snapshot-tabs button[role=tab]").allTextContents(),
                        List.of("Before", "Action", "After"));
                page.locator("#snapshot-tabs button[data-snapshot=before]").click();
                Assert.assertTrue(page.locator("#comparison-before").isVisible());
                Assert.assertEquals(page.frameLocator("#comparison-before").locator("[data-shaft-target]").count(), 1, id);
                page.locator("#snapshot-tabs button[data-snapshot=action]").click();
                Assert.assertEquals(page.frameLocator("#comparison-input").locator("[data-shaft-target][data-shaft-click]")
                        .count(), 1, id);
                Assert.assertTrue(String.valueOf(page.frameLocator("#comparison-input").locator("[data-shaft-click]")
                        .evaluate("e => getComputedStyle(e).backgroundImage")).startsWith("radial-gradient"),
                        "The Action snapshot draws the click point at the target's center.");
                Assert.assertTrue(String.valueOf(page.frameLocator("#comparison-input").locator("[data-shaft-target]")
                        .evaluate("e => getComputedStyle(e).outlineStyle")).equals("solid"));
                page.locator("#snapshot-tabs button[data-snapshot=after]").click();
                Assert.assertEquals(page.frameLocator("#comparison-after").locator("[data-shaft-target]").count(), 0);
                Assert.assertTrue(page.locator("#snapshot-target").textContent().contains("is outlined"));
            }
            page.evaluate("() => selectAction(actions.find(action => action.id === 'pr-b-nav'))");
            Assert.assertEquals(page.locator("#snapshot-tabs button[role=tab]").count(), 3);
            Assert.assertTrue(page.frameLocator("#comparison-after").locator("body").textContent().contains("new page"));
            Assert.assertEquals(page.locator("#snapshot-target").textContent().startsWith("This action has no target"), true);
            page.locator("#snapshot-tabs button[data-snapshot=after]").focus();
            page.keyboard().press("ArrowLeft");
            Assert.assertEquals(page.locator("#snapshot-tabs button[aria-selected=true]").textContent(), "Action");

            // #6716: pop the visible snapshot out into its own tab.
            page.evaluate("() => selectAction(actions.find(action => action.id === 'pr-b-click'))");
            page.locator("#snapshot-tabs button[data-snapshot=before]").click();
            Page popout = context.waitForPage(() -> page.locator("#snapshot-popout").click());
            popout.waitForLoadState();
            Assert.assertTrue(popout.url().startsWith("blob:"), popout.url());
            Assert.assertEquals(popout.locator("#pay[data-shaft-target]").count(), 1);
            popout.close();

            // #6722: pick a locator from the snapshot; it must resolve to exactly that element.
            page.locator("#snapshot-tabs button[data-snapshot=action]").click();
            page.locator("#snapshot-pick").click();
            Assert.assertEquals(page.locator("#snapshot-pick").getAttribute("aria-pressed"), "true");
            page.frameLocator("#comparison-input").locator("a").click();
            Assert.assertTrue(page.frameLocator("#comparison-input").locator("#pay").isVisible(),
                    "Picking must not follow snapshot links.");
            Assert.assertEquals(page.locator("#picked-locator-code").textContent(),
                    "SHAFT.GUI.Locator.hasTagName(\"a\").hasText(\"Help\").build()");

            page.frameLocator("#comparison-input").locator("[name=email]").click();
            Assert.assertEquals(page.locator("#picked-locator-code").textContent(),
                    "By.cssSelector(\"[data-testid=\\\"email-field\\\"]\")");
            Assert.assertEquals(page.frameLocator("#comparison-input").locator("[data-testid=\"email-field\"]").count(), 1);
            page.frameLocator("#comparison-input").locator("button.secondary").click();
            Assert.assertEquals(page.locator("#picked-locator-code").textContent(),
                    "SHAFT.GUI.Locator.hasTagName(\"button\").hasText(\"Pay now later\").build()");
            Assert.assertEquals(((Number) page.frameLocator("#comparison-input").locator("body").evaluate(
                    "(body, xpath) => body.ownerDocument.evaluate(xpath, body.ownerDocument, null, 7, null).snapshotLength",
                    page.evaluate("window.shaftPickedLocator.xpath"))).intValue(), 1);
            Assert.assertTrue(page.locator("#picked-locator-detail").textContent().contains("Matches exactly this element"));
            page.frameLocator("#comparison-input").locator("#pay").click();
            Assert.assertEquals(page.locator("#picked-locator-code").textContent(), "By.id(\"pay\")");
            page.locator("#snapshot-pick").click();
            Assert.assertEquals(page.locator("#snapshot-pick").getAttribute("aria-pressed"), "false");
            page.locator("#comparison-panel").screenshot(new com.microsoft.playwright.Locator.ScreenshotOptions()
                    .setPath(sibling(screenshot, "-snapshots")));

            // #6717: drag on the timeline track to select a range that filters actions, network, console and log.
            page.locator("#show-all-range").click();
            page.evaluate("""
                    () => {
                      trace.timeline.push(new Date(baseTime + 1).toISOString() + ' [main] early step',
                        new Date(traceEnd).toISOString() + ' [main] late step');
                    }
                    """);
            int allActions = page.locator("#action-list .action").count();
            var track = page.locator("#timeline-track").boundingBox();
            page.mouse().move(track.x + 1, track.y + track.height / 2);
            page.mouse().down();
            page.mouse().move(track.x + track.width * 0.03, track.y + track.height / 2, new Mouse.MoveOptions().setSteps(4));
            page.mouse().up();
            Assert.assertTrue(page.locator("#range-selection").isVisible());
            Assert.assertTrue(page.locator("#action-list .action").count() < allActions,
                    "A dragged range filters the action list.");
            page.locator("button[data-tab=network]").click();
            int overlapping = ((Number) page.evaluate("() => network.filter(entry => "
                    + "intervalOverlaps(networkStartMs(entry), entry.durationMs, selectedWindow())).length")).intValue();
            Assert.assertTrue(overlapping < 2, "The dragged range must exclude at least one exchange.");
            Assert.assertEquals(page.locator("#network-rows tr").count(), overlapping,
                    "Network shows only exchanges overlapping the dragged range.");
            page.locator("button[data-tab=console]").click();
            Assert.assertEquals(page.locator("#console-result-count").textContent(), "0 console messages",
                    "Console messages logged after the dragged range are filtered out.");
            page.locator("button[data-tab=log]").click();
            Assert.assertTrue(page.locator("#test-log").textContent().contains("early step"));
            Assert.assertFalse(page.locator("#test-log").textContent().contains("late step"));
            Assert.assertTrue(page.locator("#test-log-count").textContent().contains("lines in the selected range"));
            page.locator("#show-all-range").click();
            Assert.assertEquals(page.locator("#action-list .action").count(), allActions);
            Assert.assertTrue(page.locator("#range-selection").isHidden());
            Assert.assertTrue(page.locator("#test-log").textContent().contains("late step"));
            page.locator("#action-list .action").first().dblclick();
            Assert.assertTrue(page.locator("#action-list .action").count() < allActions,
                    "Double-clicking an action selects its range and filters the list.");
            page.locator("#action-list .action").first().click();
            Assert.assertEquals(page.locator("#action-list .action").count(), allActions,
                    "A single click selects an action without hiding the rest.");
            page.locator("#trace-error-markers button").first().click();
            Assert.assertTrue(page.locator("#details-title").textContent().contains("TEXT"));

            // #6732: request body preview, redacted and pretty-printed.
            page.locator("#show-all-range").click();
            page.locator("button[data-tab=network]").click();
            page.locator("#network-method-filter").selectOption("POST");
            page.locator("#network-rows tr button").click();
            String requestBody = page.locator("#network-request-body").textContent();
            Assert.assertTrue(requestBody.contains("\"card\": \"4111\""), requestBody);
            Assert.assertFalse(requestBody.contains("hunter2"), requestBody);
            Assert.assertTrue(requestBody.contains("********"), requestBody);
            Assert.assertTrue(page.locator("#network-request-truncated").isHidden());
            page.evaluate("() => showNetworkDetail({method:'PUT', url:'https://example.test/big', status:200, requestBody:'[omitted because browser metadata exceeded the safe redaction boundary]'})");
            Assert.assertTrue(page.locator("#network-request-truncated").isVisible());

            // #6722: visual comparison slider in Attachments.
            page.locator("button[data-tab=attachments]").click();
            Assert.assertTrue(page.locator("#attachments-hint").textContent().contains("1 visual comparison"));
            Assert.assertEquals(page.locator(".visual-comparison .tabs button").allTextContents(),
                    List.of("Slider", "Expected", "Actual", "Diff"));
            Assert.assertTrue(page.locator(".diff-slider .diff-expected").evaluate("e => e.complete && e.naturalWidth === 120")
                    .equals(true));
            page.locator("#visual-slider-0").fill("20");
            Assert.assertEquals(page.locator(".diff-slider").evaluate("e => e.style.getPropertyValue('--split')"), "20%");
            Assert.assertTrue(String.valueOf(page.locator(".diff-slider .diff-actual")
                    .evaluate("e => getComputedStyle(e).clipPath")).contains("80%"));
            page.locator(".visual-comparison .tabs button", new Page.LocatorOptions().setHasText("Diff")).click();
            Assert.assertEquals(page.locator(".visual-view img").getAttribute("alt"), "diff image");
            page.locator(".visual-comparison .tabs button", new Page.LocatorOptions().setHasText("Slider")).click();
            page.locator("#attachments-panel").screenshot(new com.microsoft.playwright.Locator.ScreenshotOptions()
                    .setPath(sibling(screenshot, "-attachments")));

            // #6733: downscaled screenshots are labelled in Artifacts.
            page.evaluate("""
                    () => {
                      const shot = artifacts.find(artifact => artifact.kind === 'screenshot');
                      shot.metadata = {...shot.metadata, downscaled:'true', originalSizeBytes:'3145728'};
                      renderArtifacts();
                    }
                    """);
            page.locator("button[data-tab=artifacts]").click();
            Assert.assertTrue(page.locator("#artifact-rows").textContent().contains("Downscaled from 3145728 B"));
            Assert.assertTrue(page.locator("#artifact-rows td:nth-child(4)").allTextContents().contains("Downscaled"));

            page.evaluate("() => selectAction(actions.find(action => action.id === 'pr-b-click'))");
            page.locator("button[data-tab=comparison]").click();
            page.screenshot(new Page.ScreenshotOptions().setPath(sibling(screenshot, "-pr-b")).setFullPage(true));
            page.emulateMedia(new Page.EmulateMediaOptions().setColorScheme(ColorScheme.DARK));
            page.screenshot(new Page.ScreenshotOptions().setPath(sibling(screenshot, "-pr-b-dark")).setFullPage(true));
            page.setViewportSize(390, 844);
            page.screenshot(new Page.ScreenshotOptions().setPath(sibling(screenshot, "-pr-b-narrow")).setFullPage(true));
            Assert.assertTrue((Boolean) page.evaluate("document.documentElement.scrollWidth <= window.innerWidth"),
                    "The phone layout must not overflow horizontally.");
            Assert.assertTrue(pageErrors.isEmpty(), "Page errors: " + pageErrors);
            Assert.assertTrue(externalRequests.isEmpty(), "External requests: " + externalRequests);
        } finally {
            deleteTraceFixture();
        }
    }

    private static Path sibling(Path screenshot, String suffix) {
        String name = screenshot.getFileName().toString();
        int dot = name.lastIndexOf('.');
        return screenshot.resolveSibling(dot < 0 ? name + suffix : name.substring(0, dot) + suffix + name.substring(dot));
    }

    private static void keepGeneratedHtml(Path html) throws IOException {
        String target = System.getProperty("shaft.trace.viewer.keepHtml", "");
        if (!target.isBlank()) {
            Path copy = Path.of(target).toAbsolutePath().normalize();
            Files.createDirectories(copy.getParent());
            Files.copy(html, copy, StandardCopyOption.REPLACE_EXISTING);
        }
    }

    private static void openViewer(Page page, String url) {
        page.navigate(url);
        page.waitForFunction("() => window.shaftTraceReady === true");
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
