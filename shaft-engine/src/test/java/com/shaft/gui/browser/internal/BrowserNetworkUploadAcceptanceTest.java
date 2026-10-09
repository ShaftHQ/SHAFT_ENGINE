package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.openqa.selenium.remote.http.HttpResponse;
import org.openqa.selenium.support.ui.WebDriverWait;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.time.Duration;
import java.util.HexFormat;

/**
 * Issue #6735: trace network capture must not corrupt binary multipart uploads.
 */
public class BrowserNetworkUploadAcceptanceTest {
    private static final String PAGE = """
            <!doctype html><title>upload</title><p id="result">pending</p>
            <script>
            const bytes = new Uint8Array(1024);
            const png = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0, 0, 0, 13, 0x49, 0x48, 0x44, 0x52];
            for (let i = 0; i < bytes.length; i++) bytes[i] = i < png.length ? png[i] : (i * 37 + 11) % 256;
            async function sha(buffer) {
              const digest = await crypto.subtle.digest('SHA-256', buffer);
              return [...new Uint8Array(digest)].map(b => b.toString(16).padStart(2, '0')).join('');
            }
            async function upload() {
              const form = new FormData();
              form.append('note', 'hello');
              form.append('file', new Blob([bytes], {type: 'image/png'}), 'pixel.png');
              const response = await fetch('/upload', {method: 'POST', body: form});
              const received = await response.text();
              document.getElementById('result').textContent = (await sha(bytes)) === received ? 'match' : 'mismatch';
            }
            upload().catch(error => { document.getElementById('result').textContent = 'error ' + error; });
            </script>""";

    @AfterMethod(alwaysRun = true)
    public void clear() {
        BrowserObservabilityRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test(groups = "trace-viewer-browser-acceptance")
    public void binaryMultipartUploadShouldReachTheServerUnchangedWhileTraceNetworkCaptureIsOn() throws Exception {
        String chrome = System.getProperty("shaft.trace.viewer.chrome", "/usr/bin/google-chrome");
        if (!Files.isExecutable(Path.of(chrome))) {
            throw new SkipException("Chrome is required for the upload acceptance test: " + chrome);
        }
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> respond(exchange, "text/html", PAGE.getBytes(StandardCharsets.UTF_8)));
        server.createContext("/upload", exchange -> respond(exchange, "text/plain",
                sha256(filePart(exchange)).getBytes(StandardCharsets.UTF_8)));
        server.start();
        ChromeOptions options = new ChromeOptions().setBinary(chrome)
                .addArguments("--headless=new", "--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage");
        ChromeDriver driver = new ChromeDriver(options);
        BrowserNetworkInterceptor interceptor = new BrowserNetworkInterceptor(driver);
        try {
            Assert.assertTrue(interceptor.startObserving(), "Passive CDP network observation must start on Chrome.");
            driver.get("http://127.0.0.1:" + server.getAddress().getPort() + "/");
            new WebDriverWait(driver, Duration.ofSeconds(20)).until(d ->
                    !"pending".equals(d.findElement(org.openqa.selenium.By.id("result")).getText()));
            Assert.assertEquals(driver.findElement(org.openqa.selenium.By.id("result")).getText(), "match",
                    "The server must receive the uploaded file byte for byte while trace network capture is on.");

            BrowserObservabilityRecorder.NetworkSnapshotEntry upload = awaitUpload();
            Assert.assertEquals(upload.method(), "POST");
            Assert.assertEquals(upload.status(), 200);
            Assert.assertTrue(upload.requestSizeBytes() > 1024, "The trace must keep the upload size.");
            Assert.assertTrue(upload.requestBodyPreview().startsWith("[binary body: "),
                    "Binary request bodies must be marked, not decoded: " + upload.requestBodyPreview());
            Assert.assertEquals(upload.bodyPreview().length(), 64, "Textual response bodies keep a preview.");

            interceptor.addRule(BrowserNetworkInterceptionRule.mock(
                    request -> request.getUri().contains("/unrelated"), request -> new HttpResponse().setStatus(204)));
            interceptor.clear();
            driver.navigate().refresh();
            new WebDriverWait(driver, Duration.ofSeconds(20)).until(d ->
                    !"pending".equals(d.findElement(org.openqa.selenium.By.id("result")).getText()));
            Assert.assertEquals(driver.findElement(org.openqa.selenium.By.id("result")).getText(), "match",
                    "Clearing rules must return to passive capture without pausing uploads.");
        } finally {
            interceptor.close();
            driver.quit();
            server.stop(0);
        }
    }

    private static BrowserObservabilityRecorder.NetworkSnapshotEntry awaitUpload() throws InterruptedException {
        long deadline = System.nanoTime() + Duration.ofSeconds(10).toNanos();
        while (System.nanoTime() < deadline) {
            for (BrowserObservabilityRecorder.NetworkSnapshotEntry event : BrowserObservabilityRecorder.snapshot()) {
                if (event.url().endsWith("/upload")) {
                    return event;
                }
            }
            Thread.sleep(100);
        }
        throw new AssertionError("The upload request was not recorded: " + BrowserObservabilityRecorder.snapshot());
    }

    private static byte[] filePart(HttpExchange exchange) throws IOException {
        String type = exchange.getRequestHeaders().getFirst("Content-Type");
        byte[] body = exchange.getRequestBody().readAllBytes();
        byte[] boundary = ("\r\n--" + type.substring(type.indexOf("boundary=") + 9)).getBytes(StandardCharsets.ISO_8859_1);
        int part = indexOf(body, "filename=\"pixel.png\"".getBytes(StandardCharsets.ISO_8859_1), 0);
        int start = indexOf(body, "\r\n\r\n".getBytes(StandardCharsets.ISO_8859_1), part) + 4;
        int end = indexOf(body, boundary, start);
        return part < 0 || end < 0 ? new byte[0] : java.util.Arrays.copyOfRange(body, start, end);
    }

    private static int indexOf(byte[] data, byte[] needle, int from) {
        outer:
        for (int i = Math.max(0, from); i <= data.length - needle.length; i++) {
            for (int j = 0; j < needle.length; j++) {
                if (data[i + j] != needle[j]) {
                    continue outer;
                }
            }
            return i;
        }
        return -1;
    }

    private static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (java.security.NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private static void respond(HttpExchange exchange, String type, byte[] body) throws IOException {
        exchange.getResponseHeaders().add("Content-Type", type);
        exchange.sendResponseHeaders(200, body.length);
        exchange.getResponseBody().write(body);
        exchange.close();
    }
}
