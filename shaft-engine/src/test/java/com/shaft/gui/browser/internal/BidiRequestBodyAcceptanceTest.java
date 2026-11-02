package com.shaft.gui.browser.internal;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.BrowserObservabilityRecorder;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.openqa.selenium.By;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
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
import java.time.Duration;

/**
 * Issue #6740: the BiDi network provider keeps a bounded, redacted request body preview, read through
 * a BiDi data collector in a real Chrome session.
 */
public class BidiRequestBodyAcceptanceTest {
    private static final String PAGE = """
            <!doctype html><title>login</title><p id="result">pending</p>
            <script>
            fetch('/login', {method: 'POST', headers: {'Content-Type': 'application/json'},
                body: JSON.stringify({user: 'alice', password: 'hunter2'})})
              .then(r => r.text()).then(t => { document.getElementById('result').textContent = t; })
              .catch(e => { document.getElementById('result').textContent = 'error ' + e; });
            </script>""";

    @AfterMethod(alwaysRun = true)
    public void clear() {
        BrowserObservabilityRecorder.clear();
        Properties.clearForCurrentThread();
    }

    @Test(groups = "trace-viewer-browser-acceptance")
    public void bidiPostBodyShouldReachTheTraceWithSensitiveFieldsMasked() throws Exception {
        String chrome = System.getProperty("shaft.trace.viewer.chrome", "/usr/bin/google-chrome");
        if (!Files.isExecutable(Path.of(chrome))) {
            throw new SkipException("Chrome is required for the BiDi request body acceptance test: " + chrome);
        }
        SHAFT.Properties.reporting.set().traceEnabled(true).traceIncludeNetwork(true);
        BrowserObservabilityRecorder.ObservationSession owner = BrowserObservabilityRecorder.startSession();
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/", exchange -> respond(exchange, "text/html", PAGE));
        server.createContext("/login", exchange -> respond(exchange, "text/plain", "welcome"));
        server.start();
        ChromeOptions options = new ChromeOptions().setBinary(chrome)
                .addArguments("--headless=new", "--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage");
        options.setCapability("webSocketUrl", true);
        ChromeDriver driver = new ChromeDriver(options);
        BidiNetworkActivitySource source = new BidiNetworkActivitySource(driver, System::nanoTime);
        try {
            Assert.assertTrue(source.healthy(), "The BiDi network source must attach to Chrome.");
            driver.get("http://127.0.0.1:" + server.getAddress().getPort() + "/");
            new WebDriverWait(driver, Duration.ofSeconds(20)).until(d ->
                    !"pending".equals(d.findElement(By.id("result")).getText()));
            Assert.assertEquals(driver.findElement(By.id("result")).getText(), "welcome");

            BrowserObservabilityRecorder.NetworkSnapshotEntry login = awaitLogin(owner);
            Assert.assertEquals(login.method(), "POST");
            Assert.assertTrue(login.requestBodyPreview().contains("alice"),
                    "The BiDi provider must keep the POST body preview: " + login.requestBodyPreview());
            Assert.assertFalse(login.requestBodyPreview().contains("hunter2"),
                    "Sensitive fields must be masked: " + login.requestBodyPreview());
        } finally {
            source.close();
            driver.quit();
            server.stop(0);
        }
    }

    private static BrowserObservabilityRecorder.NetworkSnapshotEntry awaitLogin(
            BrowserObservabilityRecorder.ObservationSession owner) throws InterruptedException {
        long deadline = System.nanoTime() + Duration.ofSeconds(10).toNanos();
        while (System.nanoTime() < deadline) {
            for (BrowserObservabilityRecorder.NetworkSnapshotEntry event : BrowserObservabilityRecorder.snapshot(owner)) {
                if (event.url().endsWith("/login")) {
                    return event;
                }
            }
            Thread.sleep(50);
        }
        throw new AssertionError("No BiDi network event was recorded for /login.");
    }

    private static void respond(HttpExchange exchange, String type, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", type);
        exchange.sendResponseHeaders(200, bytes.length);
        exchange.getResponseBody().write(bytes);
        exchange.close();
    }
}
