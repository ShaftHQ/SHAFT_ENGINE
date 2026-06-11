package com.shaft.capture.runtime;

import com.shaft.capture.format.CaptureJsonCodec;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.capture.model.Checkpoint;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.openqa.selenium.Alert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.Select;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Tag("external-e2e")
class ManagedCaptureRecorderBrowserTest {
    private static final String SECRET_CANARY = "capture-browser-secret-canary";

    @ParameterizedTest
    @ValueSource(strings = {"chrome", "edge"})
    void recordsRepresentativeJourneyAcrossSupportedBrowsers(
            String browserName,
            @TempDir Path temp) throws Exception {
        HttpServer server = localFixture();
        Path output = temp.resolve("recordings with spaces").resolve(browserName + ".json");
        Path runtime = temp.resolve("runtime with spaces").resolve(browserName);
        Path upload = temp.resolve("upload fixture.txt");
        Files.writeString(upload, "fixture", StandardCharsets.UTF_8);
        ManagedCaptureRecorder recorder = new ManagedCaptureRecorder(new CaptureStartRequest(
                "http://127.0.0.1:" + server.getAddress().getPort() + "/",
                CaptureBrowser.parse(browserName),
                output,
                runtime,
                true));
        try {
            recorder.start();
            WebDriver driver = recorder.driverForTesting();
            driver.findElement(By.id("username")).sendKeys("alice");
            driver.findElement(By.id("password")).sendKeys(SECRET_CANARY);
            new Select(driver.findElement(By.id("country"))).selectByVisibleText("Egypt");
            driver.findElement(By.id("terms")).click();
            driver.findElement(By.id("upload")).sendKeys(upload.toAbsolutePath().toString());
            new Actions(driver).doubleClick(driver.findElement(By.id("double"))).perform();
            driver.findElement(By.id("replace")).click();

            WebElement shadowHost = driver.findElement(By.id("shadow-host"));
            shadowHost.getShadowRoot().findElement(By.cssSelector("button")).click();

            driver.switchTo().frame(driver.findElement(By.id("child")));
            driver.findElement(By.id("frame-button")).click();
            driver.switchTo().defaultContent();

            String originalWindow = driver.getWindowHandle();
            driver.findElement(By.id("popup")).click();
            waitFor(() -> driver.getWindowHandles().size() == 2);
            String popup = driver.getWindowHandles().stream()
                    .filter(handle -> !handle.equals(originalWindow))
                    .findFirst()
                    .orElseThrow();
            driver.switchTo().window(popup);
            driver.close();
            driver.switchTo().window(originalWindow);

            driver.findElement(By.id("prompt")).click();
            Alert alert = driver.switchTo().alert();
            alert.sendKeys("ordinary prompt text");
            alert.accept();
            recorder.checkpoint("Representative journey completed", Checkpoint.CheckpointKind.ASSERTION);
            Thread.sleep(1500);
            recorder.stop(false);
        } finally {
            if (recorder.status().state() == CaptureStatus.State.ACTIVE) {
                recorder.interrupt();
            }
            server.stop(0);
        }

        CaptureSession session = new CaptureJsonCodec().read(output);
        Set<Class<?>> eventTypes = session.events().stream()
                .map(Object::getClass)
                .collect(java.util.stream.Collectors.toSet());
        assertTrue(eventTypes.contains(CaptureEvent.NavigationEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.TypeEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.SelectEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.ToggleEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.UploadEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.FrameEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.WindowEvent.class));
        assertTrue(eventTypes.contains(CaptureEvent.AlertEvent.class));
        assertFalse(Files.readString(output, StandardCharsets.UTF_8).contains(SECRET_CANARY));
        assertFalse(Files.readString(output.getParent().resolve("capture-data.json"),
                StandardCharsets.UTF_8).contains(SECRET_CANARY));
        Path profiles = runtime.resolve("profiles");
        if (Files.exists(profiles)) {
            try (var entries = Files.list(profiles)) {
                assertTrue(entries.findAny().isEmpty());
            }
        }
    }

    private static HttpServer localFixture() throws IOException {
        HttpServer server = HttpServer.create(
                new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 0), 0);
        server.createContext("/", exchange -> respond(exchange, """
                <!doctype html>
                <html>
                <head><title>SHAFT Capture Fixture</title></head>
                <body>
                  <label>Username <input id="username" name="username"></label>
                  <label>Password <input id="password" name="password" type="password"></label>
                  <label>Country
                    <select id="country" name="country">
                      <option>United States</option><option>Egypt</option>
                    </select>
                  </label>
                  <label><input id="terms" name="terms" type="checkbox"> Terms</label>
                  <input id="upload" name="upload" type="file">
                  <button id="double">Double</button>
                  <button id="replace" onclick="this.outerHTML='<button id=&quot;replacement&quot;>Replacement</button>'">
                    Replace dynamically
                  </button>
                  <div id="shadow-host"></div>
                  <iframe id="child" src="/frame"></iframe>
                  <button id="popup" onclick="window.open('/popup', '_blank')">Popup</button>
                  <button id="prompt" onclick="prompt('Prompt value')">Prompt</button>
                  <script>
                    const root = document.querySelector('#shadow-host').attachShadow({mode: 'open'});
                    root.innerHTML = '<button id="shadow-button">Shadow</button>';
                  </script>
                </body>
                </html>
                """));
        server.createContext("/frame", exchange -> respond(exchange,
                "<!doctype html><button id=\"frame-button\">Frame</button>"));
        server.createContext("/popup", exchange -> respond(exchange,
                "<!doctype html><title>Popup</title><p>Popup</p>"));
        server.start();
        return server;
    }

    private static void respond(HttpExchange exchange, String body) throws IOException {
        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "text/html; charset=utf-8");
        exchange.sendResponseHeaders(200, bytes.length);
        try (exchange) {
            exchange.getResponseBody().write(bytes);
        }
    }

    private static void waitFor(java.util.function.BooleanSupplier condition) throws InterruptedException {
        InstantDeadline deadline = new InstantDeadline(Duration.ofSeconds(5));
        while (!condition.getAsBoolean() && !deadline.expired()) {
            Thread.sleep(50);
        }
        assertTrue(condition.getAsBoolean());
    }

    private record InstantDeadline(long deadlineNanos) {
        private InstantDeadline(Duration duration) {
            this(System.nanoTime() + duration.toNanos());
        }

        private boolean expired() {
            return System.nanoTime() >= deadlineNanos;
        }
    }
}
