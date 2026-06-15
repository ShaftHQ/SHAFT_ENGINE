package com.shaft.heal.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.healing.HealingObservation;
import com.shaft.gui.internal.healing.HealingRequest;
import com.shaft.gui.internal.healing.HealingResolution;
import com.shaft.heal.ShaftHeal;
import com.shaft.heal.model.HealingDecision;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Optional;

public class WebDomHealingAcceptanceTest {
    private final Path directory = Path.of(
            "target", "shaft-heal-tests", "browser-acceptance").toAbsolutePath();
    private WebDriver driver;

    @BeforeMethod
    public void setup() throws IOException {
        Files.createDirectories(directory);
        SHAFT.Properties.healing.set()
                .strategy("shaft-heal")
                .minimumConfidence(0.75)
                .ambiguityMargin(0.10)
                .testIdAttributes("data-testid")
                .historyPath(directory.resolve("history.json").toString())
                .historyEnabled(true)
                .visualEnabled(false)
                .aiEnabled(false);
        ChromeOptions options = new ChromeOptions();
        options.addArguments("--headless=new", "--disable-gpu", "--no-sandbox", "--window-size=1280,900");
        driver = new ChromeDriver(options);
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws IOException {
        if (driver != null) {
            driver.quit();
            driver = null;
        }
        com.shaft.properties.internal.Properties.clearForCurrentThread();
        ShaftHeal.clear();
        if (Files.exists(directory)) {
            try (var paths = Files.walk(directory)) {
                paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException ignored) {
                        // Best-effort isolated fixture cleanup.
                    }
                });
            }
        }
    }

    @Test
    public void deterministicWebContextMatrixShouldRecoverOrFailClosed() throws Exception {
        ShaftHealingProvider provider = new ShaftHealingProvider();
        Path page = directory.resolve("fixture.html");

        write(page, """
                <html><body>
                  <label for="old-user">Username</label>
                  <input id="old-user" name="username" data-testid="username" aria-label="Username">
                </body></html>
                """);
        open(page);
        By oldUser = By.id("old-user");
        provider.observe(new HealingObservation(
                driver, oldUser, driver.findElement(oldUser), "TYPE", null, null, null));
        write(page, """
                <html><body>
                  <section><label for="new-user">Username</label>
                  <input id="new-user" name="username" data-testid="username" aria-label="Username"></section>
                </body></html>
                """);
        driver.navigate().refresh();

        HealingResolution changedId = provider.resolve(new HealingRequest(
                driver, oldUser, "TYPE", true, null, null, null)).orElseThrow();

        Assert.assertTrue(changedId.selectedLocator().toString().contains("username"));

        write(page, """
                <html><body>
                  <input id="duplicate-a" name="username" data-testid="username" aria-label="Username">
                  <input id="duplicate-b" name="username" data-testid="username" aria-label="Username">
                </body></html>
                """);
        driver.navigate().refresh();

        Optional<HealingResolution> ambiguous = provider.resolve(new HealingRequest(
                driver, oldUser, "TYPE", true, null, null, null));

        Assert.assertTrue(ambiguous.isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.AMBIGUOUS);

        write(page, """
                <html><body>
                  <input id="hidden-user" name="username" data-testid="username"
                    aria-label="Username" style="display:none">
                </body></html>
                """);
        driver.navigate().refresh();
        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, oldUser, "TYPE", true, null, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.REJECTED_PRECONDITION);

        write(page, """
                <html><body>
                  <input id="disabled-user" name="username" data-testid="username"
                    aria-label="Username" disabled>
                </body></html>
                """);
        driver.navigate().refresh();
        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, oldUser, "TYPE", true, null, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.REJECTED_PRECONDITION);

        By frame = By.id("stable-frame");
        By oldFrameButton = By.id("old-frame-button");
        write(page, framePage("old-frame-button"));
        driver.navigate().refresh();
        driver.switchTo().frame(driver.findElement(frame));
        provider.observe(new HealingObservation(
                driver,
                oldFrameButton,
                driver.findElement(oldFrameButton),
                "CLICK",
                frame,
                null,
                null));
        write(page, framePage("new-frame-button"));
        driver.navigate().refresh();

        HealingResolution frameResolution = provider.resolve(new HealingRequest(
                driver, oldFrameButton, "CLICK", true, frame, null, null)).orElseThrow();

        Assert.assertTrue(frameResolution.selectedLocator().toString().contains("frame-action"));

        write(page, framePage("newer-frame-button").replace("stable-frame", "changed-frame"));
        driver.navigate().refresh();
        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, oldFrameButton, "CLICK", true, frame, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.NO_CANDIDATES);

        By host = By.id("stable-host");
        By oldShadowButton = By.cssSelector("#old-shadow-button");
        write(page, shadowPage("old-shadow-button"));
        driver.navigate().refresh();
        var shadowElement = driver.findElement(host).getShadowRoot().findElement(oldShadowButton);
        provider.observe(new HealingObservation(
                driver, oldShadowButton, shadowElement, "CLICK", null, host, oldShadowButton));
        write(page, shadowPage("new-shadow-button"));
        driver.navigate().refresh();

        HealingResolution shadowResolution = provider.resolve(new HealingRequest(
                driver, oldShadowButton, "CLICK", true, null, host, oldShadowButton)).orElseThrow();

        Assert.assertTrue(shadowResolution.selectedLocator().toString().contains("shadow-action"));

        write(page, shadowPage("newer-shadow-button").replace("stable-host", "changed-host"));
        driver.navigate().refresh();
        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, oldShadowButton, "CLICK", true, null, host, oldShadowButton)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.NO_CANDIDATES);

        write(page, "<html><body><h1>Wrong page</h1></body></html>");
        driver.navigate().refresh();
        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, oldUser, "TYPE", true, null, null, null)).isEmpty());
    }

    private void open(Path page) {
        driver.get(page.toUri().toString());
    }

    private static String framePage(String id) {
        return """
                <html><body>
                  <iframe id="stable-frame" srcdoc='<button id="%s" data-testid="frame-action"
                    aria-label="Frame action">Frame action</button>'></iframe>
                </body></html>
                """.formatted(id);
    }

    private static String shadowPage(String id) {
        return """
                <html><body><div id="stable-host"></div>
                <script>
                  const root = document.querySelector('#stable-host').attachShadow({mode: 'open'});
                  root.innerHTML = '<button id="%s" data-testid="shadow-action" aria-label="Shadow action">Shadow action</button>';
                </script></body></html>
                """.formatted(id);
    }

    private static void write(Path page, String html) throws IOException {
        Files.writeString(page, html, StandardCharsets.UTF_8);
    }
}
