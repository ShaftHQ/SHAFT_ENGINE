package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.healing.HealingActionOutcome;
import com.shaft.gui.internal.healing.HealingManager;
import com.shaft.gui.internal.healing.HealingObservation;
import com.shaft.gui.internal.healing.HealingRequest;
import com.shaft.gui.internal.healing.HealingResolution;
import com.shaft.heal.ShaftHeal;
import com.shaft.heal.model.HealingDecision;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ShaftHealingProviderTest {
    private static final String SECRET_CANARY = "secret-canary-123456789012345678901234567890";
    private final Path directory = Path.of("target", "shaft-heal-tests", "provider").toAbsolutePath();

    @BeforeMethod
    public void configure() throws IOException {
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
        ShaftHeal.clear();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() throws IOException {
        com.shaft.properties.internal.Properties.clearForCurrentThread();
        ShaftHeal.clear();
        if (Files.exists(directory)) {
            try (var paths = Files.walk(directory)) {
                paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException ignored) {
                        // Best-effort isolated test cleanup.
                    }
                });
            }
        }
    }

    @Test
    public void changedIdShouldRecoverAndReportCompletedActionWithoutPersistingValues() throws Exception {
        WebDriver driver = driver();
        WebElement original = element("old-id", "Username");
        WebElement candidate = element("new-id", "Username");
        configureSearch(driver, List.of(candidate));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-id");
        HealingObservation observation = new HealingObservation(
                driver, originalLocator, original, "TYPE", null, null, null);
        provider.observe(observation);

        Optional<HealingResolution> resolution = provider.resolve(new HealingRequest(
                driver, originalLocator, "TYPE", true, null, null, null));

        Assert.assertTrue(resolution.isPresent());
        Assert.assertTrue(resolution.get().selectedLocator().toString().contains("new-id-test"));
        provider.recordOutcome(new HealingActionOutcome(
                driver,
                resolution.get().attemptId(),
                originalLocator,
                resolution.get().selectedLocator(),
                "TYPE",
                true,
                "UNVERIFIABLE",
                ""));
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().action().outcome(),
                "PASSED");
        String history = Files.readString(directory.resolve("history.json"));
        String report = new ObjectMapper().writeValueAsString(ShaftHeal.lastReport().orElseThrow());
        Assert.assertFalse(history.contains(SECRET_CANARY));
        Assert.assertFalse(report.contains(SECRET_CANARY));
    }

    @Test
    public void serviceLoadedProviderShouldRetainPendingOutcomeState() {
        WebDriver driver = driver();
        WebElement original = element("old-id", "Username");
        WebElement candidate = element("new-id", "Username");
        configureSearch(driver, List.of(candidate));
        By originalLocator = By.id("old-id");
        new ShaftHealingProvider().observe(new HealingObservation(
                driver, originalLocator, original, "CLICK", null, null, null));

        HealingResolution resolution = HealingManager.resolve(
                        driver,
                        originalLocator,
                        "CLICK",
                        true,
                        null,
                        null,
                        null)
                .orElseThrow();
        HealingManager.recordOutcome(driver, resolution, originalLocator, "CLICK", true, "");

        Assert.assertEquals(ShaftHeal.lastReport().orElseThrow().action().outcome(), "PASSED");
        HealingManager.clear(driver);
    }

    @Test
    public void closingDifferentDriverShouldNotDiscardPendingRecovery() {
        WebDriver driver = driver();
        WebDriver otherDriver = driver();
        WebElement original = element("old-id", "Username");
        WebElement candidate = element("new-id", "Username");
        configureSearch(driver, List.of(candidate));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-id");
        provider.observe(new HealingObservation(driver, originalLocator, original, "CLICK", null, null, null));
        HealingResolution resolution = provider.resolve(new HealingRequest(
                        driver, originalLocator, "CLICK", true, null, null, null))
                .orElseThrow();

        provider.clear(otherDriver);
        provider.recordOutcome(new HealingActionOutcome(
                driver,
                resolution.attemptId(),
                originalLocator,
                resolution.selectedLocator(),
                "CLICK",
                true,
                "UNVERIFIABLE",
                ""));

        Assert.assertEquals(ShaftHeal.lastReport().orElseThrow().action().outcome(), "PASSED");
    }

    @Test
    public void missingShadowContextShouldRejectLightDomCandidates() {
        WebDriver driver = driver();
        WebElement original = element("old-id", "Username");
        By originalLocator = By.id("old-id");
        By shadowHost = By.id("shadow-host");
        ShaftHealingProvider provider = new ShaftHealingProvider();
        provider.observe(new HealingObservation(
                driver, originalLocator, original, "CLICK", null, shadowHost, By.cssSelector("input")));

        Optional<HealingResolution> resolution = provider.resolve(new HealingRequest(
                driver,
                originalLocator,
                "CLICK",
                true,
                null,
                shadowHost,
                By.cssSelector("input")));

        Assert.assertTrue(resolution.isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.NO_CANDIDATES);
    }

    @Test
    public void duplicateSemanticCandidatesShouldFailSafely() {
        WebDriver driver = driver();
        WebElement original = element("old-id", "Username");
        WebElement first = element("new-a", "Username");
        WebElement second = element("new-b", "Username");
        configureSearch(driver, List.of(first, second));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-id");
        provider.observe(new HealingObservation(driver, originalLocator, original, "CLICK", null, null, null));

        Optional<HealingResolution> resolution = provider.resolve(new HealingRequest(
                driver, originalLocator, "CLICK", true, null, null, null));

        Assert.assertTrue(resolution.isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.AMBIGUOUS);
    }

    @Test
    public void missingHistoryShouldPreserveOriginalFailure() {
        ShaftHealingProvider provider = new ShaftHealingProvider();

        Optional<HealingResolution> resolution = provider.resolve(new HealingRequest(
                driver(), By.id("unknown"), "CLICK", true, null, null, null));

        Assert.assertTrue(resolution.isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.NO_HISTORY);
    }

    private WebDriver driver() {
        WebDriver driver = mock(WebDriver.class,
                org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        when(driver.getCurrentUrl()).thenReturn("https://example.test/form?token=" + SECRET_CANARY);
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class)))
                .thenAnswer(invocation -> {
                    String script = invocation.getArgument(0);
                    return script.contains("element.labels") ? "Username" : "html:nth-of-type(1)>body:nth-of-type(1)>input:nth-of-type(1)";
                });
        return driver;
    }

    private WebElement element(String id, String accessibleName) {
        WebElement element = mock(WebElement.class);
        when(element.getTagName()).thenReturn("input");
        when(element.getAccessibleName()).thenReturn(accessibleName);
        when(element.getText()).thenReturn("");
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(new byte[]{1, 2, 3});
        when(element.getDomAttribute(anyString())).thenAnswer(invocation -> switch ((String) invocation.getArgument(0)) {
            case "id" -> id;
            case "name" -> "username";
            case "role" -> "textbox";
            case "type" -> "text";
            case "placeholder" -> "Username";
            case "aria-label" -> accessibleName;
            case "data-testid" -> id + "-test";
            case "value" -> SECRET_CANARY;
            default -> "";
        });
        when(element.getDomProperty("value")).thenReturn(SECRET_CANARY);
        return element;
    }

    private void configureSearch(WebDriver driver, List<WebElement> candidates) {
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> {
            String locator = invocation.getArgument(0).toString();
            if (locator.contains("tagName: input")) {
                return candidates;
            }
            for (WebElement candidate : candidates) {
                String id = candidate.getDomAttribute("id");
                if (locator.contains(id)) {
                    return List.of(candidate);
                }
            }
            return List.of();
        });
    }
}
