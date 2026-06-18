package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.driver.SHAFT;
import com.shaft.gui.internal.healing.HealingActionOutcome;
import com.shaft.gui.internal.healing.HealingExplanation;
import com.shaft.gui.internal.healing.HealingManager;
import com.shaft.gui.internal.healing.HealingObservation;
import com.shaft.gui.internal.healing.HealingRequest;
import com.shaft.gui.internal.healing.HealingResolution;
import com.shaft.heal.ShaftHeal;
import com.shaft.heal.model.HealingDecision;
import com.shaft.heal.model.HealingPlatform;
import com.shaft.pilot.json.JsonSchemaValidator;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.android.AndroidDriver;
import io.appium.java_client.ios.IOSDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.MutableCapabilities;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

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
        ObjectMapper json = new ObjectMapper();
        var reportNode = json.valueToTree(ShaftHeal.lastReport().orElseThrow());
        String report = json.writeValueAsString(reportNode);
        Assert.assertFalse(history.contains(SECRET_CANARY));
        Assert.assertFalse(report.contains(SECRET_CANARY));
        try (var schemaStream = getClass().getResourceAsStream(
                "/schema/shaft-heal-report-2.0.schema.json")) {
            Assert.assertNotNull(schemaStream);
            Assert.assertEquals(
                    JsonSchemaValidator.validate(json.readTree(schemaStream), reportNode),
                    List.of());
        }
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
    public void acceptedRecoveryShouldExposeWarningExplanationMetadata() {
        WebDriver driver = driver();
        WebElement original = element("old-id", "Username");
        WebElement candidate = element("new-id", "Username");
        configureSearch(driver, List.of(candidate));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-id");
        provider.observe(new HealingObservation(driver, originalLocator, original, "CLICK", null, null, null));

        HealingResolution resolution = provider.resolve(new HealingRequest(
                        driver, originalLocator, "CLICK", true, null, null, null))
                .orElseThrow();
        HealingExplanation explanation = provider.explain(resolution.attemptId()).orElseThrow();

        Assert.assertEquals(explanation.originalLocator(), originalLocator);
        Assert.assertEquals(explanation.healedLocator(), resolution.selectedLocator());
        Assert.assertTrue(explanation.confidence() >= 0.75);
        Assert.assertEquals(explanation.threshold(), 0.75);
        Assert.assertFalse(explanation.evidence().isEmpty());
        Assert.assertEquals(explanation.providerStatus(), "deterministic");
        Assert.assertTrue(explanation.reason().contains("unique candidate"));
    }

    @Test
    public void enabledAiShouldNotRunForDeterministicRecoveryByDefault() {
        SHAFT.Properties.healing.set()
                .aiEnabled(true)
                .aiTrigger("ambiguous");
        WebDriver driver = driver();
        WebElement original = element("old-id", "Username");
        WebElement candidate = element("new-id", "Username");
        configureSearch(driver, List.of(candidate));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-id");
        provider.observe(new HealingObservation(driver, originalLocator, original, "CLICK", null, null, null));

        HealingResolution resolution = provider.resolve(new HealingRequest(
                        driver, originalLocator, "CLICK", true, null, null, null))
                .orElseThrow();

        Assert.assertEquals(ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.RECOVERED);
        Assert.assertEquals(ShaftHeal.lastReport().orElseThrow().provider().status(), "SKIPPED");
        Assert.assertTrue(ShaftHeal.lastReport().orElseThrow().provider().fallbackReason()
                .contains("not triggered"));
        provider.clear(driver);
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

    @Test(dataProvider = "nativePlatforms")
    public void nativeAccessibilityChangesShouldRecoverDeterministically(
            String platformName,
            HealingPlatform expectedPlatform) {
        AppiumDriver driver = nativeDriver(platformName);
        WebElement original = nativeElement("old-login", "Sign in");
        WebElement candidate = nativeElement("new-login", "Sign in");
        configureNativeSearch(driver, List.of(candidate));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-login");
        provider.observe(new HealingObservation(
                driver, originalLocator, original, "CLICK", null, null, null));

        Optional<HealingResolution> resolved = provider.resolve(new HealingRequest(
                driver, originalLocator, "CLICK", true, null, null, null));
        Assert.assertTrue(resolved.isPresent(), String.valueOf(ShaftHeal.lastReport().orElse(null)));
        HealingResolution resolution = resolved.orElseThrow();

        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().contextMetadata().platform(),
                expectedPlatform);
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().candidates().getFirst().fingerprint().platform(),
                expectedPlatform);
        Assert.assertEquals(resolution.elements(), List.of(candidate));
    }

    @Test
    public void duplicateAndDisabledNativeCandidatesShouldFailSafely() {
        AppiumDriver driver = nativeDriver("Android");
        WebElement original = nativeElement("old-login", "Sign in");
        WebElement first = nativeElement("new-login-a", "Sign in");
        WebElement second = nativeElement("new-login-b", "Sign in");
        configureNativeSearch(driver, List.of(first, second));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-login");
        provider.observe(new HealingObservation(
                driver, originalLocator, original, "CLICK", null, null, null));

        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, originalLocator, "CLICK", true, null, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.REJECTED_PRECONDITION);

        WebElement disabled = nativeElement("new-login", "Sign in");
        when(disabled.isEnabled()).thenReturn(false);
        configureNativeSearch(driver, List.of(disabled));
        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, originalLocator, "CLICK", true, null, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.REJECTED_PRECONDITION);
    }

    @Test
    public void wrongNativeScreenShouldNotReuseHistory() {
        AndroidDriver driver = (AndroidDriver) nativeDriver("Android");
        when(driver.currentActivity()).thenReturn(
                ".LoginActivity",
                ".LoginActivity",
                ".SettingsActivity",
                ".SettingsActivity");
        WebElement original = nativeElement("old-login", "Sign in");
        configureNativeSearch(driver, List.of(nativeElement("new-login", "Sign in")));
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-login");
        provider.observe(new HealingObservation(
                driver, originalLocator, original, "CLICK", null, null, null));

        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, originalLocator, "CLICK", true, null, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.NO_HISTORY);
    }

    @Test
    public void iosStructuralScreenChangeShouldNotReuseHistory() {
        AppiumDriver driver = nativeDriver("iOS");
        WebElement loginRoot = nativeElement("login-root", "Login screen");
        WebElement settingsRoot = nativeElement("settings-root", "Settings screen");
        WebElement candidate = nativeElement("new-login", "Sign in");
        AtomicInteger rootLookups = new AtomicInteger();
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> {
            String locator = invocation.getArgument(0).toString();
            if ("By.xpath: /*".equals(locator)) {
                return rootLookups.getAndIncrement() < 3
                        ? List.of(loginRoot)
                        : List.of(settingsRoot);
            }
            return List.of(candidate);
        });
        ShaftHealingProvider provider = new ShaftHealingProvider();
        By originalLocator = By.id("old-login");
        provider.observe(new HealingObservation(
                driver,
                originalLocator,
                nativeElement("old-login", "Sign in"),
                "CLICK",
                null,
                null,
                null));

        Assert.assertTrue(provider.resolve(new HealingRequest(
                driver, originalLocator, "CLICK", true, null, null, null)).isEmpty());
        Assert.assertEquals(
                ShaftHeal.lastReport().orElseThrow().decision().status(),
                HealingDecision.Status.NO_HISTORY);
        Assert.assertTrue(
                ShaftHeal.lastReport().orElseThrow().contextMetadata().screenId()
                        .startsWith("root-"));
    }

    @DataProvider
    public Object[][] nativePlatforms() {
        return new Object[][]{
                {"Android", HealingPlatform.ANDROID},
                {"iOS", HealingPlatform.IOS}
        };
    }

    private WebDriver driver() {
        WebDriver driver = mock(WebDriver.class,
                org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebDriver.TargetLocator targetLocator = mock(WebDriver.TargetLocator.class);
        when(driver.switchTo()).thenReturn(targetLocator);
        when(targetLocator.defaultContent()).thenReturn(driver);
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

    private AppiumDriver nativeDriver(String platformName) {
        MutableCapabilities capabilities = new MutableCapabilities();
        capabilities.setCapability("platformName", platformName);
        capabilities.setCapability(
                "appium:" + ("Android".equals(platformName) ? "appPackage" : "bundleId"),
                "com.example.app");
        AppiumDriver driver;
        if ("Android".equals(platformName)) {
            AndroidDriver android = mock(AndroidDriver.class);
            when(android.getCurrentPackage()).thenReturn("com.example.app");
            when(android.currentActivity()).thenReturn(".LoginActivity");
            driver = android;
        } else {
            driver = mock(IOSDriver.class);
        }
        when(driver.getCapabilities()).thenReturn(capabilities);
        when(driver.getWindowHandle()).thenReturn("window-1");
        return driver;
    }

    private void configureNativeSearch(AppiumDriver driver, List<WebElement> candidates) {
        WebElement root = nativeElement("root", "Login screen");
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> {
            String locator = invocation.getArgument(0).toString();
            if ("By.xpath: /*".equals(locator)) {
                return List.of(root);
            }
            return candidates;
        });
    }

    private WebElement nativeElement(String resourceId, String name) {
        WebElement parent = mock(WebElement.class);
        when(parent.getTagName()).thenReturn("container");
        when(parent.getAttribute(anyString())).thenReturn("");
        WebElement element = mock(WebElement.class);
        when(element.getTagName()).thenReturn("button");
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.findElement(any(By.class))).thenReturn(parent);
        when(parent.findElement(any(By.class)))
                .thenThrow(new org.openqa.selenium.NoSuchElementException("root"));
        when(element.getAttribute(anyString())).thenAnswer(invocation -> switch ((String) invocation.getArgument(0)) {
            case "content-desc", "label", "name", "accessibility id", "text" -> name;
            case "resource-id", "id" -> resourceId;
            case "class", "type", "role" -> "button";
            default -> "";
        });
        return element;
    }
}
