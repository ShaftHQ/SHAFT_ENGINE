package com.shaft.gui.internal.natural;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class NaturalActionExecutorTest {
    @BeforeMethod
    public void configureFastActionProperties() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(true);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.LINUX.name());
        SHAFT.Properties.mobile.set().browserName("chrome");
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
        Properties.clearForCurrentThread();
    }

    @Test
    public void defaultDisabledNaturalActionsShouldFailBeforeUsingDriver() {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);

        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> NaturalActionExecutor.perform(helper, "refresh page"));

        Assert.assertTrue(exception.getMessage().contains("Natural actions are disabled"));
        verify(helper, never()).getDriver();
    }

    @Test
    public void enabledLoginIntentShouldValidateAndExecuteDeterministicSteps() {
        SHAFT.Properties.naturalActions.set().enabled(true).minimumTrustPercentage(85);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        WebDriver driver = driver();
        WebElement username = element();
        WebElement password = element();
        WebElement button = element();
        when(helper.getDriver()).thenReturn(driver);
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> elementsForLocator(
                invocation.getArgument(0),
                username,
                password,
                button));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            NaturalActionExecutor.perform(
                    helper,
                    "Login with valid credentials",
                    "user@example.test",
                    "correct-password");
        }

        verify(username).sendKeys("user@example.test");
        verify(password).sendKeys("correct-password");
        verify(button).click();
    }

    @Test
    public void minimumTrustShouldRejectPlanBeforeExecutingAnyStep() {
        SHAFT.Properties.naturalActions.set().enabled(true).minimumTrustPercentage(95);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        WebDriver driver = driver();
        WebElement username = element();
        WebElement password = element();
        WebElement button = element();
        when(helper.getDriver()).thenReturn(driver);
        when(driver.findElements(any(By.class))).thenAnswer(invocation -> elementsForLocator(
                invocation.getArgument(0),
                username,
                password,
                button));

        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> NaturalActionExecutor.perform(
                        helper,
                        "Login with valid credentials",
                        "user@example.test",
                        "correct-password"));

        Assert.assertTrue(exception.getMessage().contains("below the configured threshold"));
        verify(username, never()).sendKeys(any(CharSequence[].class));
        verify(password, never()).sendKeys(any(CharSequence[].class));
        verify(button, never()).click();
    }

    @Test
    public void disallowedTargetShouldRejectPlanBeforeBrowserExecution() {
        SHAFT.Properties.naturalActions.set()
                .enabled(true)
                .minimumTrustPercentage(85)
                .allowedActions("element,touch");
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        WebDriver driver = driver();
        when(helper.getDriver()).thenReturn(driver);

        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> NaturalActionExecutor.perform(helper, "refresh page"));

        Assert.assertTrue(exception.getMessage().contains("below the configured threshold"));
        verify(driver, never()).navigate();
    }

    @Test
    public void rejectedIntentShouldRedactSecretsFromFailureMessage() {
        SHAFT.Properties.naturalActions.set().enabled(true).minimumTrustPercentage(85);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        WebDriver driver = driver();
        when(helper.getDriver()).thenReturn(driver);
        when(driver.findElements(any(By.class))).thenReturn(List.of());

        RuntimeException exception = Assert.expectThrows(
                RuntimeException.class,
                () -> NaturalActionExecutor.perform(
                        helper,
                        "click Login password=hunter2 token=abcdefghijklmnopqrstuvwxyz123456"));

        Assert.assertTrue(exception.getMessage().contains("password=[REDACTED]"));
        Assert.assertTrue(exception.getMessage().contains("token=[REDACTED]"));
        Assert.assertFalse(exception.getMessage().contains("hunter2"));
        Assert.assertFalse(exception.getMessage().contains("abcdefghijklmnopqrstuvwxyz123456"));
    }

    @Test
    public void unavailableConfiguredPlannerShouldReturnUnsupportedPlan() {
        SHAFT.Properties.naturalActions.set().planner("missing-provider");
        NaturalActionPlan plan = NaturalActionPlannerRegistry.plan(new NaturalActionRequest(
                mock(WebDriver.class),
                "refresh page",
                List.of(),
                false,
                false));

        Assert.assertEquals(plan.plannerId(), "missing-provider");
        Assert.assertTrue(plan.steps().isEmpty());
        Assert.assertTrue(plan.explanation().contains("unavailable"));
    }

    private WebDriver driver() {
        WebDriver driver = mock(WebDriver.class,
                org.mockito.Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        when(((JavascriptExecutor) driver).executeScript(anyString(), any(Object[].class))).thenReturn(null);
        return driver;
    }

    private WebElement element() {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        return element;
    }

    private List<WebElement> elementsForLocator(
            By locator,
            WebElement username,
            WebElement password,
            WebElement button) {
        String locatorText = locator.toString().toLowerCase(java.util.Locale.ROOT);
        if (locatorText.contains("password")) {
            return List.of(password);
        }
        if (locatorText.contains("submit") || locatorText.contains("sign in")) {
            return List.of(button);
        }
        return List.of(username);
    }
}
