package com.shaft.gui.internal.healing;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.Actions;
import com.shaft.properties.internal.Properties;
import io.appium.java_client.AppiumDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.Platform;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class HealingActionsIntegrationTest {
    private static final By ORIGINAL = By.id("old-id");

    @BeforeMethod
    public void configure() {
        SHAFT.Properties.healing.set().strategy("shaft-heal");
        SHAFT.Properties.healenium.set().healEnabled(false);
        SHAFT.Properties.platform.set().targetPlatform(Platform.LINUX.name());
        SHAFT.Properties.mobile.set().browserName("chrome");
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(true);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        HealingProviderRegistry.resetProviderForTesting();
        Properties.clearForCurrentThread();
    }

    @Test
    public void fluentActionShouldExecuteOnlyAfterProviderReturnsOneValidatedElement() {
        WebDriver driver = mock(WebDriver.class);
        WebElement recovered = mock(WebElement.class);
        when(driver.findElements(ORIGINAL)).thenReturn(List.of());
        when(recovered.isDisplayed()).thenReturn(true);
        when(recovered.isEnabled()).thenReturn(true);
        HealingProvider provider = mock(HealingProvider.class);
        when(provider.resolve(any())).thenReturn(Optional.of(
                new HealingResolution("attempt-1", List.of(recovered), By.id("new-id"))));
        HealingProviderRegistry.setProviderForTesting(provider);
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helper).click(ORIGINAL);
        }

        verify(recovered).click();
        verify(provider).recordOutcome(any());
        verify(provider, never()).observe(any());
    }

    @Test
    public void providerFailureShouldPreserveOriginalActionFailure() {
        WebDriver driver = mock(WebDriver.class);
        when(driver.findElements(ORIGINAL)).thenReturn(List.of());
        HealingProvider provider = mock(HealingProvider.class);
        when(provider.resolve(any())).thenThrow(new IllegalStateException("provider failed"));
        HealingProviderRegistry.setProviderForTesting(provider);

        Optional<HealingResolution> resolution = HealingManager.resolve(
                driver, ORIGINAL, "CLICK", true, null, null, null);

        org.testng.Assert.assertTrue(resolution.isEmpty());
    }

    @Test
    public void validationActionsShouldNeverInvokeHealingProvider() {
        WebDriver driver = mock(WebDriver.class);
        HealingProvider provider = mock(HealingProvider.class);
        HealingProviderRegistry.setProviderForTesting(provider);

        Optional<HealingResolution> resolution = HealingManager.resolve(
                driver, ORIGINAL, "ASSERT_THAT_ELEMENT_EXISTS", true, null, null, null);

        Assert.assertTrue(resolution.isEmpty());
        verify(provider, never()).resolve(any());
    }

    @Test
    public void nativeMobileActionsShouldUseTheConfiguredHealingProvider() {
        AppiumDriver driver = mock(AppiumDriver.class);
        WebElement recovered = mock(WebElement.class);
        HealingProvider provider = mock(HealingProvider.class);
        when(provider.resolve(any())).thenReturn(Optional.of(
                new HealingResolution("attempt-mobile", List.of(recovered), By.id("new-id"))));
        HealingProviderRegistry.setProviderForTesting(provider);

        Optional<HealingResolution> resolution = HealingManager.resolve(
                driver, ORIGINAL, "CLICK", true, null, null, null);

        Assert.assertTrue(resolution.isPresent());
        verify(provider, atLeastOnce()).resolve(any());
    }

    @Test(dataProvider = "postActionVerification")
    public void recoveryShouldRecordExpectedPostActionVerification(
            String action,
            String expectedVerification) {
        WebDriver driver = mock(WebDriver.class);
        WebElement recovered = mock(WebElement.class);
        when(recovered.isDisplayed()).thenReturn(true);
        when(recovered.isEnabled()).thenReturn(true);
        HealingProvider provider = mock(HealingProvider.class);
        HealingProviderRegistry.setProviderForTesting(provider);
        HealingResolution resolution =
                new HealingResolution("attempt-type", List.of(recovered), By.id("new-id"));

        HealingManager.recordOutcome(driver, resolution, ORIGINAL, action, true, "");

        var outcome = org.mockito.ArgumentCaptor.forClass(HealingActionOutcome.class);
        verify(provider).recordOutcome(outcome.capture());
        Assert.assertEquals(outcome.getValue().verification(), expectedVerification);
    }

    @DataProvider
    public Object[][] postActionVerification() {
        return new Object[][]{
                {"TYPE", "ELEMENT_INTERACTABLE"},
                {"SELECT", "ELEMENT_INTERACTABLE"},
                {"CLEAR", "ELEMENT_INTERACTABLE"},
                {"ELEMENT_RESOLUTION", "ELEMENT_INTERACTABLE"},
                {"WAIT", "ELEMENT_INTERACTABLE"},
                {"CLICK", "UNVERIFIABLE"},
                {"HOVER", "UNVERIFIABLE"},
                {"DRAG_AND_DROP", "UNVERIFIABLE"}
        };
    }
}
