package com.shaft.validation.internal;

import com.shaft.cli.FileActions;
import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import com.shaft.validation.ValidationEnums;
import org.mockito.Mockito;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Coverage-focused unit tests for {@code matchesAriaSnapshot()} wiring from
 * {@link WebDriverElementValidationsBuilder} through to the baseline lifecycle.
 */
public class WebDriverElementValidationsBuilderAriaSnapshotUnitTest {
    private static final By SAMPLE_LOCATOR = By.id("sample");

    @AfterMethod(alwaysRun = true)
    public void resetState() {
        ValidationsHelper.resetVerificationStateAfterFailing();
        Properties.clearForCurrentThread();
    }

    @Test(description = "matchesAriaSnapshot() should configure validation state before execution")
    public void matchesAriaSnapshotShouldConfigureStateBeforeExecution() {
        WebDriverElementValidationsBuilder builder = new WebDriverElementValidationsBuilder(
                ValidationEnums.ValidationCategory.HARD_ASSERT, null, SAMPLE_LOCATOR, new StringBuilder("the element "));

        try {
            builder.matchesAriaSnapshot("login-form");
            Assert.fail("Expected invocation to fail without a live WebDriver session.");
        } catch (Throwable ignored) {
            // expected in unit context without a browser session
        }

        Assert.assertEquals(builder.validationMethod, "elementAriaSnapshotMatches");
        Assert.assertEquals(builder.ariaSnapshotFileName, "login-form");
        Assert.assertEquals(builder.validationType, ValidationEnums.ValidationType.POSITIVE);
        Assert.assertTrue(builder.reportMessageBuilder.toString().contains("matches the aria snapshot \"login-form\"."));
    }

    @Test(description = "first run should save the captured aria snapshot as the new baseline and pass")
    public void matchesAriaSnapshotShouldCreateMissingBaselineAndPass() {
        WebDriver driver = mock(WebDriver.class, Mockito.withSettings().extraInterfaces(JavascriptExecutor.class));
        WebElement element = mock(WebElement.class);
        when(driver.findElement(SAMPLE_LOCATOR)).thenReturn(element);
        when(((JavascriptExecutor) driver).executeScript(anyString(), any())).thenReturn(List.of(
                Map.of("role", "button", "name", "Sign in", "children", List.of())
        ));

        String baselineFile = "matchesAriaSnapshotShouldCreateMissingBaselineAndPass_" + System.identityHashCode(this);
        String baselinePath = SHAFT.Properties.paths.ariaSnapshot() + baselineFile + ".yaml";
        FileActions.getInstance(true).deleteFile(baselinePath);

        try {
            WebDriverElementValidationsBuilder builder = new WebDriverElementValidationsBuilder(
                    ValidationEnums.ValidationCategory.HARD_ASSERT, driver, SAMPLE_LOCATOR, new StringBuilder("the element "));

            builder.matchesAriaSnapshot(baselineFile);

            Assert.assertTrue(FileActions.getInstance(true).doesFileExist(baselinePath));
            Assert.assertTrue(FileActions.getInstance(true).readFile(baselinePath).contains("button \"Sign in\""));
        } finally {
            FileActions.getInstance(true).deleteFile(baselinePath);
        }
    }

    @Test(description = "on a mobile-native session, first run should capture via the Appium accessibility XML converter instead of JavaScript execution")
    public void matchesAriaSnapshotShouldUseMobileConverterOnMobileNativeExecution() {
        String previousTargetPlatform = SHAFT.Properties.platform.targetPlatform();
        SHAFT.Properties.platform.set().targetPlatform("Android");

        WebDriver driver = mock(WebDriver.class);
        WebElement element = mock(WebElement.class);
        when(driver.findElement(SAMPLE_LOCATOR)).thenReturn(element);
        when(element.getAttribute("outerXML")).thenReturn("""
                <hierarchy>
                  <android.widget.Button class="android.widget.Button" content-desc="Sign in" bounds="[0,0][100,50]"/>
                </hierarchy>
                """);

        String baselineFile = "matchesAriaSnapshotShouldUseMobileConverterOnMobileNativeExecution_" + System.identityHashCode(this);
        String baselinePath = SHAFT.Properties.paths.ariaSnapshot() + baselineFile + ".yaml";
        FileActions.getInstance(true).deleteFile(baselinePath);

        try {
            WebDriverElementValidationsBuilder builder = new WebDriverElementValidationsBuilder(
                    ValidationEnums.ValidationCategory.HARD_ASSERT, driver, SAMPLE_LOCATOR, new StringBuilder("the element "));

            builder.matchesAriaSnapshot(baselineFile);

            Assert.assertTrue(FileActions.getInstance(true).doesFileExist(baselinePath));
            Assert.assertTrue(FileActions.getInstance(true).readFile(baselinePath).contains("button \"Sign in\""));
        } finally {
            FileActions.getInstance(true).deleteFile(baselinePath);
            SHAFT.Properties.platform.set().targetPlatform(previousTargetPlatform);
        }
    }
}
