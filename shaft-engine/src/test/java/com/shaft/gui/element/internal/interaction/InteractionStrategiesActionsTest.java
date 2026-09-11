package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.FlakeProfiler;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Test(singleThreaded = true)
public class InteractionStrategiesActionsTest {
    private static final By LOCATOR = By.id("target");
    private static final byte[] PNG = new byte[]{(byte) 0x89, 'P', 'N', 'G'};

    @BeforeMethod
    public void configureFastMockFriendlyProperties() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
        SHAFT.Properties.flags.set().attemptToClickBeforeTyping(false);
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.platform.set().targetPlatform(org.openqa.selenium.Platform.LINUX.name());
        SHAFT.Properties.mobile.set().browserName("chrome");
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
        FlakeProfiler.reset();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanupThreadLocalState() {
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
        FlakeProfiler.reset();
        Properties.clearForCurrentThread();
    }

    @Test
    public void typeTextLikeUsesSendKeys() {
        WebDriver driver = mockDriver();
        WebElement element = textInput();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "hello");
            verify(element).sendKeys(eq("hello"));
            verify(element, never()).click();
        }
    }

    @Test
    public void typeCheckboxClicksAndNeverSendKeys() {
        WebDriver driver = mockDriver();
        WebElement element = inputOfType("checkbox");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "ignored");
            verify(element).click();
            verify(element, never()).sendKeys(any(CharSequence[].class));
            verify(element, never()).sendKeys(anyString());
            verify(element, never()).clear();
        }
    }

    @Test
    public void typeSelectUsesNativeSelectApiNotBlindSendKeys() {
        SHAFT.Properties.flags.set().handleNonSelectDropDown(false);
        WebDriver driver = mockDriver();
        WebElement dropdown = baseElement();
        when(dropdown.getTagName()).thenReturn("select");
        when(dropdown.getDomAttribute("multiple")).thenReturn(null);
        WebElement option = baseElement();
        when(option.getText()).thenReturn("Egypt");
        when(option.getDomProperty("value")).thenReturn("eg");
        when(option.getAttribute("index")).thenReturn("0");
        when(dropdown.findElements(By.tagName("option"))).thenReturn(List.of(option));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(dropdown));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "Egypt");
            verify(option).click();
            verify(dropdown, never()).sendKeys(any(CharSequence[].class));
            verify(dropdown, never()).sendKeys(anyString());
        }
    }

    @Test
    public void typeFileUsesSendKeysPathWithoutClear() {
        WebDriver driver = mockDriver();
        WebElement element = inputOfType("file");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "/tmp/upload.txt");
            verify(element).sendKeys("/tmp/upload.txt");
            verify(element, never()).clear();
        }
    }

    @Test
    public void typeContentEditableUsesJsInsertNotClear() {
        SHAFT.Properties.flags.set().clearBeforeTypingMode("native");
        WebDriver driver = mockDriver();
        WebElement element = baseElement();
        when(element.getTagName()).thenReturn("div");
        when(element.getDomAttribute("contenteditable")).thenReturn("true");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "editor text");
            verify((JavascriptExecutor) driver).executeScript(contains("insertText"), eq(element), eq("editor text"));
            verify(element, never()).clear();
            verify(element, never()).sendKeys(any(CharSequence[].class));
        }
    }

    @Test
    public void typeDateSetsValueWithEventsNotClearSendKeys() {
        WebDriver driver = mockDriver();
        WebElement element = inputOfType("date");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "2024-01-15");
            verify((JavascriptExecutor) driver).executeScript(contains("arguments[0].value"), eq(element), eq("2024-01-15"));
            verify(element, never()).sendKeys(any(CharSequence[].class));
            verify(element, never()).clear();
        }
    }

    @Test
    public void clickLadderRetriesAfterScrollThenUsesFlaggedJs() {
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);
        WebDriver driver = mockDriver();
        WebElement element = baseElement();
        when(element.getTagName()).thenReturn("button");
        doThrow(new InvalidElementStateException("blocked")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).click(LOCATOR);
            verify((JavascriptExecutor) driver, atLeastOnce()).executeScript(contains("scrollIntoView"), eq(element));
            verify((JavascriptExecutor) driver).executeScript(eq("arguments[0].click();"), eq(element));
        }
    }

    @Test
    public void clickPreservesExceptionWhenJsFallbackDisabled() {
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(false);
        WebDriver driver = mockDriver();
        WebElement element = baseElement();
        when(element.getTagName()).thenReturn("button");
        doThrow(new InvalidElementStateException("blocked")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RuntimeException exception = Assert.expectThrows(RuntimeException.class,
                    () -> new Actions(helperFor(driver)).click(LOCATOR));
            Assert.assertTrue(hasCause(exception, InvalidElementStateException.class));
            verify((JavascriptExecutor) driver, never()).executeScript(eq("arguments[0].click();"), eq(element));
        }
    }

    private static WebDriver mockDriver() {
        return mock(WebDriver.class, org.mockito.Mockito.withSettings()
                .extraInterfaces(JavascriptExecutor.class, TakesScreenshot.class));
    }

    private DriverFactoryHelper helperFor(WebDriver driver) {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return helper;
    }

    private WebElement baseElement() {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.isSelected()).thenReturn(false);
        when(element.getAccessibleName()).thenReturn("accessible target");
        when(element.getText()).thenReturn("");
        when(element.getAttribute(anyString())).thenReturn(null);
        when(element.getDomAttribute(anyString())).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(element.getCssValue(anyString())).thenReturn("");
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 30, 40));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return element;
    }

    private WebElement textInput() {
        return inputOfType("text");
    }

    private WebElement inputOfType(String type) {
        WebElement element = baseElement();
        when(element.getTagName()).thenReturn("input");
        when(element.getDomAttribute("type")).thenReturn(type);
        return element;
    }

    private boolean hasCause(Throwable throwable, Class<? extends Throwable> expectedCause) {
        Throwable current = throwable;
        while (current != null) {
            if (expectedCause.isInstance(current)) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }
}
