package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.FlakeProfiler;
import io.appium.java_client.AppiumDriver;
import io.appium.java_client.HidesKeyboard;
import io.appium.java_client.android.CanReplaceElementValue;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Platform;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.interactions.Interactive;
import org.openqa.selenium.remote.RemoteWebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.Collection;
import java.util.List;
import java.util.Map;

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

/**
 * Wave D (#5732): mocked mobile-native click/type routing without a device farm.
 */
@Test(singleThreaded = true)
public class MobileInteractionStrategiesTest {
    private static final By LOCATOR = By.id("mobile-target");
    private static final byte[] PNG = new byte[]{(byte) 0x89, 'P', 'N', 'G'};

    @BeforeMethod
    public void configureMobileNativeMockSession() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
        SHAFT.Properties.flags.set().attemptToClickBeforeTyping(false);
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(false);
        SHAFT.Properties.flags.set().hideKeyboardAfterTyping(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.platform.set().targetPlatform(Platform.ANDROID.name());
        SHAFT.Properties.mobile.set().browserName("");
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

    @DataProvider(name = "mobileKinds")
    public Object[][] mobileKinds() {
        return new Object[][]{
                {nativeElement("android.widget.EditText"), ElementKind.TEXT_LIKE},
                {nativeElement("android.widget.CheckBox"), ElementKind.CHECKBOX},
                {nativeElement("android.widget.Switch"), ElementKind.CHECKBOX},
                {nativeElement("android.widget.RadioButton"), ElementKind.RADIO},
                {nativeElement("android.widget.Button"), ElementKind.BUTTON},
                {nativeElement("XCUIElementTypeTextField"), ElementKind.TEXT_LIKE},
                {nativeElement("XCUIElementTypeSecureTextField"), ElementKind.TEXT_LIKE},
                {nativeElement("XCUIElementTypeSwitch"), ElementKind.CHECKBOX},
                {nativeElement("XCUIElementTypeButton"), ElementKind.BUTTON},
                {nativeElement("TextField"), ElementKind.TEXT_LIKE},
                {nativeElement("EditableText"), ElementKind.TEXT_LIKE},
        };
    }

    @Test(dataProvider = "mobileKinds")
    public void classifierRecognizesMobileNativeControls(WebElement element, ElementKind expected) {
        Assert.assertEquals(ElementClassifier.classify(element), expected);
    }

    @Test
    public void mobileRouteMapsToggleAndText() {
        Assert.assertEquals(TypeStrategies.mobileRouteFor(ElementKind.CHECKBOX),
                TypeStrategies.MobileTypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.mobileRouteFor(ElementKind.RADIO),
                TypeStrategies.MobileTypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.mobileRouteFor(ElementKind.TEXT_LIKE),
                TypeStrategies.MobileTypeRoute.MOBILE_TEXT);
        Assert.assertEquals(TypeStrategies.mobileRouteFor(ElementKind.DISABLED),
                TypeStrategies.MobileTypeRoute.REJECT);
    }

    @Test
    public void typeEditTextFocusesThenSendKeys() {
        AppiumDriver driver = mockAppiumDriver();
        WebElement element = nativeElement("android.widget.EditText");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "hello-mobile");
            verify(element).click();
            verify(element).sendKeys(eq("hello-mobile"));
        }
    }

    @Test
    public void typeSwitchTogglesAndNeverSendKeys() {
        AppiumDriver driver = mockAppiumDriver();
        WebElement element = nativeElement("android.widget.Switch");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "ignored");
            verify(element).click();
            verify(element, never()).sendKeys(any(CharSequence[].class));
            verify(element, never()).sendKeys(anyString());
        }
    }

    @Test
    public void typeFallsBackToMobileTypeWhenSendKeysRejected() {
        AppiumDriver driver = mockAppiumDriver();
        WebElement element = nativeElement("android.widget.EditText");
        doThrow(new InvalidElementStateException("not focused")).when(element).sendKeys(any(CharSequence[].class));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "compose-text");
            verify(driver).executeScript(eq("mobile: type"), any(Map.class));
            // clearBeforeTypingMode defaults to off on mobile — must not wipe via replaceElementValue
            verify((CanReplaceElementValue) driver, never()).replaceElementValue(any(), anyString());
        }
    }

    @Test
    public void typeWithNativeClearMayUseReplaceElementValue() {
        SHAFT.Properties.flags.set().clearBeforeTypingMode("native");
        AppiumDriver driver = mockAppiumDriver();
        RemoteWebElement element = mock(RemoteWebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getTagName()).thenReturn("android.widget.EditText");
        when(element.getAttribute(anyString())).thenReturn(null);
        when(element.getDomAttribute(anyString())).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(element.getText()).thenReturn("");
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 40, 60));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        doThrow(new InvalidElementStateException("sendKeys blocked")).when(element).sendKeys(any(CharSequence[].class));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "replaced");
            verify((CanReplaceElementValue) driver).replaceElementValue(eq(element), eq("replaced"));
        }
    }

    @Test
    public void mobileRouteRejectsRangeAndDate() {
        Assert.assertEquals(TypeStrategies.mobileRouteFor(ElementKind.RANGE),
                TypeStrategies.MobileTypeRoute.REJECT);
        Assert.assertEquals(TypeStrategies.mobileRouteFor(ElementKind.DATE_LIKE),
                TypeStrategies.MobileTypeRoute.REJECT);
    }

    @Test
    public void typeComposeRejectionSurfacesActionableError() {
        AppiumDriver driver = mockAppiumDriver();
        WebElement element = nativeElement("android.view.View");
        when(element.getDomAttribute("class")).thenReturn("androidx.compose.ui.platform.ComposeView");
        doThrow(new InvalidElementStateException("setValue rejected"))
                .when(element).sendKeys(any(CharSequence[].class));
        when(driver.executeScript(eq("mobile: type"), any(Map.class)))
                .thenThrow(new InvalidElementStateException("still rejected"));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RuntimeException exception = Assert.expectThrows(RuntimeException.class,
                    () -> new Actions(helperFor(driver)).type(LOCATOR, "x"));
            Assert.assertTrue(hasCauseMessage(exception, "Focus the field first")
                    || hasCauseMessage(exception, "testTagsAsResourceId")
                    || hasCauseMessage(exception, "Compose"),
                    "Expected Compose/focus guidance in: " + exception);
        }
    }

    @Test
    public void hideKeyboardRunsWhenFlagEnabled() {
        SHAFT.Properties.flags.set().hideKeyboardAfterTyping(true);
        AppiumDriver driver = mockAppiumDriver();
        WebElement element = nativeElement("android.widget.EditText");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "typed");
            verify((HidesKeyboard) driver).hideKeyboard();
        }
    }

    @Test
    public void clickUsesTouchFallbackWhenWebDriverClickFlakes() {
        AppiumDriver driver = mockAppiumDriver();
        WebElement element = nativeElement("android.widget.Button");
        doThrow(new InvalidElementStateException("not clickable")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).click(LOCATOR);
            verify(driver, atLeastOnce()).perform(any(Collection.class));
        }
    }

    @Test
    public void webDefaultsUnchangedWhenNotMobileNative() {
        SHAFT.Properties.platform.set().targetPlatform(Platform.LINUX.name());
        SHAFT.Properties.mobile.set().browserName("chrome");
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);

        AppiumDriver driver = mockAppiumDriver();
        // Use a plain WebDriver-style mock path: still AppiumDriver but isMobileNativeExecution is false.
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getTagName()).thenReturn("button");
        when(element.getDomAttribute(anyString())).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(element.getAttribute(anyString())).thenReturn(null);
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 30, 40));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        doThrow(new InvalidElementStateException("blocked")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).click(LOCATOR);
            verify(driver).executeScript(eq("arguments[0].click();"), eq(element));
            verify(driver, never()).perform(any(Collection.class));
        }
    }

    @Test
    public void typeMobileTextDirectReplaceElementValue() {
        AppiumDriver driver = mockAppiumDriver();
        RemoteWebElement element = mock(RemoteWebElement.class);
        doThrow(new InvalidElementStateException("sendKeys blocked")).when(element).sendKeys(any(CharSequence[].class));

        TypeStrategies.typeMobileText(driver, element, new CharSequence[]{"via-replace"}, true);
        verify((CanReplaceElementValue) driver).replaceElementValue(eq(element), eq("via-replace"));
    }

    private static AppiumDriver mockAppiumDriver() {
        return mock(AppiumDriver.class, org.mockito.Mockito.withSettings()
                .extraInterfaces(TakesScreenshot.class, Interactive.class, HidesKeyboard.class, CanReplaceElementValue.class));
    }

    private DriverFactoryHelper helperFor(AppiumDriver driver) {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return helper;
    }

    private WebElement nativeElement(String className) {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.isSelected()).thenReturn(false);
        when(element.getAccessibleName()).thenReturn("mobile target");
        when(element.getText()).thenReturn("");
        when(element.getTagName()).thenReturn(className);
        when(element.getAttribute(anyString())).thenReturn(null);
        when(element.getDomAttribute(anyString())).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(element.getCssValue(anyString())).thenReturn("");
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 40, 60));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return element;
    }

    private boolean hasCauseMessage(Throwable throwable, String fragment) {
        Throwable current = throwable;
        while (current != null) {
            if (current.getMessage() != null && current.getMessage().contains(fragment)) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }
}
