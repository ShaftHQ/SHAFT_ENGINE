package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.driver.internal.DriverFactory.DriverFactoryHelper;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.Actions;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.FlakeProfiler;
import io.appium.java_client.windows.WindowsDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.Keys;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.Platform;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.remote.RemoteWebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Wave E (#5732): mocked WinAppDriver / UIA click/type routing without a live desktop session.
 */
@Test(singleThreaded = true)
public class DesktopInteractionStrategiesTest {
    private static final By LOCATOR = By.id("desktop-target");
    private static final byte[] PNG = new byte[]{(byte) 0x89, 'P', 'N', 'G'};

    @BeforeMethod
    public void configureWindowsDesktopMockSession() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.set().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.flags.set().scrollingMode("legacy");
        SHAFT.Properties.flags.set().clearBeforeTypingMode("off");
        SHAFT.Properties.flags.set().forceCheckTextWasTypedCorrectly(false);
        SHAFT.Properties.flags.set().attemptToClickBeforeTyping(false);
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.platform.set().targetPlatform(Platform.WINDOWS.name());
        SHAFT.Properties.web.set().targetBrowserName("WindowsApp");
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

    @DataProvider(name = "windowsKinds")
    public Object[][] windowsKinds() {
        return new Object[][]{
                {uiaElement("ControlType.Edit"), ElementKind.TEXT_LIKE},
                {uiaElement("Edit"), ElementKind.TEXT_LIKE},
                {uiaElement("ControlType.Document"), ElementKind.TEXT_LIKE},
                {uiaElement("Document"), ElementKind.TEXT_LIKE},
                {uiaElement("ControlType.Button"), ElementKind.BUTTON},
                {uiaElement("Button"), ElementKind.BUTTON},
                {uiaElement("ControlType.Hyperlink"), ElementKind.LINK},
                {uiaElement("Hyperlink"), ElementKind.LINK},
                {uiaElement("ControlType.CheckBox"), ElementKind.CHECKBOX},
                {uiaElement("CheckBox"), ElementKind.CHECKBOX},
                {uiaElement("ControlType.RadioButton"), ElementKind.RADIO},
                {uiaElement("RadioButton"), ElementKind.RADIO},
                {uiaElement("ControlType.ComboBox"), ElementKind.COMBOBOX},
                {uiaElement("ComboBox"), ElementKind.COMBOBOX},
        };
    }

    @Test(dataProvider = "windowsKinds")
    public void classifierRecognizesWindowsUiaControls(WebElement element, ElementKind expected) {
        Assert.assertEquals(ElementClassifier.classify(element), expected);
    }

    @Test
    public void focusSurfacesAreWindowAndPane() {
        Assert.assertTrue(ElementClassifier.isWindowsFocusSurface(uiaElement("ControlType.Window")));
        Assert.assertTrue(ElementClassifier.isWindowsFocusSurface(uiaElement("Pane")));
        Assert.assertFalse(ElementClassifier.isWindowsFocusSurface(uiaElement("ControlType.Edit")));
        Assert.assertFalse(ElementClassifier.isWindowsFocusSurface(null));
    }

    @Test
    public void desktopRouteMapsToggleTextAndCombo() {
        Assert.assertEquals(TypeStrategies.desktopRouteFor(ElementKind.CHECKBOX),
                TypeStrategies.DesktopTypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.desktopRouteFor(ElementKind.RADIO),
                TypeStrategies.DesktopTypeRoute.TOGGLE_CLICK);
        Assert.assertEquals(TypeStrategies.desktopRouteFor(ElementKind.TEXT_LIKE),
                TypeStrategies.DesktopTypeRoute.DESKTOP_TEXT);
        Assert.assertEquals(TypeStrategies.desktopRouteFor(ElementKind.COMBOBOX),
                TypeStrategies.DesktopTypeRoute.COMBOBOX);
        Assert.assertEquals(TypeStrategies.desktopRouteFor(ElementKind.DISABLED),
                TypeStrategies.DesktopTypeRoute.REJECT);
    }

    @Test
    public void typeEditFocusesThenSendKeys() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.Edit");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "hello-desktop");
            verify(element).click();
            verify(element).sendKeys(eq("hello-desktop"));
        }
    }

    @Test
    public void typeEditWithNativeClearDoesClickClearSendKeys() {
        SHAFT.Properties.flags.set().clearBeforeTypingMode("native");
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.Edit");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "replaced");
            verify(element).click();
            verify(element).clear();
            verify(element).sendKeys(eq("replaced"));
        }
    }

    @Test
    public void typeCheckBoxTogglesAndNeverSendKeys() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.CheckBox");
        when(element.isSelected()).thenReturn(false, true);
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "ignored");
            verify(element).click();
            verify(element, never()).sendKeys(any(CharSequence[].class));
            verify(element, never()).sendKeys(anyString());
        }
    }

    @Test
    public void typeAppendCheckBoxTogglesAndNeverSendKeys() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.CheckBox");
        when(element.isSelected()).thenReturn(false, true);
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).typeAppend(LOCATOR, "ignored");
            verify(element).click();
            verify(element, never()).sendKeys(any(CharSequence[].class));
            verify(element, never()).sendKeys(anyString());
        }
    }

    @Test
    public void typeCheckBoxFailsWhenToggleStateUnchanged() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.CheckBox");
        when(element.isSelected()).thenReturn(false, false);
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RuntimeException exception = Assert.expectThrows(RuntimeException.class,
                    () -> new Actions(helperFor(driver)).type(LOCATOR, "ignored"));
            Assert.assertTrue(hasCauseMessage(exception, "did not change observable state"),
                    "Expected toggle assert in: " + exception);
        }
    }

    @Test
    public void typeAlreadySelectedRadioWithToggleStateDoesNotFail() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.RadioButton");
        when(element.isSelected()).thenReturn(true, true);
        when(element.getAttribute("Toggle.ToggleState")).thenReturn("On");
        when(element.getDomAttribute("Toggle.ToggleState")).thenReturn("On");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "ignored");
            verify(element).click();
            verify(element, never()).sendKeys(any(CharSequence[].class));
        }
    }

    @Test
    public void typeAppendComboBoxExpandsFiltersAndEnters() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.ComboBox");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).typeAppend(LOCATOR, "Option B");
            verify(element).click();
            verify(element).sendKeys(eq("Option B"));
            verify(element).sendKeys(Keys.ENTER);
        }
    }

    @Test
    public void typeWindowOnlyEnsuresForeground() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.Window");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "should-not-type");
            verify(element).click();
            verify(element, never()).sendKeys(any(CharSequence[].class));
            verify(element, never()).sendKeys(anyString());
        }
    }

    @Test
    public void typeComboBoxExpandsFiltersAndEnters() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.ComboBox");
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "Option A");
            verify(element).click();
            verify(element).sendKeys(eq("Option A"));
            verify(element).sendKeys(Keys.ENTER);
        }
    }

    @Test
    public void typeFallsBackToWindowsKeysWhenSendKeysRejected() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("ControlType.Edit");
        doThrow(new InvalidElementStateException("not focused")).when(element).sendKeys(any(CharSequence[].class));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).type(LOCATOR, "via-keys");
            verify(driver).executeScript(eq("windows: keys"), any(Map.class));
        }
    }

    @Test
    public void clickUsesWindowsClickFallbackWhenWebDriverClickFlakes() {
        WindowsDriver driver = mockWindowsDriver();
        RemoteWebElement element = mock(RemoteWebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.getTagName()).thenReturn("ControlType.Button");
        when(element.getAttribute(anyString())).thenReturn(null);
        when(element.getDomAttribute(anyString())).thenReturn(null);
        when(element.getDomProperty(anyString())).thenReturn(null);
        when(element.getText()).thenReturn("");
        when(element.getRect()).thenReturn(new Rectangle(10, 20, 40, 60));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        when(element.getId()).thenReturn("uia-button-1");
        doThrow(new InvalidElementStateException("not clickable")).when(element).click();
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            new Actions(helperFor(driver)).click(LOCATOR);
            verify(driver, atLeastOnce()).executeScript(eq("windows: click"), any(Map.class));
        }
    }

    @Test
    public void webDefaultsUnchangedWhenNotWindowsDesktop() {
        SHAFT.Properties.platform.set().targetPlatform(Platform.LINUX.name());
        SHAFT.Properties.web.set().targetBrowserName("chrome");
        SHAFT.Properties.flags.set().clickUsingJavascriptWhenWebDriverClickFails(true);

        WindowsDriver driver = mockWindowsDriver();
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
            verify(driver, never()).executeScript(eq("windows: click"), any(Map.class));
        }
    }

    @Test
    public void typeDesktopTextDirectWindowsKeys() {
        WindowsDriver driver = mockWindowsDriver();
        WebElement element = uiaElement("Edit");
        doThrow(new InvalidElementStateException("sendKeys blocked")).when(element).sendKeys(any(CharSequence[].class));

        TypeStrategies.typeDesktopText(driver, element, new CharSequence[]{"via-keys"});
        verify(driver).executeScript(eq("windows: keys"), any(Map.class));
    }

    @SuppressWarnings("rawtypes")
    private static WindowsDriver mockWindowsDriver() {
        return mock(WindowsDriver.class, org.mockito.Mockito.withSettings()
                .extraInterfaces(TakesScreenshot.class));
    }

    private DriverFactoryHelper helperFor(WindowsDriver driver) {
        DriverFactoryHelper helper = mock(DriverFactoryHelper.class);
        when(helper.getDriver()).thenReturn(driver);
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return helper;
    }

    private WebElement uiaElement(String controlType) {
        WebElement element = mock(WebElement.class);
        when(element.isDisplayed()).thenReturn(true);
        when(element.isEnabled()).thenReturn(true);
        when(element.isSelected()).thenReturn(false);
        when(element.getAccessibleName()).thenReturn("desktop target");
        when(element.getText()).thenReturn("");
        when(element.getTagName()).thenReturn(controlType);
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
