package com.shaft.gui.element.internal.interaction;

import io.appium.java_client.AppiumDriver;
import io.appium.java_client.HidesKeyboard;
import org.mockito.Mockito;
import org.openqa.selenium.InvalidElementStateException;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;

/**
 * Extra unit coverage for {@link TypeStrategies} branches (issue #7077).
 */
public class TypeStrategiesCoverageUnitTest {

    @Test
    public void routeTablesShouldCoverEveryElementKind() {
        for (ElementKind kind : ElementKind.values()) {
            Assert.assertNotNull(TypeStrategies.routeFor(kind), kind.name());
            Assert.assertNotNull(TypeStrategies.mobileRouteFor(kind), kind.name());
            Assert.assertNotNull(TypeStrategies.desktopRouteFor(kind), kind.name());
            Assert.assertNotNull(TypeStrategies.playwrightRouteFor(kind, false), kind.name());
            Assert.assertNotNull(TypeStrategies.playwrightRouteFor(kind, true), kind.name());
        }
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.TEXT_LIKE, false),
                TypeStrategies.PlaywrightTypeRoute.FILL);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.TEXT_LIKE, true),
                TypeStrategies.PlaywrightTypeRoute.PRESS_SEQUENTIALLY);
        Assert.assertEquals(TypeStrategies.playwrightRouteFor(ElementKind.CONTENTEDITABLE, false),
                TypeStrategies.PlaywrightTypeRoute.PRESS_SEQUENTIALLY);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.COLOR),
                TypeStrategies.TypeRoute.SET_VALUE_WITH_EVENTS);
        Assert.assertEquals(TypeStrategies.routeFor(ElementKind.IFRAME), TypeStrategies.TypeRoute.REJECT);
    }

    @Test
    public void rejectUnsupportedShouldNameTheKind() {
        InvalidElementStateException thrown = Assert.expectThrows(InvalidElementStateException.class,
                () -> TypeStrategies.rejectUnsupported(ElementKind.DISABLED));
        Assert.assertTrue(thrown.getMessage().contains("DISABLED"));
    }

    @Test
    public void stringifyShouldJoinNullSafeSequences() {
        Assert.assertEquals(TypeStrategies.stringify(null), "");
        Assert.assertEquals(TypeStrategies.stringify(new CharSequence[]{null, "a", null, "b"}), "ab");
    }

    @Test
    public void toggleInsteadOfTypingShouldClick() {
        WebElement element = Mockito.mock(WebElement.class);
        TypeStrategies.toggleInsteadOfTyping(element, ElementKind.CHECKBOX);
        Mockito.verify(element).click();
    }

    @Test
    public void setValueWithEventsShouldRequireJavascriptExecutor() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        Assert.expectThrows(InvalidElementStateException.class,
                () -> TypeStrategies.setValueWithEvents(driver, element, "x"));
    }

    @Test
    public void setValueWithEventsShouldExecuteScript() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(JavascriptExecutor.class));
        WebElement element = Mockito.mock(WebElement.class);
        TypeStrategies.setValueWithEvents(driver, element, null);
        Mockito.verify((JavascriptExecutor) driver).executeScript(Mockito.anyString(), Mockito.eq(element), Mockito.eq(""));
    }

    @Test
    public void typeContentEditableShouldUseJsWhenClearing() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(JavascriptExecutor.class));
        WebElement element = Mockito.mock(WebElement.class);
        TypeStrategies.typeContentEditable(driver, element, new CharSequence[]{"hello"}, true);
        Mockito.verify((JavascriptExecutor) driver).executeScript(Mockito.anyString(), Mockito.eq(element), Mockito.eq("hello"));
        Mockito.verify(element, Mockito.never()).sendKeys(Mockito.any(CharSequence[].class));
    }

    @Test
    public void typeContentEditableShouldSendKeysWhenJsUnavailable() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.doThrow(new RuntimeException("no click")).when(element).click();
        TypeStrategies.typeContentEditable(driver, element, new CharSequence[]{"ab"}, true);
        Mockito.verify(element).sendKeys(Keys.chord(Keys.CONTROL, "a"));
        Mockito.verify(element).sendKeys(Keys.BACK_SPACE);
        Mockito.verify(element).sendKeys("a");
        Mockito.verify(element).sendKeys("b");
    }

    @Test
    public void sendKeysFilePathShouldForwardAbsolutePath() {
        WebElement element = Mockito.mock(WebElement.class);
        TypeStrategies.sendKeysFilePath(element, "/tmp/upload.txt");
        Mockito.verify(element).sendKeys("/tmp/upload.txt");
    }

    @Test
    public void hideKeyboardIfConfiguredShouldNoOpWhenDisabled() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HidesKeyboard.class));
        TypeStrategies.hideKeyboardIfConfigured(driver, false);
        Mockito.verifyNoInteractions(driver);
    }

    @Test
    public void hideKeyboardIfConfiguredShouldHideAndSwallowFailures() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(HidesKeyboard.class));
        TypeStrategies.hideKeyboardIfConfigured(driver, true);
        Mockito.verify((HidesKeyboard) driver).hideKeyboard();

        Mockito.doThrow(new RuntimeException("gone")).when((HidesKeyboard) driver).hideKeyboard();
        TypeStrategies.hideKeyboardIfConfigured(driver, true);
    }

    @Test
    public void assertDesktopToggleChangedShouldAcceptChangedSelectedState() {
        TypeStrategies.assertDesktopToggleChanged(ElementKind.CHECKBOX, false, true, null, null);
    }

    @Test
    public void assertDesktopToggleChangedShouldAcceptAlreadySelectedRadio() {
        TypeStrategies.assertDesktopToggleChanged(ElementKind.RADIO, true, true, "On", "On");
    }

    @Test
    public void assertDesktopToggleChangedShouldFailWhenObservableStateStays() {
        Assert.expectThrows(InvalidElementStateException.class,
                () -> TypeStrategies.assertDesktopToggleChanged(ElementKind.CHECKBOX, false, false, "Off", "Off"));
    }

    @Test
    public void assertDesktopToggleChangedShouldSkipWhenNoObservableSignal() {
        TypeStrategies.assertDesktopToggleChanged(ElementKind.CHECKBOX, null, null, null, " ");
    }

    @Test
    public void actionableMobileTypeFailureShouldIncludeComposeHint() {
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.when(element.getTagName()).thenReturn("android.widget.EditText");
        Mockito.when(element.getDomAttribute(Mockito.anyString())).thenReturn("androidx.compose.ui.viewinterop");
        InvalidElementStateException failure = TypeStrategies.actionableMobileTypeFailure(element, new RuntimeException("boom"));
        Assert.assertTrue(failure.getMessage().contains("Compose-like control"));
        Assert.assertNotNull(failure.getCause());
    }

    @Test
    public void actionableMobileTypeFailureShouldIncludeFlutterHint() {
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.when(element.getTagName()).thenReturn("FlutterEditableText");
        Mockito.when(element.getDomAttribute(Mockito.anyString())).thenReturn(null);
        Mockito.when(element.getAttribute(Mockito.anyString())).thenReturn("flutter");
        InvalidElementStateException failure = TypeStrategies.actionableMobileTypeFailure(element, null);
        Assert.assertTrue(failure.getMessage().contains("Flutter-like control"));
        Assert.assertNull(failure.getCause());
    }

    @Test
    public void typeDesktopTextShouldFallBackToWindowsKeys() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(JavascriptExecutor.class));
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.doThrow(new RuntimeException("no keys")).when(element).sendKeys(Mockito.any(CharSequence[].class));
        Mockito.when(element.getDomAttribute("Value.Value")).thenReturn("typed");
        TypeStrategies.typeDesktopText(driver, element, new CharSequence[]{"hi"});
        Mockito.verify((JavascriptExecutor) driver).executeScript(Mockito.eq("windows: keys"), Mockito.any(Map.class));
    }

    @Test
    public void windowsKeysShouldRequireJavascriptExecutor() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        Assert.expectThrows(InvalidElementStateException.class,
                () -> TypeStrategies.windowsKeys(driver, "x"));
    }

    @Test
    public void typeMobileTextShouldUseSendKeysWhenAvailable() {
        WebDriver driver = Mockito.mock(AppiumDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        TypeStrategies.typeMobileText(driver, element, new CharSequence[]{"ok"});
        Mockito.verify(element).sendKeys(Mockito.any(CharSequence[].class));
    }

    @Test
    public void typeMobileTextShouldFallBackToMobileTypeScript() {
        WebDriver driver = Mockito.mock(WebDriver.class, Mockito.withSettings()
                .extraInterfaces(JavascriptExecutor.class));
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.doThrow(new RuntimeException("sendKeys blocked")).when(element)
                .sendKeys(Mockito.any(CharSequence[].class));
        TypeStrategies.typeMobileText(driver, element, new CharSequence[]{"typed"}, false);
        Mockito.verify((JavascriptExecutor) driver).executeScript(Mockito.eq("mobile: type"), Mockito.any(Map.class));
    }


    @Test
    public void toggleMobileInsteadOfTypingShouldTap() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        try (var clicks = Mockito.mockStatic(ClickStrategies.class)) {
            TypeStrategies.toggleMobileInsteadOfTyping(driver, element, ElementKind.CHECKBOX);
            clicks.verify(() -> ClickStrategies.focusTap(driver, element));
        }
    }

    @Test
    public void toggleDesktopInsteadOfTypingShouldClickAndAssertChange() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        Mockito.when(element.isSelected()).thenReturn(false, true);
        Mockito.when(element.getDomAttribute(Mockito.anyString())).thenReturn(null);
        Mockito.when(element.getAttribute(Mockito.anyString())).thenReturn(null);
        try (var clicks = Mockito.mockStatic(ClickStrategies.class)) {
            TypeStrategies.toggleDesktopInsteadOfTyping(driver, element, ElementKind.CHECKBOX);
            clicks.verify(() -> ClickStrategies.focusWindows(driver, element));
        }
    }

    @Test
    public void typeDesktopComboboxShouldSendKeysAndEnter() {
        WebDriver driver = Mockito.mock(WebDriver.class);
        WebElement element = Mockito.mock(WebElement.class);
        try (var clicks = Mockito.mockStatic(ClickStrategies.class)) {
            TypeStrategies.typeDesktopCombobox(driver, element, new CharSequence[]{"item"});
            clicks.verify(() -> ClickStrategies.focusWindows(driver, element));
            Mockito.verify(element, Mockito.atLeastOnce()).sendKeys(Mockito.any(CharSequence[].class));
            Mockito.verify(element).sendKeys(Keys.ENTER);
        }
    }


    @Test
    public void enumSetsShouldStayNonEmpty() {
        Assert.assertFalse(EnumSet.allOf(TypeStrategies.TypeRoute.class).isEmpty());
        Assert.assertFalse(EnumSet.allOf(TypeStrategies.MobileTypeRoute.class).isEmpty());
        Assert.assertFalse(EnumSet.allOf(TypeStrategies.DesktopTypeRoute.class).isEmpty());
        Assert.assertFalse(EnumSet.allOf(TypeStrategies.PlaywrightTypeRoute.class).isEmpty());
        Assert.assertEquals(List.of(TypeStrategies.TypeRoute.values()).size(),
                TypeStrategies.TypeRoute.values().length);
    }
}
