package com.shaft.gui.element.internal;

import com.shaft.driver.SHAFT;
import com.shaft.gui.browser.internal.JavaScriptWaitManager;
import com.shaft.gui.element.internal.interaction.IosXcuiAttributeNames;
import com.shaft.gui.internal.locator.LocatorBuilder;
import com.shaft.gui.internal.locator.ShadowLocatorBuilder;
import com.shaft.properties.internal.Properties;
import com.shaft.tools.io.internal.FlakeProfiler;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.StaleElementReferenceException;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.TimeoutException;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * #7455 and #7456: iOS attribute alias and stale re-find, without a device.
 */
@Test(singleThreaded = true)
public class IosElementReadTest {
    private static final By LOCATOR = By.id("login_nafath_anotherWayButton");
    private static final byte[] PNG = new byte[]{(byte) 0x89, 'P', 'N', 'G'};
    private static final String XCUI_STALE =
            "stale element reference: The previously found element \"Button\" is not present in the current view anymore. "
                    + "Original error: predicate 'fb_uid IN {\"13000000-0000-0000-E40E-000000000000\"}'";

    @BeforeMethod
    public void configure() {
        SHAFT.Properties.reporting.set().captureElementName(false);
        SHAFT.Properties.flags.setForCurrentThread().forceCheckElementLocatorIsUnique(false);
        SHAFT.Properties.visuals.set().createAnimatedGif(false);
        SHAFT.Properties.visuals.set().screenshotParamsWhenToTakeAScreenshot("ValidationPointsOnly");
        SHAFT.Properties.visuals.set().screenshotParamsWatermark(false);
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(1);
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
        FlakeProfiler.reset();
    }

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        LocatorBuilder.cleanup();
        ShadowLocatorBuilder.cleanup();
        FlakeProfiler.reset();
        Properties.clearForCurrentThread();
    }

    @Test(description = "#7455: getAttribute/getDomAttribute class and className are issued as type on iOS XCUITest")
    public void iosClassAndClassNameAreIssuedAsType() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        WebDriver driver = driver();
        WebElement element = element();
        when(element.getAttribute(anyString())).thenAnswer(invocation -> attributeValue(invocation.getArgument(0)));
        when(element.getDomAttribute(anyString())).thenAnswer(invocation -> attributeValue(invocation.getArgument(0)));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Actions actions = new Actions(driver, true);
            Assert.assertEquals(actions.get().attribute(LOCATOR, "class"), "XCUIElementTypeButton");
            Assert.assertEquals(actions.get().attribute(LOCATOR, "className"), "XCUIElementTypeButton");
            Assert.assertEquals(actions.get().domAttribute(LOCATOR, "class"), "XCUIElementTypeButton");
        }
        verify(element, times(2)).getAttribute("type");
        verify(element).getDomAttribute("type");
        verify(element, org.mockito.Mockito.never()).getAttribute("class");
        verify(element, org.mockito.Mockito.never()).getAttribute("className");
        verify(element, org.mockito.Mockito.never()).getDomAttribute("class");
    }

    @Test(description = "#7455: Android class reads stay class")
    public void androidClassReadStaysClass() {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        SHAFT.Properties.mobile.set().browserName("").automationName("UiAutomator2");
        WebDriver driver = driver();
        WebElement element = element();
        when(element.getAttribute(anyString())).thenAnswer(invocation -> attributeValue(invocation.getArgument(0)));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(element));

        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Assert.assertEquals(new Actions(driver, true).get().attribute(LOCATOR, "class"), "android.widget.Button");
        }
        verify(element).getAttribute("class");
        verify(element, org.mockito.Mockito.never()).getAttribute("type");
    }

    @Test(description = "#7455: a null attribute name is issued as empty")
    public void nullAttributeNameIsIssuedEmpty() {
        Assert.assertEquals(IosXcuiAttributeNames.issued(null), "");
    }

    @Test(description = "#7455: iOS mobile web keeps class")
    public void iosMobileWebKeepsClass() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("Safari");
        Assert.assertEquals(IosXcuiAttributeNames.issued("class"), "class");
    }

    @Test(description = "#7456: a wrapped stale cause drops the cached element and re-finds inside the wait")
    public void wrappedStaleCauseRefindsDisplayed() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        assertRefound(driver -> {
            WebElement stale = element();
            WebElement fresh = element();
            when(stale.isDisplayed()).thenThrow(new WebDriverException("other",
                    new StaleElementReferenceException("previously found element")));
            when(fresh.isDisplayed()).thenReturn(true);
            when(driver.findElements(LOCATOR)).thenReturn(List.of(stale), List.of(fresh));
            return new Actions(driver, true).get().isDisplayed(LOCATOR);
        }, true);
    }

    @Test(description = "#7456: stale XCUI text drops the cached element and re-finds inside the wait")
    public void staleXcuiTextRefindsBooleanAndAttributeReads() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        assertRefound(driver -> {
            WebElement stale = element();
            WebElement fresh = element();
            when(stale.isDisplayed()).thenThrow(new WebDriverException(XCUI_STALE));
            when(fresh.isDisplayed()).thenReturn(true);
            when(driver.findElements(LOCATOR)).thenReturn(List.of(stale), List.of(fresh));
            return new Actions(driver, true).get().isDisplayed(LOCATOR);
        }, true);
        assertRefound(driver -> {
            WebElement stale = element();
            WebElement fresh = element();
            when(stale.isEnabled()).thenThrow(new WebDriverException("No matches found for predicate fb_uid IN {\"1\"}"));
            when(fresh.isEnabled()).thenReturn(false);
            when(driver.findElements(LOCATOR)).thenReturn(List.of(stale), List.of(fresh));
            return new Actions(driver, true).get().isEnabled(LOCATOR);
        }, false);
        assertRefound(driver -> {
            WebElement stale = element();
            WebElement fresh = element();
            when(stale.isSelected()).thenThrow(new StaleElementReferenceException("previously found element"));
            when(fresh.isSelected()).thenReturn(true);
            when(driver.findElements(LOCATOR)).thenReturn(List.of(stale), List.of(fresh));
            return new Actions(driver, true).get().isSelected(LOCATOR);
        }, true);
        assertRefound(driver -> {
            WebElement stale = element();
            WebElement fresh = element();
            when(stale.getAttribute(anyString())).thenThrow(new WebDriverException(XCUI_STALE));
            when(fresh.getAttribute("name")).thenReturn("login_nafath_anotherWayButton");
            when(driver.findElements(LOCATOR)).thenReturn(List.of(stale), List.of(fresh));
            return new Actions(driver, true).get().attribute(LOCATOR, "name");
        }, "login_nafath_anotherWayButton");
    }

    @Test(description = "#7456: a non-stale driver error is not retried for the whole wait")
    public void unrelatedDriverExceptionIsNotRetried() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        // Safari Local E2E sets -DtargetBrowserName=safari for the whole Maven JVM; prove
        // native XCUI fail-fast still holds under that property (#7530 / #7456).
        SHAFT.Properties.web.set().targetBrowserName("safari");
        SHAFT.Properties.timeouts.set().defaultElementIdentificationTimeout(4);
        WebDriver driver = driver();
        WebElement element = element();
        when(element.isDisplayed()).thenThrow(new WebDriverException("element click intercepted"));
        AtomicInteger finds = new AtomicInteger();
        when(driver.findElements(LOCATOR)).thenAnswer(invocation -> {
            finds.incrementAndGet();
            return List.of(element);
        });
        long started = System.nanoTime();
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                    () -> new Actions(driver, true).get().isDisplayed(LOCATOR));
            Assert.assertFalse(hasType(failure, TimeoutException.class), failure.toString());
        }
        Assert.assertTrue(System.nanoTime() - started < 2_000_000_000L, "unrelated error waited out the element budget");
        Assert.assertEquals(finds.get(), 1);
    }

    @Test(description = "#7456: XCUI stale that never clears fails only after the wait expires")
    public void persistentXcuiStaleFailsAfterTheWait() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        WebDriver driver = driver();
        WebElement stale = element();
        when(stale.isDisplayed()).thenThrow(new WebDriverException(XCUI_STALE));
        when(driver.findElements(LOCATOR)).thenReturn(List.of(stale));
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            RuntimeException failure = Assert.expectThrows(RuntimeException.class,
                    () -> new Actions(driver, true).get().isDisplayed(LOCATOR));
            Assert.assertTrue(hasType(failure, TimeoutException.class), failure.toString());
        }
    }

    private static void assertRefound(java.util.function.Function<WebDriver, Object> read, Object expected) {
        WebDriver driver = driver();
        try (var ignored = org.mockito.Mockito.mockStatic(JavaScriptWaitManager.class)) {
            Assert.assertEquals(read.apply(driver), expected);
        }
        verify(driver, times(2)).findElements(LOCATOR);
    }

    private static WebDriver driver() {
        WebDriver driver = mock(WebDriver.class, org.mockito.Mockito.withSettings().extraInterfaces(TakesScreenshot.class));
        when(((TakesScreenshot) driver).getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return driver;
    }

    private static WebElement element() {
        WebElement element = mock(WebElement.class);
        when(element.getTagName()).thenReturn("XCUIElementTypeButton");
        when(element.getText()).thenReturn("");
        when(element.getRect()).thenReturn(new org.openqa.selenium.Rectangle(1, 2, 10, 10));
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(PNG);
        return element;
    }

    private static String attributeValue(String name) {
        if ("type".equals(name)) {
            return "XCUIElementTypeButton";
        }
        if ("class".equals(name)) {
            return "android.widget.Button";
        }
        throw new WebDriverException("The attribute '" + name + "' is unknown. Valid attribute names are: (type, wdType)");
    }

    private static boolean hasType(Throwable thrown, Class<? extends Throwable> type) {
        Throwable current = thrown;
        int depth = 0;
        while (current != null && depth < 8) {
            if (type.isInstance(current)) {
                return true;
            }
            current = current.getCause();
            depth++;
        }
        return false;
    }
}
