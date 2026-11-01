package com.shaft.gui.element.internal.interaction;

import com.shaft.driver.SHAFT;
import com.shaft.properties.internal.Properties;
import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** #7669: branch coverage for IosXcuiAttributeNames helper paths. */
public class IosXcuiAttributeNamesCoverageTest {

    @AfterMethod(alwaysRun = true)
    public void cleanup() {
        Properties.clearForCurrentThread();
    }

    @Test(description = "#7669: isIosXcuiNative false for Android, mobile web, Flutter, and blank platform")
    public void isIosXcuiNativeGates() {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        Assert.assertFalse(IosXcuiAttributeNames.isIosXcuiNative());
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("Safari").automationName("XCUITest");
        Assert.assertFalse(IosXcuiAttributeNames.isIosXcuiNative());
        SHAFT.Properties.mobile.set().browserName("").automationName("Flutter");
        Assert.assertFalse(IosXcuiAttributeNames.isIosXcuiNative());
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        Assert.assertTrue(IosXcuiAttributeNames.isIosXcuiNative());
        SHAFT.Properties.platform.set().targetPlatform("");
        Assert.assertFalse(IosXcuiAttributeNames.isIosXcuiNative());
    }

    @Test(description = "#7669: scroll container type detection covers table/webkit/textview/blank")
    public void scrollContainerTypeDetection() {
        Assert.assertFalse(IosXcuiAttributeNames.isXcuiScrollContainer(null));
        Assert.assertFalse(IosXcuiAttributeNames.isXcuiScrollContainer(""));
        Assert.assertFalse(IosXcuiAttributeNames.isXcuiScrollContainer("   "));
        Assert.assertTrue(IosXcuiAttributeNames.isXcuiScrollContainer("XCUIElementTypeTable"));
        Assert.assertTrue(IosXcuiAttributeNames.isXcuiScrollContainer("XCUIElementTypeWebKitWebView"));
        Assert.assertTrue(IosXcuiAttributeNames.isXcuiScrollContainer("XCUIElementTypeTextView"));
        Assert.assertFalse(IosXcuiAttributeNames.isXcuiScrollContainer("XCUIElementTypeButton"));
    }

    @Test(description = "#7669: interpret leaves non-scrollable and non-iOS values unchanged")
    public void interpretPassthrough() {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        Assert.assertEquals(IosXcuiAttributeNames.interpret("scrollable", "true"), "true");
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        Assert.assertEquals(IosXcuiAttributeNames.interpret("clickable", "true"), "true");
        Assert.assertEquals(IosXcuiAttributeNames.interpret(null, "x"), "x");
        Assert.assertEquals(IosXcuiAttributeNames.interpret("scrollable", "XCUIElementTypeTable"), "true");
        Assert.assertEquals(IosXcuiAttributeNames.interpret("scrollable", "XCUIElementTypeStaticText"), "false");
    }

    @Test(description = "#7669: readDomAttribute maps aliases and skips empty issued names")
    public void readDomAttributeBranches() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        WebElement element = mock(WebElement.class);
        when(element.getDomAttribute("type")).thenReturn("XCUIElementTypeCollectionView");
        when(element.getDomAttribute("label")).thenReturn("Go");
        Assert.assertEquals(IosXcuiAttributeNames.readDomAttribute(element, "scrollable"), "true");
        Assert.assertEquals(IosXcuiAttributeNames.readDomAttribute(element, "content-desc"), "Go");
        Assert.assertNull(IosXcuiAttributeNames.readDomAttribute(element, "password"));
        Assert.assertNull(IosXcuiAttributeNames.readDomAttribute(element, null));
        verify(element, never()).getDomAttribute("scrollable");
        verify(element, never()).getDomAttribute("password");
        verify(element, never()).getDomAttribute("");
    }

    @Test(description = "#7669: readAttribute propagates driver errors; read soft-swallows them")
    public void readAttributeVsSoftRead() {
        SHAFT.Properties.platform.set().targetPlatform("iOS");
        SHAFT.Properties.mobile.set().browserName("").automationName("XCUITest");
        WebElement element = mock(WebElement.class);
        when(element.getAttribute("name")).thenThrow(new WebDriverException("boom"));
        Assert.expectThrows(WebDriverException.class,
                () -> IosXcuiAttributeNames.readAttribute(element, "resource-id"));
        Assert.assertNull(IosXcuiAttributeNames.read(element, "resource-id"));
        Assert.assertNull(IosXcuiAttributeNames.readAttribute(element, "package"));
    }

    @Test(description = "#7669: Android issued path returns the requested name unchanged")
    public void androidIssuedPassthrough() {
        SHAFT.Properties.platform.set().targetPlatform("Android");
        Assert.assertEquals(IosXcuiAttributeNames.issued("content-desc"), "content-desc");
        Assert.assertEquals(IosXcuiAttributeNames.issued(null), "");
        WebElement element = mock(WebElement.class);
        when(element.getAttribute("password")).thenReturn("true");
        Assert.assertEquals(IosXcuiAttributeNames.readAttribute(element, "password"), "true");
    }
}
