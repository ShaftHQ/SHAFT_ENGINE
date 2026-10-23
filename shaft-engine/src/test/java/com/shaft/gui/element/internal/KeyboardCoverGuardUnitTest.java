package com.shaft.gui.element.internal;

import io.appium.java_client.android.AndroidDriver;
import org.openqa.selenium.Dimension;
import org.openqa.selenium.Rectangle;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class KeyboardCoverGuardUnitTest {
    @Test
    public void centreInTheLowerImeAreaIsCovered() {
        Assert.assertTrue(KeyboardCoverGuard.centreUnderKeyboard(new Rectangle(0, 1800, 100, 400), 2400));
        Assert.assertFalse(KeyboardCoverGuard.centreUnderKeyboard(new Rectangle(0, 200, 100, 400), 2400));
        Assert.assertFalse(KeyboardCoverGuard.centreUnderKeyboard(new Rectangle(0, 200, 100, 400), 0));
    }

    @Test
    public void hidesTheKeyboardOnlyWhenItCoversTheTarget() {
        AndroidDriver android = mock(AndroidDriver.class, RETURNS_DEEP_STUBS);
        when(android.isKeyboardShown()).thenReturn(true);
        when(android.manage().window().getSize()).thenReturn(new Dimension(1080, 2400));
        WebElement low = mock(WebElement.class);
        when(low.getRect()).thenReturn(new Rectangle(0, 1900, 200, 120));
        KeyboardCoverGuard.hideIfCovering(android, low);
        verify(android).hideKeyboard();

        AndroidDriver other = mock(AndroidDriver.class, RETURNS_DEEP_STUBS);
        when(other.isKeyboardShown()).thenReturn(true);
        when(other.manage().window().getSize()).thenReturn(new Dimension(1080, 2400));
        WebElement high = mock(WebElement.class);
        when(high.getRect()).thenReturn(new Rectangle(0, 300, 200, 120));
        KeyboardCoverGuard.hideIfCovering(other, high);
        verify(other, never()).hideKeyboard();
    }

    @Test
    public void nonAndroidDriversAreIgnored() {
        KeyboardCoverGuard.hideIfCovering(mock(WebDriver.class), mock(WebElement.class));
    }
}
