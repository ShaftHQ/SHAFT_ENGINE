package com.shaft.gui.element;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class TouchActionsDisplayedExactTextTest {
    @Test
    public void ocrHitDoesNotCountUntilAnElementWithThatExactTextIsDisplayed() {
        WebDriver driver = mock(WebDriver.class);
        WebElement hidden = mock(WebElement.class);
        WebElement visible = mock(WebElement.class);
        when(hidden.isDisplayed()).thenReturn(false);
        when(visible.isDisplayed()).thenReturn(true);
        when(driver.findElements(any())).thenReturn(List.of(hidden), List.of(hidden, visible));
        TouchActions actions = new TouchActions(driver);

        Assert.assertFalse(actions.displayedExactText("TAB 1"));
        Assert.assertTrue(actions.displayedExactText("TAB 1"));
    }
}
