package testPackage.appium;

import org.openqa.selenium.WebDriverException;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class IosAccessibilityFocusUnitTest {

    @Test
    public void prefersFocusedWhenHasKeyboardFocusIsUnknownOnXcuiTest() {
        WebElement element = mock(WebElement.class);
        when(element.getAttribute("hasKeyboardFocus"))
                .thenThrow(new WebDriverException("unknown attribute: hasKeyboardFocus"));
        when(element.getAttribute("focused")).thenReturn("true");
        when(element.getAttribute("wdFocused")).thenReturn("0");

        Assert.assertTrue(IOSBasicInteractionsTest.isAccessibilityFocused(element));
    }

    @Test
    public void usesWdFocusedWhenFocusedIsMissing() {
        WebElement element = mock(WebElement.class);
        when(element.getAttribute("hasKeyboardFocus"))
                .thenThrow(new WebDriverException("unknown attribute"));
        when(element.getAttribute("focused")).thenThrow(new WebDriverException("unknown attribute"));
        when(element.getAttribute("wdFocused")).thenReturn("1");

        Assert.assertTrue(IOSBasicInteractionsTest.isAccessibilityFocused(element));
    }
}
