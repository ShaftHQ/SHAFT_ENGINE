package com.shaft.validation.internal;

import com.shaft.validation.ValidationEnums;
import org.openqa.selenium.By;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/** #6823: an animated element is compared only after two consecutive frames match. */
public class VisualStabilityWaitUnitTest {
    private static final By LOCATOR = By.id("splash");

    private WebDriverElementValidationsBuilder builder(WebElement element) {
        WebDriver driver = mock(WebDriver.class);
        when(driver.findElement(LOCATOR)).thenReturn(element);
        return new WebDriverElementValidationsBuilder(ValidationEnums.ValidationCategory.HARD_ASSERT, driver,
                LOCATOR, new StringBuilder());
    }

    @Test
    public void settlesOnceTwoConsecutiveFramesMatch() {
        WebElement element = mock(WebElement.class);
        when(element.getScreenshotAs(OutputType.BYTES))
                .thenReturn(new byte[]{1}, new byte[]{2}, new byte[]{3}, new byte[]{3});

        Assert.assertTrue(builder(element).waitForVisualStability(Duration.ofSeconds(5), Duration.ofMillis(1)));
        verify(element, times(4)).getScreenshotAs(OutputType.BYTES);
    }

    @Test
    public void givesUpAtTheTimeoutWhenTheElementKeepsAnimating() {
        WebElement element = mock(WebElement.class);
        byte[][] frames = new byte[1000][];
        for (int i = 0; i < frames.length; i++) {
            frames[i] = new byte[]{(byte) i, (byte) (i >> 8)};
        }
        when(element.getScreenshotAs(OutputType.BYTES)).thenReturn(frames[0], java.util.Arrays.copyOfRange(frames, 1, frames.length));

        Assert.assertFalse(builder(element).waitForVisualStability(Duration.ofMillis(50), Duration.ofMillis(5)));
    }
}
