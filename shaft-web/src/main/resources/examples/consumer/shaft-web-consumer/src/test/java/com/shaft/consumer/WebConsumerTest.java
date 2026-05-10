package com.shaft.consumer;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.tools.internal.support.JavaHelper;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class WebConsumerTest {

    @Test
    public void webModuleIsConsumable() {
        assertNotNull(BrowserActions.class.getName(),
            "BrowserActions must be loadable from shaft-web consumer");
    }

    @Test
    public void seleniumShouldBePresentInWebConsumer() {
        assertTrue(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must be transitively available in shaft-web consumer");
    }

    @Test
    public void appiumShouldBePresentInWebConsumer() {
        assertTrue(JavaHelper.isClassAvailable("io.appium.java_client.AppiumDriver"),
            "AppiumDriver must be transitively available in shaft-web consumer");
    }
}
