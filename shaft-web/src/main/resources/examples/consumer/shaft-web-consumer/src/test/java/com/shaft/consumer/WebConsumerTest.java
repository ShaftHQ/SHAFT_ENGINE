package com.shaft.consumer;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.tools.internal.support.JavaHelper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class WebConsumerTest {

    @Test
    void webModuleIsConsumable() {
        assertNotNull(BrowserActions.class.getName(),
            "BrowserActions must be loadable from shaft-web consumer");
    }

    @Test
    void seleniumShouldBePresentInWebConsumer() {
        assertTrue(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must be transitively available in shaft-web consumer");
    }

    @Test
    void appiumShouldBePresentInWebConsumer() {
        assertTrue(JavaHelper.isClassAvailable("io.appium.java_client.AppiumDriver"),
            "AppiumDriver must be transitively available in shaft-web consumer");
    }
}
