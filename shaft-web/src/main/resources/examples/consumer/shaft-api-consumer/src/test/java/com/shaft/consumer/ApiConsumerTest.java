package com.shaft.consumer;

import com.shaft.api.validation.ApiValidations;
import com.shaft.tools.internal.support.JavaHelper;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class ApiConsumerTest {

    @Test
    public void apiModuleIsConsumable() {
        assertNotNull(ApiValidations.class.getName(),
            "ApiValidations must be loadable from shaft-api consumer");
    }

    @Test
    public void seleniumShouldBeAbsentFromApiOnlyConsumer() {
        assertFalse(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must not be transitively pulled in by shaft-api");
    }

    @Test
    public void appiumShouldBeAbsentFromApiOnlyConsumer() {
        assertFalse(JavaHelper.isClassAvailable("io.appium.java_client.AppiumDriver"),
            "AppiumDriver must not be transitively pulled in by shaft-api");
    }
}
