package com.shaft.consumer;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.api.RestActions;
import com.shaft.tools.internal.support.JavaHelper;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class EngineConsumerTest {

    @Test
    public void engineModuleIsConsumable() {
        assertNotNull(BrowserActions.class.getName(),
            "BrowserActions must be loadable from SHAFT_ENGINE consumer");
    }

    @Test
    public void apiActionsAvailableInEngineConsumer() {
        assertNotNull(RestActions.class.getName(),
            "RestActions must be loadable from SHAFT_ENGINE consumer");
    }

    @Test
    public void seleniumShouldBePresentInEngineConsumer() {
        assertTrue(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must be transitively available in SHAFT_ENGINE consumer");
    }

    @Test
    public void restAssuredShouldBePresentInEngineConsumer() {
        assertTrue(JavaHelper.isClassAvailable("io.restassured.RestAssured"),
            "RestAssured must be transitively available in SHAFT_ENGINE consumer");
    }
}
