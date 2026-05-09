package com.shaft.consumer;

import com.shaft.gui.browser.BrowserActions;
import com.shaft.api.RestActions;
import com.shaft.tools.internal.support.JavaHelper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class EngineConsumerTest {

    @Test
    void engineModuleIsConsumable() {
        assertNotNull(BrowserActions.class.getName(),
            "BrowserActions must be loadable from SHAFT_ENGINE consumer");
    }

    @Test
    void apiActionsAvailableInEngineConsumer() {
        assertNotNull(RestActions.class.getName(),
            "RestActions must be loadable from SHAFT_ENGINE consumer");
    }

    @Test
    void seleniumShouldBePresentInEngineConsumer() {
        assertTrue(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver must be transitively available in SHAFT_ENGINE consumer");
    }

    @Test
    void restAssuredShouldBePresentInEngineConsumer() {
        assertTrue(JavaHelper.isClassAvailable("io.restassured.RestAssured"),
            "RestAssured must be transitively available in SHAFT_ENGINE consumer");
    }
}
