package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

import static org.junit.jupiter.api.Assertions.assertEquals;

class EngineServiceLocatorTest {

    @Test
    void idStrategyUsesNativeWebDriverIdLocator() {
        assertEquals(By.id("com.example:id/list").toString(),
                EngineService.getLocator(locatorStrategy.ID, "com.example:id/list").toString());
    }
}
