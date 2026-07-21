package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EngineServiceLocatorTest {

    @Test
    void idStrategyUsesNativeWebDriverIdLocator() {
        assertEquals(By.id("com.example:id/list").toString(),
                EngineService.getLocator(locatorStrategy.ID, "com.example:id/list").toString());
    }

    @Test
    void cssSelectorStrategyUsesNativeWebDriverCssLocator() {
        assertEquals(By.cssSelector(".list-item").toString(),
                EngineService.getLocator(locatorStrategy.CSSSELECTOR, ".list-item").toString());
    }

    @Test
    void cssAliasStrategyUsesNativeWebDriverCssLocator() {
        assertEquals(By.cssSelector(".list-item").toString(),
                EngineService.getLocator(locatorStrategy.CSS, ".list-item").toString());
    }

    @Test
    void selectorAliasStrategyUsesNativeWebDriverCssLocator() {
        assertEquals(By.cssSelector(".list-item").toString(),
                EngineService.getLocator(locatorStrategy.SELECTOR, ".list-item").toString());
    }

    @Test
    void xpathStrategyUsesNativeWebDriverXpathLocator() {
        assertEquals(By.xpath("//div").toString(),
                EngineService.getLocator(locatorStrategy.XPATH, "//div").toString());
    }

    @Test
    void androidUiAutomatorStrategyUsesAppiumLocator() {
        assertEquals(io.appium.java_client.AppiumBy.androidUIAutomator("new UiSelector().text(\"OK\")").toString(),
                EngineService.getLocator(locatorStrategy.ANDROID_UIAUTOMATOR, "new UiSelector().text(\"OK\")")
                        .toString());
    }

    @Test
    void iosPredicateStrategyUsesAppiumLocator() {
        assertEquals(io.appium.java_client.AppiumBy.iOSNsPredicateString("type == 'XCUIElementTypeButton'")
                        .toString(),
                EngineService.getLocator(locatorStrategy.IOS_PREDICATE, "type == 'XCUIElementTypeButton'")
                        .toString());
    }

    @Test
    void iosClassChainStrategyUsesAppiumLocator() {
        assertEquals(io.appium.java_client.AppiumBy.iOSClassChain("**/XCUIElementTypeButton").toString(),
                EngineService.getLocator(locatorStrategy.IOS_CLASS_CHAIN, "**/XCUIElementTypeButton").toString());
    }

    @Test
    void nameStrategyBuildsANameAttributeLocator() {
        By locator = EngineService.getLocator(locatorStrategy.NAME, "email");

        assertTrue(locator.toString().contains("name"), locator.toString());
        assertTrue(locator.toString().contains("email"), locator.toString());
    }

    @Test
    void tagNameStrategyBuildsATagNameLocator() {
        By locator = EngineService.getLocator(locatorStrategy.TAGNAME, "button");

        assertTrue(locator.toString().contains("button"), locator.toString());
    }

    @Test
    void classNameStrategyBuildsAClassAttributeLocator() {
        By locator = EngineService.getLocator(locatorStrategy.CLASSNAME, "primary-button");

        assertTrue(locator.toString().contains("class"), locator.toString());
        assertTrue(locator.toString().contains("primary-button"), locator.toString());
    }
}
