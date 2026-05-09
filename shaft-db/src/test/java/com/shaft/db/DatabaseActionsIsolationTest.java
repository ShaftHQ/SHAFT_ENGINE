package com.shaft.db;

import com.shaft.tools.internal.support.JavaHelper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

class DatabaseActionsIsolationTest {

    @Test
    void shaftWebShouldBeAbsentFromShaftDbScope() {
        assertFalse(JavaHelper.isClassAvailable("com.shaft.gui.browser.BrowserActions"),
            "BrowserActions (shaft-web) must not be on shaft-db test classpath");
    }

    @Test
    void shaftApiRestActionsShouldBeAbsentFromShaftDbScope() {
        assertFalse(JavaHelper.isClassAvailable("com.shaft.api.RestActions"),
            "RestActions (shaft-api) must not be on shaft-db test classpath");
    }

    @Test
    void seleniumWebDriverShouldBeAbsentFromShaftDbScope() {
        assertFalse(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver (Selenium) must not be on shaft-db test classpath — shaft-web is not a dep");
    }
}
