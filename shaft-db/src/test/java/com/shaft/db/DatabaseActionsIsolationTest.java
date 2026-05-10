package com.shaft.db;

import com.shaft.tools.internal.support.JavaHelper;
import org.testng.annotations.Test;

import static org.testng.Assert.assertFalse;

public class DatabaseActionsIsolationTest {

    @Test
    public void shaftWebShouldBeAbsentFromShaftDbScope() {
        assertFalse(JavaHelper.isClassAvailable("com.shaft.gui.browser.BrowserActions"),
            "BrowserActions (shaft-web) must not be on shaft-db test classpath");
    }

    @Test
    public void shaftApiRestActionsShouldBeAbsentFromShaftDbScope() {
        assertFalse(JavaHelper.isClassAvailable("com.shaft.api.RestActions"),
            "RestActions (shaft-api) must not be on shaft-db test classpath");
    }

    @Test
    public void seleniumWebDriverShouldBeAbsentFromShaftDbScope() {
        assertFalse(JavaHelper.isClassAvailable("org.openqa.selenium.WebDriver"),
            "WebDriver (Selenium) must not be on shaft-db test classpath — shaft-web is not a dep");
    }
}
