package com.shaft.gui.internal.locator;

import org.openqa.selenium.By;

public class ShadowLocatorBuilder {
    public static By shadowDomLocator;
    public static By cssSelector;

    public ShadowLocatorBuilder(By shadowDomLocator, By cssSelector) {
        ShadowLocatorBuilder.shadowDomLocator = shadowDomLocator;
        ShadowLocatorBuilder.cssSelector = cssSelector;
    }

    public By build() {
        // this is returned as a placeholder, the real locator is built when the action is being performed and currently supports a limited set of element actions
        return cssSelector;
    }
}