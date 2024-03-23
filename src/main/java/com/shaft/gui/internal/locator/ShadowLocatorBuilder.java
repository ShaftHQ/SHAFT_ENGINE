package com.shaft.gui.internal.locator;

import org.openqa.selenium.By;

public class ShadowLocatorBuilder {
    public static ThreadLocal<By> shadowDomLocator = new ThreadLocal<>();
    public static ThreadLocal<By> cssSelector = new ThreadLocal<>();

    public ShadowLocatorBuilder(By shadowDomLocator, By cssSelector) {
        ShadowLocatorBuilder.shadowDomLocator.set(shadowDomLocator);
        ShadowLocatorBuilder.cssSelector.set(cssSelector);
    }

    public By build() {
        // this is returned as a placeholder, the real locator is built when the action is being performed and currently supports a limited set of element actions
        return cssSelector.get();
    }
}