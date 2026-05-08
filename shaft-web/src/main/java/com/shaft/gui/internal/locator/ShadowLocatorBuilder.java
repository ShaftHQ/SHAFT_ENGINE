package com.shaft.gui.internal.locator;

import org.openqa.selenium.By;

/**
 * Builds shadow DOM locators by pairing a shadow host locator with a CSS selector
 * for elements inside the shadow root.
 *
 * <p>Thread safety: Uses {@link ThreadLocal} fields to isolate locator state per thread.
 *
 * @see com.shaft.gui.internal.locator.LocatorBuilder#insideShadowDom(org.openqa.selenium.By)
 */
public class ShadowLocatorBuilder {
    public static ThreadLocal<By> shadowDomLocator = new ThreadLocal<>();
    public static ThreadLocal<By> cssSelector = new ThreadLocal<>();

    public ShadowLocatorBuilder(By shadowDomLocator, By cssSelector) {
        ShadowLocatorBuilder.shadowDomLocator.set(shadowDomLocator);
        ShadowLocatorBuilder.cssSelector.set(cssSelector);
    }

    /**
     * Removes all thread-local state held by this builder, preventing memory leaks
     * in long-running or parallel test environments.
     */
    public static void cleanup() {
        shadowDomLocator.remove();
        cssSelector.remove();
    }

    public By build() {
        // this is returned as a placeholder, the real locator is built when the action is being performed and currently supports a limited set of element actions
        return cssSelector.get();
    }
}