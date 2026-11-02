package com.shaft.gui.internal.locator;

/**
 * Strategy enum for SHAFT's relation/builder XPath-vs-CSS generation path
 * ({@link LocatorBuilder}), not a full {@link org.openqa.selenium.By} factory catalog.
 * Prefer {@link Locator} / {@code SHAFT.GUI.Locator} for web, mobile, and Flutter finders.
 */
public enum Locators {
    XPATH, CSS
}
