package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;

import java.util.Objects;

/**
 * Successfully resolved element evidence supplied to an optional provider.
 *
 * @param driver active driver
 * @param originalLocator locator that resolved successfully
 * @param element unique resolved element
 * @param action current action name
 * @param frameLocator active frame locator, when applicable
 * @param shadowHostLocator active shadow host locator, when applicable
 * @param shadowContentLocator active shadow content locator, when applicable
 */
public record HealingObservation(
        WebDriver driver,
        By originalLocator,
        WebElement element,
        String action,
        By frameLocator,
        By shadowHostLocator,
        By shadowContentLocator) {
    /**
     * Creates a validated observation.
     */
    public HealingObservation {
        driver = Objects.requireNonNull(driver, "driver");
        originalLocator = Objects.requireNonNull(originalLocator, "originalLocator");
        element = Objects.requireNonNull(element, "element");
        action = Objects.requireNonNullElse(action, "UNKNOWN");
    }
}
