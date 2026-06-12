package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Objects;

/**
 * Locator-not-found request passed to an optional SHAFT Heal provider.
 *
 * @param driver active driver
 * @param originalLocator failed original locator
 * @param action intended action name
 * @param visibilityRequired whether the selected element must be visible and enabled
 * @param frameLocator active frame locator, when applicable
 * @param shadowHostLocator active shadow host locator, when applicable
 * @param shadowContentLocator active shadow content locator, when applicable
 */
public record HealingRequest(
        WebDriver driver,
        By originalLocator,
        String action,
        boolean visibilityRequired,
        By frameLocator,
        By shadowHostLocator,
        By shadowContentLocator) {
    /**
     * Creates a validated request.
     */
    public HealingRequest {
        driver = Objects.requireNonNull(driver, "driver");
        originalLocator = Objects.requireNonNull(originalLocator, "originalLocator");
        action = Objects.requireNonNullElse(action, "UNKNOWN");
    }
}
