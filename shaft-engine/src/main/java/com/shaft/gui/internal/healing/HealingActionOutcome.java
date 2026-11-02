package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import java.util.Objects;

/**
 * Outcome of an action that used a recovered element.
 *
 * @param driver active driver
 * @param attemptId healing attempt identifier
 * @param originalLocator failed original locator
 * @param selectedLocator selected replacement locator
 * @param action action name
 * @param successful whether the action completed
 * @param verification post-action verification or explicit unverifiable result
 * @param failure safe failure category or empty text
 */
public record HealingActionOutcome(
        WebDriver driver,
        String attemptId,
        By originalLocator,
        By selectedLocator,
        String action,
        boolean successful,
        String verification,
        String failure) {
    /**
     * Creates a validated outcome.
     */
    public HealingActionOutcome {
        driver = Objects.requireNonNull(driver, "driver");
        attemptId = Objects.requireNonNullElse(attemptId, "");
        originalLocator = Objects.requireNonNull(originalLocator, "originalLocator");
        selectedLocator = Objects.requireNonNull(selectedLocator, "selectedLocator");
        action = Objects.requireNonNullElse(action, "UNKNOWN");
        verification = Objects.requireNonNullElse(verification, "UNVERIFIABLE");
        failure = Objects.requireNonNullElse(failure, "");
    }
}
