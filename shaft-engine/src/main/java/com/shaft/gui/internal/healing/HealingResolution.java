package com.shaft.gui.internal.healing;

import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

import java.util.List;
import java.util.Objects;

/**
 * Safe resolution returned by an optional SHAFT Heal provider.
 *
 * @param attemptId provider-owned safe correlation identifier
 * @param elements selected elements; successful recovery must contain exactly one
 * @param selectedLocator explainable replacement locator
 */
public record HealingResolution(String attemptId, List<WebElement> elements, By selectedLocator) {
    /**
     * Creates an immutable resolution.
     */
    public HealingResolution {
        attemptId = Objects.requireNonNullElse(attemptId, "");
        elements = elements == null ? List.of() : List.copyOf(elements);
        selectedLocator = Objects.requireNonNull(selectedLocator, "selectedLocator");
    }
}
