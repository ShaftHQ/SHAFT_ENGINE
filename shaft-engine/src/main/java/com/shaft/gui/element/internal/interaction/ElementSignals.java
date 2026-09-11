package com.shaft.gui.element.internal.interaction;

/**
 * Cheap DOM signals for {@link ElementClassifier} — shared by Selenium {@code WebElement}
 * and Playwright locator evaluate results (epic #5732).
 */
public record ElementSignals(
        String tagName,
        String type,
        String role,
        String contentEditable,
        String isContentEditableProperty,
        String disabled,
        String readonly,
        String ariaDisabled,
        String dataMask,
        String dataInputmask,
        String mask,
        String className,
        String autocomplete
) {
    public static ElementSignals of(
            String tagName,
            String type,
            String role,
            String contentEditable,
            String isContentEditableProperty,
            String disabled,
            String readonly,
            String ariaDisabled) {
        return new ElementSignals(
                tagName, type, role, contentEditable, isContentEditableProperty,
                disabled, readonly, ariaDisabled, null, null, null, null, null);
    }
}
