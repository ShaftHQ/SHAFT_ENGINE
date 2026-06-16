package com.shaft.gui.internal.natural;

import org.openqa.selenium.WebDriver;

import java.util.List;
import java.util.Objects;

/**
 * Natural-language GUI action request supplied to planners.
 *
 * @param driver active Selenium driver
 * @param intent user intent
 * @param arguments user-provided action arguments
 * @param mobileNativeExecution whether the active session is a mobile native session
 * @param mobileWebExecution whether the active session is a mobile web session
 */
public record NaturalActionRequest(
        WebDriver driver,
        String intent,
        List<Object> arguments,
        boolean mobileNativeExecution,
        boolean mobileWebExecution) {
    /**
     * Creates an immutable request.
     */
    public NaturalActionRequest {
        driver = Objects.requireNonNull(driver, "driver");
        intent = Objects.requireNonNullElse(intent, "").trim();
        arguments = arguments == null ? List.of() : List.copyOf(arguments);
    }
}
