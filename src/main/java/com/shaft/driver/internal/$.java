package com.shaft.driver.internal;

import com.shaft.driver.SHAFT;

/**
 * A shorthand alias for the {@link com.shaft.driver.SHAFT} class, providing the same unified
 * entry point to all SHAFT framework capabilities (GUI, API, CLI, DB, Validations, TestData,
 * and Report) under a concise {@code $} identifier.
 *
 * <p>Usage example:
 * <pre>
 *     $.GUI.WebDriver driver = new $.GUI.WebDriver();
 * </pre>
 */
@SuppressWarnings("unused")
public class $ extends SHAFT {

    /**
     * Default constructor for the {@code $} shorthand alias.
     * Delegates to the {@link com.shaft.driver.SHAFT} superclass constructor.
     */
    public $() {
        super();
    }
}
