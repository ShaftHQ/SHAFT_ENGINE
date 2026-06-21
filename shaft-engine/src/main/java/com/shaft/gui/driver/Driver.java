package com.shaft.gui.driver;

import com.shaft.gui.element.TouchActions;

/**
 * Public contract for SHAFT GUI automation sessions.
 */
public interface Driver {

    /**
     * Returns the primary native automation object for the active backend.
     *
     * @return the live backend driver object
     */
    Object getDriver();

    /**
     * Alias for {@link #getDriver()} that reads naturally when callers handle
     * the session through the generic GUI driver contract.
     *
     * @return the live backend driver object
     */
    default Object getNativeDriver() {
        return getDriver();
    }

    /**
     * Returns the native backend context object when the backend exposes one.
     *
     * @return the active backend context
     */
    default Object getNativeContext() {
        return getNativeDriver();
    }

    /**
     * Returns browser-level actions and context helpers.
     *
     * @return browser actions facade
     */
    BrowserActionsContract browser();

    /**
     * Returns element-level actions and assertions.
     *
     * @return element actions facade
     */
    ElementActionsContract element();

    /**
     * Returns touch-action helpers for mobile scenarios.
     *
     * @return touch actions facade
     */
    TouchActions touch();

    /**
     * Returns alert/prompt helpers.
     *
     * @return alert actions facade
     */
    AlertActionsContract alert();

    /**
     * Returns hard-assertion builder methods scoped to this session.
     *
     * @return driver assertions facade
     */
    DriverAssertions assertThat();

    /**
     * Returns soft-verification builder methods scoped to this session.
     *
     * @return driver verifications facade
     */
    DriverVerifications verifyThat();

    /**
     * Runs a natural-language action against the current context.
     *
     * @param intent action intent
     * @param args   action arguments
     * @return this driver for fluent chaining
     */
    Driver act(String intent, Object... args);

    /**
     * Terminates the underlying driver session.
     */
    void quit();
}
