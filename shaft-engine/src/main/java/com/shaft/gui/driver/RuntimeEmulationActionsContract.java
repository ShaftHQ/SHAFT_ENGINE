package com.shaft.gui.driver;

/** Browser runtime identity and scripting emulation controls. */
public interface RuntimeEmulationActionsContract {
    /**
     * Overrides the user agent for the current BiDi browsing context. Playwright contexts use
     * {@code playwright.userAgent} before session creation.
     */
    RuntimeEmulationActionsContract userAgent(String userAgent);

    /** Clears the user-agent override. */
    RuntimeEmulationActionsContract clearUserAgent();

    /**
     * Disables scripting for the current BiDi browsing context. WebDriver BiDi does not provide a
     * symmetric force-enable override; call {@link #clearScriptingOverride()} to restore the context default.
     * Playwright contexts use {@code playwright.javaScriptEnabled} before session creation.
     */
    RuntimeEmulationActionsContract disableScripting();

    /** Clears SHAFT's scripting override. */
    RuntimeEmulationActionsContract clearScriptingOverride();

    /** @return owning emulation facade */
    EmulationActionsContract and();
}
