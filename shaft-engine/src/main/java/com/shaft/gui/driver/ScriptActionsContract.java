package com.shaft.gui.driver;

/**
 * Script execution in the active browser realm using one optional argument.
 *
 * <p>Pass a {@link java.util.List} or {@link java.util.Map} as the single argument when a script needs
 * several values. Selenium exposes it as {@code arguments[0]}; Playwright passes it as its native
 * evaluation argument. Async Selenium scripts receive the completion callback after that optional argument,
 * while Playwright awaits a returned promise.</p>
 */
public interface ScriptActionsContract {
    /** @return the parent browser actions facade */
    BrowserActionsContract and();

    /**
     * Executes a script without an evaluation argument.
     *
     * @param script browser-realm expression or script body
     * @return deserialized result
     */
    Object evaluate(String script);

    /**
     * Executes a script with one evaluation argument.
     *
     * @param script browser-realm expression or script body
     * @param argument one serializable value, list, map, or supported native handle
     * @return deserialized result
     */
    Object evaluate(String script, Object argument);

    /**
     * Executes an asynchronous script without an evaluation argument.
     *
     * @param script Selenium callback-style body or Playwright promise-returning expression
     * @return deserialized resolved result
     */
    Object evaluateAsync(String script);

    /**
     * Executes an asynchronous script with one evaluation argument.
     *
     * @param script Selenium callback-style body or Playwright promise-returning expression
     * @param argument one serializable value, list, map, or supported native handle
     * @return deserialized resolved result
     */
    Object evaluateAsync(String script, Object argument);
}
