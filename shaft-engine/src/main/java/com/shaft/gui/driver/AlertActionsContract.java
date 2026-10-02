package com.shaft.gui.driver;

/**
 * Public contract for alert and prompt helpers.
 */
public interface AlertActionsContract {
    /**
     * Returns whether a JavaScript alert is currently open.
     */
    boolean isAlertPresent();

    /**
     * Accepts the open alert.
     */
    AlertActionsContract acceptAlert();

    /**
     * Dismisses the open alert.
     */
    AlertActionsContract dismissAlert();

    /**
     * Returns the text of the open alert.
     */
    String getAlertText();

    /**
     * Types text into the open prompt alert.
     */
    AlertActionsContract typeIntoPromptAlert(String text);
}
