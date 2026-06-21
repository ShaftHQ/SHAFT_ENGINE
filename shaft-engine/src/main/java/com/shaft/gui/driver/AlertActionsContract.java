package com.shaft.gui.driver;

/**
 * Public contract for alert and prompt helpers.
 */
public interface AlertActionsContract {
    boolean isAlertPresent();

    AlertActionsContract acceptAlert();

    AlertActionsContract dismissAlert();

    String getAlertText();

    AlertActionsContract typeIntoPromptAlert(String text);
}
