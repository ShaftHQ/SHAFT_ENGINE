package com.shaft.gui.driver;

/**
 * Public contract for alert and prompt helpers.
 */
public interface AlertActions {
    boolean isAlertPresent();

    AlertActions acceptAlert();

    AlertActions dismissAlert();

    String getAlertText();

    AlertActions typeIntoPromptAlert(String text);
}
