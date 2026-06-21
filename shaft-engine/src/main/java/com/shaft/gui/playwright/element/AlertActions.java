package com.shaft.gui.playwright.element;

import com.shaft.gui.playwright.internal.PlaywrightSession;

public class AlertActions implements com.shaft.gui.driver.AlertActionsContract {
    private final PlaywrightSession session;

    public AlertActions(PlaywrightSession session) {
        this.session = session;
    }

    @Override
    public boolean isAlertPresent() {
        return session.isDialogSeen();
    }

    @Override
    public AlertActions acceptAlert() {
        session.acceptNextDialog();
        return this;
    }

    @Override
    public AlertActions dismissAlert() {
        session.dismissNextDialog();
        return this;
    }

    @Override
    public String getAlertText() {
        return session.lastDialogText();
    }

    @Override
    public AlertActions typeIntoPromptAlert(String text) {
        session.typeIntoNextPrompt(text);
        return this;
    }
}
