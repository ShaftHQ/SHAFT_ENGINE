package com.shaft.gui.playwright.browser;

import com.shaft.gui.driver.DialogActionsContract;
import com.shaft.gui.driver.DialogObservationContract;
import com.shaft.gui.driver.CurrentDialogActionsContract;
import com.shaft.gui.driver.NextDialogActionsContract;

import java.util.Objects;

/** Discoverable Playwright dialog actions. */
public final class DialogActions implements DialogActionsContract {
    private final BrowserActions browser;

    DialogActions(BrowserActions browser) { this.browser = Objects.requireNonNull(browser, "browser"); }

    @Override public BrowserActions and() { return browser; }
    @Override public DialogObservationContract observation() { return new Observation(); }
    @Override public CurrentDialogActionsContract current() { browser.performNamespace("dialog", "current-unsupported", () -> { throw new UnsupportedOperationException("Playwright dialogs are callback-driven; configure next() before the triggering action."); }); throw new IllegalStateException("unreachable"); }
    @Override public NextDialogActionsContract next() { return new Next(); }

    private final class Observation implements DialogObservationContract {
        @Override public DialogActions and() { return DialogActions.this; }
        @Override public boolean wasSeen() { return browser.queryNamespace("dialog", "was-seen", browser::isAlertPresent); }
        @Override public String lastText() { return browser.queryNamespace("dialog", "last-text", browser::getAlertText); }
    }

    private final class Next implements NextDialogActionsContract {
        @Override public DialogActions and() { return DialogActions.this; }
        @Override public Next accept() { browser.performNamespace("dialog-policy", "arm-accept-next", browser::acceptAlert); return this; }
        @Override public Next dismiss() { browser.performNamespace("dialog-policy", "arm-dismiss-next", browser::dismissAlert); return this; }
        @Override public Next type(String text) { browser.performNamespace("dialog-policy", "arm-type-next", () -> browser.typeIntoPromptAlert(text)); return this; }
    }
}
