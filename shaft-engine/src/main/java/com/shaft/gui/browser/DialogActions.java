package com.shaft.gui.browser;

import com.shaft.gui.driver.DialogActionsContract;
import com.shaft.gui.driver.DialogObservationContract;
import com.shaft.gui.driver.CurrentDialogActionsContract;
import com.shaft.gui.driver.NextDialogActionsContract;

import java.util.Objects;

/** Discoverable Selenium/Appium browser-dialog actions. */
public final class DialogActions implements DialogActionsContract {
    private final BrowserActions browser;

    DialogActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override public BrowserActions and() { return browser; }
    @Override public DialogObservationContract observation() { browser.performNamespace("dialog", "observation-unsupported", () -> { throw new UnsupportedOperationException("Selenium/Appium does not retain dialog history; use current()."); }); throw new IllegalStateException("unreachable"); }
    @Override public CurrentDialogActionsContract current() { return new Current(); }
    @Override public NextDialogActionsContract next() { browser.performNamespace("dialog", "next-unsupported", () -> { throw new UnsupportedOperationException("Selenium/Appium cannot pre-arm the next dialog; use current()."); }); throw new IllegalStateException("unreachable"); }

    private final class Current implements CurrentDialogActionsContract {
        @Override public DialogActions and() { return DialogActions.this; }
        @Override public boolean isPresent() { return browser.queryNamespace("dialog", "is-present", browser::isCurrentAlertPresent); }
        @Override public String text() { return browser.queryNamespace("dialog", "text", browser::getAlertText); }
        @Override public Current accept() { browser.performNamespace("dialog", "accept-current", browser::acceptAlert); return this; }
        @Override public Current dismiss() { browser.performNamespace("dialog", "dismiss-current", browser::dismissAlert); return this; }
        @Override public Current type(String text) { browser.performNamespace("dialog", "type-current", () -> browser.typeIntoPromptAlert(text)); return this; }
    }
}
