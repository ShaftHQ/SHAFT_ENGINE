package com.shaft.gui.browser;

import com.shaft.gui.driver.BrowserConsoleMessage;
import com.shaft.gui.driver.ConsoleActionsContract;

import java.util.List;
import java.util.Objects;

/** Discoverable Selenium/Appium console observations. */
public final class ConsoleActions implements ConsoleActionsContract {
    private final BrowserActions browser;

    ConsoleActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override public BrowserActions and() { return browser; }
    @Override public List<BrowserConsoleMessage> messages() { return browser.consoleMessages("messages", false); }
    @Override public List<BrowserConsoleMessage> errors() { return browser.consoleMessages("errors", true); }
    @Override public boolean hasErrors() { return !browser.consoleMessages("has-errors", true).isEmpty(); }
    @Override public ConsoleActions clear() { browser.clearConsoleNamespace(); return this; }
}
