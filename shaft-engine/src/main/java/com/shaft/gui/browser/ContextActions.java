package com.shaft.gui.browser;

import com.shaft.gui.driver.ContextActionsContract;

import java.util.List;
import java.util.Objects;

/** Discoverable Selenium/Appium native/web context actions. */
public final class ContextActions implements ContextActionsContract {
    private final BrowserActions browser;

    ContextActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override public BrowserActions and() { return browser; }
    @Override public String current() { return browser.currentContextNamespace(); }
    @Override public List<String> handles() { return browser.contextHandlesNamespace(); }
    @Override public ContextActions switchTo(String context) { browser.switchContextNamespace(context); return this; }
}
