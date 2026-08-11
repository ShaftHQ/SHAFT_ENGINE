package com.shaft.gui.playwright.browser;

import com.shaft.gui.driver.ContextActionsContract;

import java.util.List;
import java.util.Objects;

/** Discoverable Playwright browsing-context actions. */
public final class ContextActions implements ContextActionsContract {
    private final BrowserActions browser;

    ContextActions(BrowserActions browser) { this.browser = Objects.requireNonNull(browser, "browser"); }

    @Override public BrowserActions and() { return browser; }
    @Override public String current() { return browser.queryNamespace("context", "current", browser::getContext); }
    @Override public List<String> handles() { return browser.queryNamespace("context", "handles", browser::getContextHandles); }
    @Override public ContextActions switchTo(String context) { browser.performNamespace("context", "switch-to", () -> browser.setContext(context)); return this; }
}
