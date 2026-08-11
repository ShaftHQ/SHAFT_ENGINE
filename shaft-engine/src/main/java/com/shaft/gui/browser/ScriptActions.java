package com.shaft.gui.browser;

import com.shaft.gui.driver.ScriptActionsContract;

import java.util.Objects;

/** Discoverable Selenium/Appium script execution. */
public final class ScriptActions implements ScriptActionsContract {
    private final BrowserActions browser;

    ScriptActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override public BrowserActions and() { return browser; }
    @Override public Object evaluate(String script) {
        return browser.evaluateScriptNamespace(false, false, script, null);
    }
    @Override public Object evaluate(String script, Object argument) {
        return browser.evaluateScriptNamespace(false, true, script, argument);
    }
    @Override public Object evaluateAsync(String script) {
        return browser.evaluateScriptNamespace(true, false, script, null);
    }
    @Override public Object evaluateAsync(String script, Object argument) {
        return browser.evaluateScriptNamespace(true, true, script, argument);
    }
}
