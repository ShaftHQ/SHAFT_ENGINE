package com.shaft.gui.playwright.browser;

import com.shaft.gui.driver.AuthenticationActionsContract;

import java.util.Objects;

/** Discoverable Playwright HTTP authentication actions. */
public final class AuthenticationActions implements AuthenticationActionsContract {
    private final BrowserActions browser;

    AuthenticationActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override public BrowserActions and() { return browser; }
    @Override public AuthenticationActions basic(String username, String password) {
        browser.registerBasicAuthenticationNamespace(null, username, password);
        return this;
    }
    @Override public AuthenticationActions basicFor(String origin, String username, String password) {
        browser.registerBasicAuthenticationNamespace(origin, username, password);
        return this;
    }
    @Override public BrowserActions navigateTo(String url, String username, String password) {
        browser.navigateWithBasicAuthenticationNamespace(url, username, password);
        return browser;
    }
    @Override public AuthenticationActions clear() {
        browser.clearAuthenticationNamespace();
        return this;
    }
}
