package com.shaft.gui.playwright.browser;

import com.shaft.gui.driver.PermissionActionsContract;

import java.util.Objects;

/** Discoverable Playwright browser-context permission controls. */
public final class PermissionActions implements PermissionActionsContract {
    private final BrowserActions browser;

    PermissionActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override public BrowserActions and() { return browser; }
    @Override public PermissionActions grant(String... permissions) {
        browser.setPermissionsNamespace("grant", null, permissions);
        return this;
    }
    @Override public PermissionActions grantFor(String origin, String... permissions) {
        browser.setPermissionsNamespace("grant", origin, permissions);
        return this;
    }
    @Override public PermissionActions denyFor(String origin, String... permissions) {
        browser.setPermissionsNamespace("deny", origin, permissions);
        return this;
    }
    @Override public PermissionActions promptFor(String origin, String... permissions) {
        browser.setPermissionsNamespace("prompt", origin, permissions);
        return this;
    }
    @Override public PermissionActions clear() {
        browser.clearPermissionsNamespace();
        return this;
    }
}
