package com.shaft.gui.playwright.browser;

import com.shaft.gui.driver.KeyValueStorageActionsContract;
import com.shaft.gui.driver.StorageActionsContract;
import com.shaft.gui.driver.StorageStateActionsContract;

import java.util.Objects;

/** Discoverable Playwright storage actions. */
public final class StorageActions implements StorageActionsContract {
    private final BrowserActions browser;

    StorageActions(BrowserActions browser) {
        this.browser = Objects.requireNonNull(browser, "browser");
    }

    @Override
    public BrowserActions and() {
        return browser;
    }

    @Override
    public StorageStateActionsContract state() {
        return new State();
    }

    @Override
    public KeyValueStorageActionsContract local() {
        return new KeyValue("localStorage");
    }

    @Override
    public KeyValueStorageActionsContract session() {
        return new KeyValue("sessionStorage");
    }

    private final class State implements StorageStateActionsContract {
        @Override
        public StorageActions and() {
            return StorageActions.this;
        }

        @Override
        public State save(String filePath) {
            browser.saveStorageStateNamespace(filePath);
            return this;
        }

        @Override
        public State load(String filePath) {
            browser.loadStorageStateNamespace(filePath);
            return this;
        }
    }

    private final class KeyValue implements KeyValueStorageActionsContract {
        private final String scope;

        private KeyValue(String scope) {
            this.scope = scope;
        }

        @Override
        public StorageActions and() {
            return StorageActions.this;
        }

        @Override
        public String get(String key) {
            return browser.getStorageValue(scope, key);
        }

        @Override
        public KeyValue set(String key, String value) {
            browser.setStorageValue(scope, key, value);
            return this;
        }

        @Override
        public KeyValue remove(String key) {
            browser.removeStorageValue(scope, key);
            return this;
        }

        @Override
        public KeyValue clear() {
            browser.clearStorage(scope);
            return this;
        }
    }
}
