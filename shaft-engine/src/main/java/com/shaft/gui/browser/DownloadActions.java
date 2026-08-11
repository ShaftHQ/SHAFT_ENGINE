package com.shaft.gui.browser;

import com.shaft.gui.driver.BrowserActionsContract;
import com.shaft.gui.driver.BrowserDownload;
import com.shaft.gui.driver.DownloadActionsContract;

import java.util.List;
import java.util.function.Predicate;

/** Selenium/Appium browser-download facade. */
public final class DownloadActions implements DownloadActionsContract {
    private final BrowserActions browser;

    DownloadActions(BrowserActions browser) {
        this.browser = browser;
    }

    @Override
    public List<BrowserDownload> all() {
        return browser.downloadedFilesNamespace(this);
    }

    @Override
    public BrowserDownload latest() {
        return browser.latestDownloadedFileNamespace(this);
    }

    @Override
    public BrowserDownload waitFor(Runnable trigger) {
        return waitFor(ignored -> true, trigger);
    }

    @Override
    public BrowserDownload waitFor(Predicate<BrowserDownload> predicate, Runnable trigger) {
        return browser.waitForDownloadedFileNamespace(this, predicate, trigger);
    }

    @Override
    public DownloadActions clear() {
        browser.clearDownloadedFilesNamespace();
        return this;
    }

    @Override
    public BrowserActionsContract and() {
        return browser;
    }

}
