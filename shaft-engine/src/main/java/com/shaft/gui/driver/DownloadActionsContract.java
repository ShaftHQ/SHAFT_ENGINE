package com.shaft.gui.driver;

import java.util.List;
import java.util.function.Predicate;

/** Cohesive browser-download discovery, waiting, persistence, and cleanup actions. */
public interface DownloadActionsContract {
    /** @return immutable completed/observed downloads, oldest first */
    List<BrowserDownload> all();

    /** @return newest download, or throws when none is available */
    BrowserDownload latest();

    /** Arms download observation before running the trigger, then returns the first new download. */
    BrowserDownload waitFor(Runnable trigger);

    /** Arms filtered observation before the trigger; the predicate may inspect download value metadata. */
    BrowserDownload waitFor(Predicate<BrowserDownload> predicate, Runnable trigger);

    /** Deletes all downloads owned by the live backend/session. */
    DownloadActionsContract clear();

    /** @return owning browser facade */
    BrowserActionsContract and();
}
