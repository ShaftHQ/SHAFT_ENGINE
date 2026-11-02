package com.shaft.gui.driver;

/** Portable cookies, localStorage, and sessionStorage snapshots. */
public interface StorageStateActionsContract {
    /** @return the parent storage namespace */
    StorageActionsContract and();

    /** Saves the current storage state to the requested JSON path. */
    StorageStateActionsContract save(String filePath);

    /** Loads storage state from the requested JSON path into the current origin. */
    StorageStateActionsContract load(String filePath);
}
