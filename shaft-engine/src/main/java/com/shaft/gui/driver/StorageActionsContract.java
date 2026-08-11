package com.shaft.gui.driver;

/** Browser storage actions grouped by persistence scope. */
public interface StorageActionsContract {
    /** @return the parent browser actions facade */
    BrowserActionsContract and();

    /** @return portable cookies and web-storage snapshot actions */
    StorageStateActionsContract state();

    /** @return localStorage actions for the current origin */
    KeyValueStorageActionsContract local();

    /** @return sessionStorage actions for the current page */
    KeyValueStorageActionsContract session();
}
