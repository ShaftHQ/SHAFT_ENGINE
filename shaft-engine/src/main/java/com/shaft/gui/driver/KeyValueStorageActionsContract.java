package com.shaft.gui.driver;

/** String key/value actions for one browser web-storage scope. */
public interface KeyValueStorageActionsContract {
    /** @return the parent storage namespace */
    StorageActionsContract and();

    /** Returns a stored value, or {@code null} when the key is absent. */
    String get(String key);

    /** Creates or replaces a value. */
    KeyValueStorageActionsContract set(String key, String value);

    /** Removes a value when present. */
    KeyValueStorageActionsContract remove(String key);

    /** Clears every value in this storage scope. */
    KeyValueStorageActionsContract clear();
}
