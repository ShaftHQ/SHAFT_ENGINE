package com.shaft.gui.driver;

/** Text clipboard controls for a mobile device. */
public interface MobileClipboardActionsContract {
    /** Returns the current clipboard text. */
    String text();

    /** Replaces the clipboard text. */
    MobileClipboardActionsContract text(String text);

    /** Returns the owning device namespace. */
    MobileDeviceActionsContract and();
}
