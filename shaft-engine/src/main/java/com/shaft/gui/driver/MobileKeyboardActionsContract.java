package com.shaft.gui.driver;

/** On-screen mobile keyboard controls. */
public interface MobileKeyboardActionsContract {
    /** Returns whether the on-screen keyboard is visible. */
    boolean isShown();

    /** Hides the on-screen keyboard. */
    MobileKeyboardActionsContract hide();

    /** Returns the owning device namespace. */
    MobileDeviceActionsContract and();
}
