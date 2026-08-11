package com.shaft.gui.driver;

import org.openqa.selenium.ScreenOrientation;

import java.time.Duration;

/** Mobile device state and control actions. */
public interface MobileDeviceActionsContract {
    /** Locks the device immediately. */
    MobileDeviceActionsContract lock();

    /** Locks the device for the requested duration. */
    MobileDeviceActionsContract lock(Duration duration);

    /** Unlocks the device. */
    MobileDeviceActionsContract unlock();

    /** Returns whether the device is currently locked. */
    boolean isLocked();

    /** Returns the current screen orientation. */
    ScreenOrientation orientation();

    /** Rotates the device to the requested screen orientation. */
    MobileDeviceActionsContract orientation(ScreenOrientation orientation);

    /** Returns the current device time using the provider's default format. */
    String time();

    /** Returns the current device time using the provider-supported format. */
    String time(String format);

    /** Returns the current battery level and provider state. */
    MobileBatteryInfo battery();

    /** Returns on-screen keyboard controls. */
    MobileKeyboardActionsContract keyboard();

    /** Returns text clipboard controls. */
    MobileClipboardActionsContract clipboard();

    MobileActionsContract and();
}
