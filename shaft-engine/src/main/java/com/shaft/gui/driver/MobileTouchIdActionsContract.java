package com.shaft.gui.driver;

/** iOS Simulator Touch ID simulation actions. */
public interface MobileTouchIdActionsContract {
    /** Simulates a successful Touch ID match. */
    MobileTouchIdActionsContract match();

    /** Simulates a rejected Touch ID match. */
    MobileTouchIdActionsContract reject();

    /** Enables Touch ID enrollment for the Simulator. */
    MobileTouchIdActionsContract enroll();

    /** Disables Touch ID enrollment for the Simulator. */
    MobileTouchIdActionsContract unenroll();

    /** Returns the owning biometric namespace. */
    MobileBiometricActionsContract and();
}
