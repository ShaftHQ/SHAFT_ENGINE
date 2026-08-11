package com.shaft.gui.driver;

/** Mobile biometric simulation actions. */
public interface MobileBiometricActionsContract {
    /** Returns Android emulator fingerprint simulation actions. */
    MobileFingerprintActionsContract fingerprint();

    /** Returns iOS Simulator Touch ID simulation actions. */
    MobileTouchIdActionsContract touchId();

    /** Returns the owning mobile namespace. */
    MobileActionsContract and();
}
