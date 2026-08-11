package com.shaft.gui.driver;

/** Android emulator fingerprint simulation actions. */
public interface MobileFingerprintActionsContract {
    /**
     * Simulates authentication using one of the emulator's enrolled fingerprints.
     *
     * @param fingerprintId enrolled fingerprint identifier from 1 through 10
     * @return this fingerprint action namespace
     * @throws IllegalArgumentException when the identifier is outside 1 through 10
     * @throws UnsupportedOperationException when the live session lacks Android fingerprint support
     */
    MobileFingerprintActionsContract authenticate(int fingerprintId);

    /** Returns the owning biometric namespace. */
    MobileBiometricActionsContract and();
}
