package com.shaft.gui.driver;

/** Categorized native-mobile automation actions. */
public interface MobileActionsContract {
    /**
     * Returns the mobile application actions.
     */
    MobileApplicationActionsContract app();

    /**
     * Returns the mobile device actions.
     */
    MobileDeviceActionsContract device();

    /**
     * Returns the mobile gesture actions.
     */
    MobileGestureActionsContract gestures();

    /**
     * Returns the mobile context actions.
     */
    MobileContextActionsContract context();

    /**
     * Returns the mobile file transfer actions.
     */
    MobileFileActionsContract files();

    /**
     * Returns the mobile log actions.
     */
    MobileLogActionsContract logs();

    /**
     * Returns the mobile biometric actions.
     */
    MobileBiometricActionsContract biometrics();

    /**
     * Returns the mobile performance actions.
     */
    MobilePerformanceActionsContract performance();

    /**
     * Returns the mobile screen recording actions.
     */
    MobileRecordingActionsContract recording();

    /**
     * Returns the mobile evidence actions.
     */
    MobileEvidenceActionsContract evidence();

    /**
     * Returns the driver to continue with other actions.
     */
    DriverContract and();
}
