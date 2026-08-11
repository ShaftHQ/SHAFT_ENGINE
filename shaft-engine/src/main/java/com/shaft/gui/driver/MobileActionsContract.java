package com.shaft.gui.driver;

/** Categorized native-mobile automation actions. */
public interface MobileActionsContract {
    MobileApplicationActionsContract app();

    MobileDeviceActionsContract device();

    MobileGestureActionsContract gestures();

    MobileContextActionsContract context();

    MobileFileActionsContract files();

    MobileLogActionsContract logs();

    MobileBiometricActionsContract biometrics();

    MobilePerformanceActionsContract performance();

    MobileRecordingActionsContract recording();

    MobileEvidenceActionsContract evidence();

    DriverContract and();
}
