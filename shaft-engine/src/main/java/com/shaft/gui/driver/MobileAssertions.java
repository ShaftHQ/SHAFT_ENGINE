package com.shaft.gui.driver;

import com.shaft.validation.internal.NativeValidationsBuilder;

/**
 * Public contract for focused hard or soft validations of current mobile-session values.
 */
public interface MobileAssertions {
    /** Starts a validation against the current native or web-view context name. */
    default NativeValidationsBuilder currentContextValue() {
        throw unsupported("currentContextValue");
    }

    /** Starts a validation against the number of available native and web-view contexts. */
    default NativeValidationsBuilder contextCountValue() {
        throw unsupported("contextCountValue");
    }

    /**
     * Starts a validation against whether an application is installed.
     *
     * @param appId application package or bundle identifier
     */
    default NativeValidationsBuilder appInstalledValue(String appId) {
        throw unsupported("appInstalledValue");
    }

    /**
     * Starts a validation against an application's current lifecycle state.
     *
     * @param appId application package or bundle identifier
     */
    default NativeValidationsBuilder appStateValue(String appId) {
        throw unsupported("appStateValue");
    }

    /** Starts a validation against whether the device is locked. */
    default NativeValidationsBuilder deviceLockedValue() {
        throw unsupported("deviceLockedValue");
    }

    /** Starts a validation against the current device orientation. */
    default NativeValidationsBuilder deviceOrientationValue() {
        throw unsupported("deviceOrientationValue");
    }

    /** Starts a validation against the provider's current device-time value. */
    default NativeValidationsBuilder deviceTimeValue() {
        throw unsupported("deviceTimeValue");
    }

    /** Starts a validation against the current immutable battery reading. */
    default NativeValidationsBuilder batteryValue() {
        throw unsupported("batteryValue");
    }

    /** Starts a validation against the number of buffered device-log messages. */
    default NativeValidationsBuilder logMessageCountValue() {
        throw unsupported("logMessageCountValue");
    }

    /** Starts a validation against the number of buffered device-log errors. */
    default NativeValidationsBuilder logErrorCountValue() {
        throw unsupported("logErrorCountValue");
    }

    /** Starts a validation against the number of retained performance samples. */
    default NativeValidationsBuilder performanceSampleCountValue() {
        throw unsupported("performanceSampleCountValue");
    }

    /** Starts a validation against whether screen recording is changing state or active. */
    default NativeValidationsBuilder recordingInProgressValue() {
        throw unsupported("recordingInProgressValue");
    }

    /** Starts a validation against whether a successfully saved recording is retained. */
    default NativeValidationsBuilder retainedRecordingAvailableValue() {
        throw unsupported("retainedRecordingAvailableValue");
    }

    /** Starts a validation against the decoded-byte size of the retained recording. */
    default NativeValidationsBuilder retainedRecordingSizeValue() {
        throw unsupported("retainedRecordingSizeValue");
    }

    /**
     * Starts a validation against the number of artifact references in an immutable evidence bundle.
     *
     * @param bundle evidence bundle returned by mobile evidence capture
     */
    default NativeValidationsBuilder evidenceArtifactCountValue(MobileEvidenceBundle bundle) {
        throw unsupported("evidenceArtifactCountValue");
    }

    /**
     * Starts a validation against the number of explicit omissions in an immutable evidence bundle.
     *
     * @param bundle evidence bundle returned by mobile evidence capture
     */
    default NativeValidationsBuilder evidenceOmissionCountValue(MobileEvidenceBundle bundle) {
        throw unsupported("evidenceOmissionCountValue");
    }

    private static UnsupportedOperationException unsupported(String operation) {
        return new UnsupportedOperationException(
                operation + " is not supported by this mobile assertions implementation.");
    }
}
