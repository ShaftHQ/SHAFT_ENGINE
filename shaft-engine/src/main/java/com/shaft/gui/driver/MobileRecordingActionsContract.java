package com.shaft.gui.driver;

import java.nio.file.Path;

/** Backend-native mobile screen-recording actions. */
public interface MobileRecordingActionsContract {
    /** Starts a bounded recording with SHAFT defaults. */
    default MobileRecordingActionsContract start() {
        throw unsupported();
    }

    /** Starts a bounded recording with explicit provider time and result-size limits. */
    default MobileRecordingActionsContract start(MobileRecordingOptions options) {
        throw unsupported();
    }

    /** Stops the active recording and returns its decoded media bytes. */
    default byte[] stop() {
        throw unsupported();
    }

    /** Stops the active recording and safely publishes it to the exact local target. */
    default Path stopAndSave(Path exactTarget) {
        throw unsupported();
    }

    MobileActionsContract and();

    private static UnsupportedOperationException unsupported() {
        return new UnsupportedOperationException("Mobile screen recording is not implemented by this provider.");
    }
}
