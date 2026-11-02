package com.shaft.gui.internal.video;

import java.io.InputStream;

/**
 * SHAFT-owned contract for desktop screen-recording implementations.
 *
 * <p>Implementations may use optional recording and encoding libraries, but callers in
 * {@code shaft-engine} interact only with JDK and SHAFT-owned types through this interface.</p>
 */
public interface DesktopVideoRecordingProvider {
    /**
     * Starts recording the desktop for the current test thread.
     */
    void startRecording();

    /**
     * Stops the active desktop recording and returns its encoded contents.
     *
     * @param testPassed whether the current test passed, used by providers that conditionally retain recordings
     * @param recordingName unique name to use when saving the recording
     * @return the encoded recording, or {@code null} when no recording is available
     */
    InputStream stopRecording(boolean testPassed, String recordingName);

    /**
     * Reports whether this provider is recording on the current test thread.
     *
     * @return {@code true} when a desktop recording is active; otherwise {@code false}
     */
    boolean isRecording();
}
