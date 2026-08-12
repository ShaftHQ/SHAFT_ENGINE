package com.shaft.gui.driver;

import java.time.Duration;
import java.util.Objects;

/**
 * Bounded options for one Appium mobile screen-recording session.
 *
 * @param timeLimit provider recording limit from one second through 30 minutes
 * @param maxBytes maximum decoded response size from one byte through 256 MiB
 */
public record MobileRecordingOptions(Duration timeLimit, long maxBytes) {
    public static final Duration MIN_TIME_LIMIT = Duration.ofSeconds(1);
    public static final Duration MAX_TIME_LIMIT = Duration.ofMinutes(30);
    public static final Duration DEFAULT_TIME_LIMIT = Duration.ofMinutes(3);
    public static final long MAX_RESULT_BYTES = 256L * 1024 * 1024;
    public static final long DEFAULT_MAX_BYTES = 64L * 1024 * 1024;

    public MobileRecordingOptions {
        timeLimit = Objects.requireNonNull(timeLimit, "recording time limit");
        if (timeLimit.compareTo(MIN_TIME_LIMIT) < 0 || timeLimit.compareTo(MAX_TIME_LIMIT) > 0) {
            throw new IllegalArgumentException("Recording time limit must be between one second and 30 minutes.");
        }
        if (maxBytes < 1 || maxBytes > MAX_RESULT_BYTES) {
            throw new IllegalArgumentException("Recording result limit must be between one byte and 256 MiB.");
        }
    }

    /**
     * Returns SHAFT's conservative recording defaults.
     *
     * @return a three-minute recording limit and a 64 MiB decoded-result limit
     */
    public static MobileRecordingOptions defaults() {
        return new MobileRecordingOptions(DEFAULT_TIME_LIMIT, DEFAULT_MAX_BYTES);
    }
}
