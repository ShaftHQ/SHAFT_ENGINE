package com.shaft.gui.driver;

import java.time.Instant;
import java.util.Objects;

/** Immutable device-log message captured from Appium logcat or syslog. */
public record MobileLogMessage(Instant capturedAt, String source, String text) {
    public MobileLogMessage {
        capturedAt = Objects.requireNonNull(capturedAt, "capturedAt");
        source = Objects.requireNonNull(source, "source");
        text = text == null ? "" : text;
    }
}
