package com.shaft.gui.driver;

import java.time.Instant;
import java.util.Objects;

/** Immutable error emitted by an Appium device-log listener; missing or blank types normalize to Throwable. */
public record MobileLogError(Instant capturedAt, String source, String type, String message) {
    public MobileLogError {
        capturedAt = Objects.requireNonNull(capturedAt, "capturedAt");
        source = Objects.requireNonNull(source, "source");
        type = type == null || type.isBlank() ? Throwable.class.getName() : type;
        message = message == null ? "" : message;
    }
}
