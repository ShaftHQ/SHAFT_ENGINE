package com.shaft.capture.format;

/**
 * Reports invalid, malformed, or unsupported capture artifacts without echoing their contents.
 */
public final class CaptureFormatException extends IllegalArgumentException {
    /**
     * Creates a format exception.
     *
     * @param message actionable safe message
     */
    public CaptureFormatException(String message) {
        super(message);
    }

    /**
     * Creates a format exception with a retained cause.
     *
     * @param message actionable safe message
     * @param cause original cause
     */
    public CaptureFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}
