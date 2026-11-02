package com.shaft.doctor.format;

/**
 * Safe evidence or report format failure.
 */
public class DoctorFormatException extends RuntimeException {
    /**
     * Creates a format failure.
     *
     * @param message safe message
     */
    public DoctorFormatException(String message) {
        super(message);
    }

    /**
     * Creates a format failure with its cause.
     *
     * @param message safe message
     * @param cause underlying cause
     */
    public DoctorFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}
