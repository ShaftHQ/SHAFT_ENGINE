package com.shaft.capture.proxy;

/**
 * Reports a mobile API capture proxy failure (CA generation/load, proxy start/stop, or pairing)
 * without echoing sensitive material such as certificate private keys.
 */
public final class CaptureProxyException extends RuntimeException {
    /**
     * Creates a capture proxy exception.
     *
     * @param message actionable safe message
     */
    public CaptureProxyException(String message) {
        super(message);
    }

    /**
     * Creates a capture proxy exception with a retained cause.
     *
     * @param message actionable safe message
     * @param cause original cause
     */
    public CaptureProxyException(String message, Throwable cause) {
        super(message, cause);
    }
}
