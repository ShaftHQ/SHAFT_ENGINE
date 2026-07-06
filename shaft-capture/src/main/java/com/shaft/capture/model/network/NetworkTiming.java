package com.shaft.capture.model.network;

import java.time.Duration;

/**
 * Performance timing measurements for a network transaction, mirroring the
 * conventional browser DevTools/HAR timing breakdown. All durations are
 * nullable and may be absent when a phase was not measured or does not apply.
 *
 * @param blocked time waiting for a network connection to become available
 * @param dns time spent resolving DNS
 * @param connect time spent establishing the connection
 * @param send time spent sending the request
 * @param ttfb time to first byte of the response
 * @param receive time spent downloading the response body
 */
public record NetworkTiming(
        Duration blocked,
        Duration dns,
        Duration connect,
        Duration send,
        Duration ttfb,
        Duration receive) {
    /**
     * Creates immutable network timing evidence. Every phase is optional; negative
     * durations are rejected since they cannot represent real elapsed time.
     */
    public NetworkTiming {
        requireNonNegative(blocked, "blocked");
        requireNonNegative(dns, "dns");
        requireNonNegative(connect, "connect");
        requireNonNegative(send, "send");
        requireNonNegative(ttfb, "ttfb");
        requireNonNegative(receive, "receive");
    }

    private static void requireNonNegative(Duration duration, String phase) {
        if (duration != null && duration.isNegative()) {
            throw new IllegalArgumentException("Network timing phase '" + phase + "' cannot be negative.");
        }
    }
}
