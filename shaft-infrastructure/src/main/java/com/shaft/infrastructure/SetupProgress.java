package com.shaft.infrastructure;

import java.math.BigInteger;
import java.util.Objects;

/** Immutable provider-neutral progress for one approved setup mutation. */
public record SetupProgress(SetupProfile profile, String phase, long completedBytes,
                            long totalBytes, int percentage) {
    public SetupProgress {
        Objects.requireNonNull(profile, "profile");
        if (phase == null || phase.isBlank()) {
            throw new IllegalArgumentException("Setup progress phase must not be blank.");
        }
        if (completedBytes < 0 || totalBytes < completedBytes) {
            throw new IllegalArgumentException("Setup progress bytes are invalid.");
        }
        int expected = percentage(completedBytes, totalBytes);
        if (percentage != expected) {
            throw new IllegalArgumentException("Setup progress percentage does not match its bytes.");
        }
    }

    public static SetupProgress of(SetupProfile profile, String phase, long completedBytes, long totalBytes) {
        int percentage = percentage(completedBytes, totalBytes);
        return new SetupProgress(profile, phase, completedBytes, totalBytes, percentage);
    }

    private static int percentage(long completedBytes, long totalBytes) {
        if (totalBytes == 0) return 0;
        if (completedBytes == totalBytes) return 100;
        return BigInteger.valueOf(completedBytes).multiply(BigInteger.valueOf(100))
                .divide(BigInteger.valueOf(totalBytes)).intValueExact();
    }
}
