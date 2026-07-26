package com.shaft.heal.internal;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.Optional;
import java.util.function.LongConsumer;
import java.util.function.Supplier;

/**
 * Retries a deterministic recovery attempt until it succeeds or a hard total time budget is
 * exhausted (issue #4027) -- a per-attempt timeout multiplied by N rungs is not the requirement;
 * the ceiling applies to the whole ladder, measured from the first rung.
 */
final class ResuggestionLadder {
    private final Clock clock;
    private final LongConsumer sleep;

    ResuggestionLadder(Clock clock, LongConsumer sleep) {
        this.clock = clock;
        this.sleep = sleep;
    }

    /**
     * Creates a real-time ladder backed by the system clock and {@link Thread#sleep(long)}.
     *
     * @return production-ready ladder
     */
    static ResuggestionLadder system() {
        return new ResuggestionLadder(Clock.systemUTC(), ResuggestionLadder::sleepQuietly);
    }

    private static void sleepQuietly(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
        }
    }

    /**
     * Runs {@code attempt} repeatedly until it returns a present result or the total elapsed time
     * across every rung reaches {@code budget}.
     *
     * @param budget hard total budget across the whole ladder
     * @param pollInterval delay between rungs
     * @param attempt one deterministic re-suggestion rung
     * @param <T> attempt result type
     * @return the first successful result, plus how many rungs ran and the total elapsed time
     */
    <T> Result<T> run(Duration budget, Duration pollInterval, Supplier<Optional<T>> attempt) {
        Instant start = clock.instant();
        Instant deadline = start.plus(budget);
        int rungs = 0;
        while (true) {
            rungs++;
            Optional<T> result = attempt.get();
            Instant now = clock.instant();
            if (result.isPresent()) {
                return new Result<>(result, rungs, Duration.between(start, now));
            }
            if (!now.isBefore(deadline)) {
                return new Result<>(Optional.empty(), rungs, Duration.between(start, now));
            }
            Duration remaining = Duration.between(now, deadline);
            Duration delay = remaining.compareTo(pollInterval) < 0 ? remaining : pollInterval;
            if (delay.toMillis() > 0) {
                sleep.accept(delay.toMillis());
            }
        }
    }

    /**
     * One ladder outcome.
     *
     * @param value the first successful attempt result, or empty when the budget was exhausted
     * @param rungs how many attempts ran
     * @param elapsed total elapsed time across every rung
     * @param <T> attempt result type
     */
    record Result<T>(Optional<T> value, int rungs, Duration elapsed) {
    }
}
