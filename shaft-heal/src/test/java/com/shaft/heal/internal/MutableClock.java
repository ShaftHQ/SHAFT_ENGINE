package com.shaft.heal.internal;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;

/**
 * Test-only clock with an explicitly advanceable instant, shared by every ladder-timing test
 * (issue #4027): both {@code ResuggestionLadderTest} (unit tests the ladder in isolation) and
 * {@code ShaftHealingProviderTest} (proves {@code resolve()} actually invokes it under budget)
 * need the exact same fake-time behavior.
 */
final class MutableClock extends Clock {
    private Instant instant;

    MutableClock(Instant instant) {
        this.instant = instant;
    }

    void advance(Duration duration) {
        instant = instant.plus(duration);
    }

    @Override
    public ZoneOffset getZone() {
        return ZoneOffset.UTC;
    }

    @Override
    public Clock withZone(ZoneId zone) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Instant instant() {
        return instant;
    }
}
