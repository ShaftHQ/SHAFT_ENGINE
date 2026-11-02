package com.shaft.capture.collector;

import java.time.Duration;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;

/**
 * Tracks whether a {@link BidiBrowserEventCollector} running alongside a
 * {@link PollingBrowserEventCollector} has delivered a signal recently, so the polling collector
 * can skip its own redundant script round-trips while BiDi is proven healthy, and resume full-rate
 * polling the moment BiDi goes quiet.
 */
public final class BidiActivityGate implements BooleanSupplier {
    private static final Duration DEFAULT_GRACE_PERIOD = Duration.ofSeconds(2);
    private static final long NEVER = Long.MIN_VALUE;

    private final long gracePeriodNanos;
    private final AtomicLong lastActivityNanos = new AtomicLong(NEVER);

    /**
     * Creates a gate using the default two-second grace period.
     */
    public BidiActivityGate() {
        this(DEFAULT_GRACE_PERIOD);
    }

    /**
     * Creates a gate with a custom grace period.
     *
     * @param gracePeriod how long BiDi activity keeps reporting healthy after the last signal
     */
    public BidiActivityGate(Duration gracePeriod) {
        this.gracePeriodNanos = (gracePeriod == null ? DEFAULT_GRACE_PERIOD : gracePeriod).toNanos();
    }

    /**
     * Records that a BiDi collector just delivered a signal.
     */
    public void recordActivity() {
        lastActivityNanos.set(System.nanoTime());
    }

    /**
     * Reports whether BiDi has delivered a signal within the grace period.
     *
     * @return true while BiDi is proven healthy
     */
    @Override
    public boolean getAsBoolean() {
        long last = lastActivityNanos.get();
        return last != NEVER && System.nanoTime() - last <= gracePeriodNanos;
    }
}
