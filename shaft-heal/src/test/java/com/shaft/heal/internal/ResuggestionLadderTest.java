package com.shaft.heal.internal;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class ResuggestionLadderTest {
    @Test
    public void firstAttemptSucceedingReturnsImmediatelyWithoutSleeping() {
        MutableClock clock = new MutableClock(Instant.EPOCH);
        List<Long> sleeps = new ArrayList<>();
        ResuggestionLadder ladder = new ResuggestionLadder(clock, sleeps::add);

        ResuggestionLadder.Result<String> result = ladder.run(
                Duration.ofSeconds(60), Duration.ofSeconds(5), () -> Optional.of("found"));

        Assert.assertEquals(result.value(), Optional.of("found"));
        Assert.assertEquals(result.rungs(), 1);
        Assert.assertTrue(sleeps.isEmpty());
    }

    @Test
    public void retriesUntilALaterRungSucceeds() {
        MutableClock clock = new MutableClock(Instant.EPOCH);
        List<Long> sleeps = new ArrayList<>();
        AtomicInteger attempts = new AtomicInteger();
        ResuggestionLadder ladder = new ResuggestionLadder(clock, millis -> {
            sleeps.add(millis);
            clock.advance(Duration.ofMillis(millis));
        });

        ResuggestionLadder.Result<String> result = ladder.run(
                Duration.ofSeconds(60),
                Duration.ofSeconds(5),
                () -> attempts.incrementAndGet() < 3 ? Optional.empty() : Optional.of("found"));

        Assert.assertEquals(result.value(), Optional.of("found"));
        Assert.assertEquals(result.rungs(), 3);
        Assert.assertEquals(sleeps.size(), 2);
        Assert.assertEquals((long) sleeps.get(0), 5_000L);
    }

    @Test
    public void totalElapsedAcrossAllRungsNeverExceedsTheHardBudgetEvenWithSlowRungs() {
        MutableClock clock = new MutableClock(Instant.EPOCH);
        // Each rung itself is slow (simulates an expensive deterministic re-suggestion pass),
        // advancing the clock by 26 seconds -- a per-attempt timeout multiplied by N rungs would
        // never catch this; only a hard ceiling on the TOTAL elapsed time does (issue #4027).
        ResuggestionLadder ladder = new ResuggestionLadder(clock, millis -> clock.advance(Duration.ofMillis(millis)));
        AtomicInteger attempts = new AtomicInteger();

        ResuggestionLadder.Result<String> result = ladder.run(
                Duration.ofSeconds(60),
                Duration.ofSeconds(5),
                () -> {
                    attempts.incrementAndGet();
                    clock.advance(Duration.ofSeconds(26));
                    return Optional.empty();
                });

        Assert.assertTrue(result.value().isEmpty());
        // 3 slow rungs of 26s already exceed 60s (78s); the ladder must have stopped there,
        // never launching a 4th rung, and total elapsed must not run away past the budget.
        Assert.assertEquals(attempts.get(), 3);
        Assert.assertTrue(
                result.elapsed().compareTo(Duration.ofSeconds(90)) < 0,
                "elapsed was " + result.elapsed());
    }
}
