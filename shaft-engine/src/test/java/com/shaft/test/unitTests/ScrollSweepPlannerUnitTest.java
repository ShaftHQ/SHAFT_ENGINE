package com.shaft.test.unitTests;

import com.shaft.gui.browser.internal.ScrollSweepPlanner;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Unit tests for {@code ScrollSweepPlanner} (issue #3749, Increment D).
 * <p>
 * Exercises the pure step-count/height-stability stop condition that backs
 * {@code BrowserActions.scrollToLoadAll()}'s bounded progressive scroll sweep, without a
 * browser -- following the same pure-function extraction idiom used for the readiness
 * quiet-window state machine in {@link JavaScriptWaitManagerUnitTest}.
 */
public class ScrollSweepPlannerUnitTest {

    @Test(description = "Verify the sweep continues when the page is still growing and the bottom has not been reached")
    public void testContinuesWhenPageStillGrowingAndBottomNotReached() {
        Assert.assertFalse(ScrollSweepPlanner.shouldStop(1, 20, 1000L, 2000L, false),
                "Sweep should continue: page grew and bottom was not reached");
    }

    @Test(description = "Verify the sweep continues when the bottom is reached but the page just grew (new content may still load)")
    public void testContinuesWhenBottomReachedButPageJustGrew() {
        Assert.assertFalse(ScrollSweepPlanner.shouldStop(1, 20, 1000L, 2000L, true),
                "Sweep should continue: even though the bottom was reached, the page grew on this step");
    }

    @Test(description = "Verify the sweep continues when the page height is stable but the bottom has not been reached yet")
    public void testContinuesWhenHeightStableButBottomNotReached() {
        Assert.assertFalse(ScrollSweepPlanner.shouldStop(1, 20, 1000L, 1000L, false),
                "Sweep should continue: bottom was not reached even though height is stable");
    }

    @Test(description = "Verify the sweep stops once the page height stops growing and the bottom has been reached")
    public void testStopsWhenHeightStableAndBottomReached() {
        Assert.assertTrue(ScrollSweepPlanner.shouldStop(3, 20, 1000L, 1000L, true),
                "Sweep should stop: page stopped growing and bottom was reached");
    }

    @Test(description = "Verify the sweep stops once the page height shrinks and the bottom has been reached")
    public void testStopsWhenHeightShrinksAndBottomReached() {
        Assert.assertTrue(ScrollSweepPlanner.shouldStop(3, 20, 1000L, 900L, true),
                "Sweep should stop: a shrinking (or equal) height with bottom reached means no further growth is expected");
    }

    @Test(description = "Verify the sweep always stops once the configured max-steps ceiling is reached, even mid-growth")
    public void testStopsAtMaxStepsCeilingEvenWhileStillGrowing() {
        Assert.assertTrue(ScrollSweepPlanner.shouldStop(20, 20, 1000L, 5000L, false),
                "Sweep must stop at the step ceiling regardless of growth/bottom state, to bound worst-case cost");
    }

    @Test(description = "Verify the sweep stops immediately when maxSteps is configured as zero")
    public void testStopsImmediatelyWhenMaxStepsIsZero() {
        Assert.assertTrue(ScrollSweepPlanner.shouldStop(0, 0, 1000L, 1000L, false),
                "A zero max-steps ceiling must stop the sweep before any step is attempted");
    }

    @Test(description = "Verify a completedSteps count past the ceiling (defensive) still stops the sweep")
    public void testStopsWhenCompletedStepsExceedsCeilingDefensively() {
        Assert.assertTrue(ScrollSweepPlanner.shouldStop(21, 20, 1000L, 5000L, false),
                "A completed-steps count beyond the ceiling must still be treated as stop, defensively");
    }
}
