package com.shaft.gui.internal.natural;

import com.shaft.driver.SHAFT;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.testng.Assert.*;

/**
 * Tests for natural action planner registry AI fallback confidence gating.
 */
public class NaturalActionPlannerRegistryTest {
    private MockAiNaturalActionPlanner mockPlanner;

    @BeforeMethod
    public void setUp() {
        mockPlanner = new MockAiNaturalActionPlanner();
        NaturalActionPlannerRegistry.setPlannersForTesting(List.of(mockPlanner));
        SHAFT.Properties.naturalActions.set()
                .aiFallbackEnabled(true)
                .planner("auto");
    }

    @AfterMethod
    public void tearDown() {
        NaturalActionPlannerRegistry.resetPlanners();
        SHAFT.Properties.naturalActions.set()
                .aiFallbackEnabled(false)
                .planner("deterministic")
                .aiFallbackThreshold(0);
    }

    @Test
    public void autoModeBelowThresholdShouldInvokeAiFallback() {
        double threshold = 0.95;
        SHAFT.Properties.naturalActions.set().aiFallbackThreshold(threshold);

        NaturalActionRequest request = new NaturalActionRequest(
                null,
                "refresh",
                List.of(),
                false,
                false);
        NaturalActionPlan plan = NaturalActionPlannerRegistry.plan(request);

        assertEquals(plan.resolutionPath(), "ai-fallback",
                "Should fall back to AI when deterministic confidence is below threshold");
        assertTrue(mockPlanner.callCount() > 0,
                "AI planner should be invoked when deterministic confidence is below threshold");
        assertEquals(mockPlanner.callCount(), 1,
                "AI planner should be invoked exactly once");
    }

    @Test
    public void autoModeEmptyStepsShouldInvokeAiFallback() {
        SHAFT.Properties.naturalActions.set().aiFallbackThreshold(0);

        NaturalActionRequest request = new NaturalActionRequest(
                null,
                "unmatchedintent",
                List.of(),
                false,
                false);
        NaturalActionPlan plan = NaturalActionPlannerRegistry.plan(request);

        assertEquals(plan.resolutionPath(), "ai-fallback",
                "Should fall back to AI when deterministic steps are empty");
        assertTrue(mockPlanner.callCount() > 0,
                "AI planner should be invoked when deterministic steps are empty");
    }

    @Test
    public void thresholdZeroRestoresPriorAlwaysFallbackBehavior() {
        SHAFT.Properties.naturalActions.set().aiFallbackThreshold(0);

        // Test 1: Empty steps should trigger fallback even with threshold 0
        NaturalActionRequest emptyRequest = new NaturalActionRequest(
                null,
                "unmatchedintent",
                List.of(),
                false,
                false);
        NaturalActionPlan emptyPlan = NaturalActionPlannerRegistry.plan(emptyRequest);
        assertEquals(emptyPlan.resolutionPath(), "ai-fallback",
                "Threshold 0 with empty deterministic steps should trigger fallback");
        assertTrue(mockPlanner.callCount() > 0,
                "AI planner should be invoked for empty steps");

        // Reset counter
        mockPlanner.invocationCount.set(0);

        // Test 2: Non-empty steps should NOT trigger fallback even with threshold 0
        NaturalActionRequest matchedRequest = new NaturalActionRequest(
                null,
                "refresh",
                List.of(),
                false,
                false);
        NaturalActionPlan matchedPlan = NaturalActionPlannerRegistry.plan(matchedRequest);
        assertEquals(matchedPlan.resolutionPath(), "deterministic",
                "Threshold 0 with non-empty deterministic steps should keep deterministic plan");
        assertEquals(mockPlanner.callCount(), 0,
                "AI planner should NOT be invoked when deterministic has non-empty steps (prior always-fallback behavior)");
    }

    @Test
    public void autoModeNearExactMatchShouldNotInvokeAiFallback() {
        double threshold = 0.8;
        SHAFT.Properties.naturalActions.set().aiFallbackThreshold(threshold);

        NaturalActionRequest request = new NaturalActionRequest(
                null,
                "refresh",
                List.of(),
                false,
                false);
        NaturalActionPlan plan = NaturalActionPlannerRegistry.plan(request);

        assertEquals(plan.resolutionPath(), "deterministic",
                "Should use deterministic when confidence is above threshold");
        assertEquals(mockPlanner.callCount(), 0,
                "AI planner should not be invoked when deterministic confidence is above threshold");
    }

    /**
     * Mock AI planner for testing without requiring shaft-pilot-core dependency.
     */
    private static class MockAiNaturalActionPlanner implements NaturalActionPlanner {
        private final AtomicInteger invocationCount = new AtomicInteger(0);

        @Override
        public String id() {
            return "mock-ai";
        }

        @Override
        public int priority() {
            return 50;
        }

        @Override
        public boolean supports(NaturalActionRequest request) {
            return true;
        }

        @Override
        public NaturalActionPlan plan(NaturalActionRequest request) {
            invocationCount.incrementAndGet();
            return NaturalActionPlan.of(
                    id(),
                    request.intent(),
                    List.of(new NaturalActionStep(
                            NaturalActionKind.BROWSER_REFRESH,
                            null,
                            null,
                            0.99,
                            "Mock AI plan with high confidence")),
                    0.99,
                    "Mock AI planner produced a plan");
        }

        int callCount() {
            return invocationCount.get();
        }
    }
}
