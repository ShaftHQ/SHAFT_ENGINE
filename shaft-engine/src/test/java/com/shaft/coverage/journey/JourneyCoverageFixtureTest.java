package com.shaft.coverage.journey;

import org.testng.annotations.Test;

import java.util.List;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

/**
 * Fixture tests for journey / interactive-state coverage (SC-1..SC-3, issue #5455).
 */
public class JourneyCoverageFixtureTest {

    private static CoverageTarget target(String journey, String view, String interaction, StateKind kind) {
        return new CoverageTarget(
                JourneyId.of(journey),
                ViewId.of(view),
                InteractionId.of(interaction),
                StateId.of(kind));
    }

    @Test(description = "SC-1: fixture with known states produces expected covered and uncovered maps")
    public void fixtureProducesExpectedCoveredAndUncoveredMaps() {
        CoverageTarget checkoutLoading = target("checkout", "/cart", "pay-button", StateKind.LOADING);
        CoverageTarget checkoutEmpty = target("checkout", "/cart", "pay-button", StateKind.EMPTY);
        CoverageTarget checkoutError = target("checkout", "/cart", "pay-button", StateKind.ERROR);
        CoverageTarget checkoutSuccess = target("checkout", "/cart", "pay-button", StateKind.SUCCESS);
        CoverageTarget hiddenPromo = target("checkout", "/cart", "promo-banner", StateKind.SUCCESS);

        List<DeclaredTarget> catalog = List.of(
                DeclaredTarget.observable(checkoutLoading),
                DeclaredTarget.observable(checkoutEmpty),
                DeclaredTarget.observable(checkoutError),
                DeclaredTarget.observable(checkoutSuccess),
                DeclaredTarget.unobservable(hiddenPromo));

        ObservationRecorder recorder = new ObservationRecorder();
        recorder.record(checkoutLoading, "test-loading");
        recorder.record(checkoutSuccess, "test-success");

        // Observed but not in catalog -> unknown
        CoverageTarget stray = target("admin", "/users/42", "delete", StateKind.UNKNOWN);
        recorder.record(stray, "test-stray");

        CoverageMap map = CoverageMapDeriver.derive(catalog, recorder);
        assertEquals(map.covered(), java.util.Set.of(checkoutLoading, checkoutSuccess));
        assertEquals(map.uncovered(), java.util.Set.of(checkoutEmpty, checkoutError));
        assertTrue(map.unknown().contains(stray), map.unknown().toString());
        assertEquals(map.unobservable(), java.util.Set.of(hiddenPromo));

        String summary = JourneyCoverageReport.summarize(map);
        assertTrue(summary.contains("Uncovered (2)"), summary);
        assertTrue(summary.contains("Unknown (1)"), summary);
        assertTrue(summary.contains("Unobservable (1)"), summary);
        assertFalse(summary.contains("Uncovered (2): (none)"), summary);
    }

    @Test(description = "SC-2: retry and duplicate-view mutations do not inflate coverage")
    public void retryAndDuplicateViewMutationsDoNotInflateCoverage() {
        CoverageTarget canonical = target("onboarding", "/welcome", "continue", StateKind.SUCCESS);
        ObservationRecorder recorder = new ObservationRecorder();

        recorder.record(canonical, "run-1");
        recorder.record(target("onboarding#retry-2", "/welcome", "continue", StateKind.SUCCESS), "run-1-retry");
        recorder.record(target("onboarding", "/welcome[dup-3]", "continue", StateKind.SUCCESS), "run-1-dup");
        recorder.record(target("onboarding", "/welcome?session=abc", "continue-attempt-4", StateKind.SUCCESS),
                "run-1-query");
        recorder.record(target("onboarding", "/welcome/99", "continue", StateKind.SUCCESS), "run-dynamic-id");

        // /welcome/99 normalizes path segment 99 -> {id}, so it is a DIFFERENT view than /welcome.
        // Retries/dups/query on the same logical view must collapse.
        assertEquals(recorder.distinctCount(), 2, recorder.observedTargets().toString());
        assertEquals(recorder.hitCount(canonical), 4);
        assertTrue(recorder.observedTargets().contains(canonical));

        CoverageTarget parameterized = target("onboarding", "/welcome/{id}", "continue", StateKind.SUCCESS);
        assertTrue(recorder.observedTargets().contains(parameterized));
        assertEquals(recorder.hitCount(parameterized), 1);
    }

    @Test(description = "SC-3: baseline reductions are visible before merge")
    public void baselineReductionsVisibleBeforeMerge() {
        CoverageTarget loading = target("checkout", "/cart", "pay-button", StateKind.LOADING);
        CoverageTarget success = target("checkout", "/cart", "pay-button", StateKind.SUCCESS);
        CoverageTarget error = target("checkout", "/cart", "pay-button", StateKind.ERROR);

        CoverageBaseline baseline = CoverageBaseline.of("main", List.of(
                DeclaredTarget.observable(loading),
                DeclaredTarget.observable(success),
                DeclaredTarget.observable(error),
                DeclaredTarget.unobservable(target("checkout", "/cart", "legacy-widget", StateKind.SUCCESS))));

        ObservationRecorder prRun = new ObservationRecorder();
        prRun.record(loading, "pr-loading");
        // success and error missing -> reductions

        BaselineComparison comparison = BaselineComparison.compare(baseline, prRun);
        assertTrue(comparison.hasReductions());
        assertEquals(comparison.reductions(), java.util.Set.of(success, error));
        assertEquals(comparison.map().covered(), java.util.Set.of(loading));
        assertEquals(comparison.map().uncovered(), java.util.Set.of(success, error));
        assertEquals(comparison.map().unobservable().size(), 1);

        String report = JourneyCoverageReport.summarize(comparison);
        assertTrue(report.contains("Baseline reductions (2)"), report);
        assertTrue(report.contains("Has reductions before merge: true"), report);
        assertTrue(report.contains(success.key()), report);
    }

    @Test(description = "FR-1: identities normalize volatile path and retry tokens")
    public void identitiesAreStable() {
        assertEquals(JourneyId.of("Checkout#retry-1").value(), "checkout");
        assertEquals(ViewId.of("/orders/550e8400-e29b-41d4-a716-446655440000/details").value(),
                "/orders/{id}/details");
        assertEquals(InteractionId.of("Submit (retry 3)").value(), "submit");
        assertEquals(StateId.of(StateKind.ERROR, "Payment Failed").toString(), "error:payment failed");
    }
}
