package testPackage.unitTests;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Unit tests for the telemetry de-duplication and flaky-test detection logic used in
 * {@code TestNGListener}, {@code JunitListener}, and {@code CucumberHelper}.
 * <p>
 * The logic is extracted and replicated here as plain Java (no listener wiring needed) so
 * tests run fast and without a real test engine.  The assertions verify the four categories
 * are always mutually exclusive and that their sum equals the total number of unique tests.
 */
public class TelemetryDeduplicationUnitTest {

    // ─── Helper: simulate TestNG/JUnit de-dup logic ─────────────────────────────

    /**
     * Mirrors the set-arithmetic used in {@code TestNGListener.onExecutionFinish()} and
     * {@code JunitListener.engineTearDown()} to compute per-category counts from raw
     * "passed" and "failed" ID lists (which may contain retried duplicates).
     *
     * @return int[]{passed, failed, skipped, flaky}
     */
    private int[] computeListenerCounts(List<String> passedList, List<String> failedList, List<String> skippedList) {
        Set<String> passedSet = new HashSet<>(passedList);
        Set<String> flakySet = failedList.stream()
                .filter(passedSet::contains)
                .collect(Collectors.toCollection(HashSet::new));
        Set<String> failedSet = failedList.stream()
                .filter(id -> !passedSet.contains(id))
                .collect(Collectors.toCollection(HashSet::new));
        int uniquePassed = passedSet.size() - flakySet.size();
        int uniqueFailed = failedSet.size();
        int uniqueFlaky = flakySet.size();
        Set<String> resolvedSet = new HashSet<>(passedSet);
        resolvedSet.addAll(failedSet);
        int uniqueSkipped = (int) skippedList.stream().filter(id -> !resolvedSet.contains(id)).distinct().count();
        return new int[]{uniquePassed, uniqueFailed, uniqueSkipped, uniqueFlaky};
    }

    /**
     * Mirrors the set-arithmetic used in {@code CucumberHelper.shaftTearDown()} to compute
     * per-category counts from scenario-ID sets (which replace duplicate-inflated counters).
     *
     * @return int[]{passed, failed, skipped, flaky}
     */
    private int[] computeCucumberCounts(Set<String> passedIds, Set<String> failedIds, int skippedCount) {
        Set<String> flakyIds = new HashSet<>(failedIds);
        flakyIds.retainAll(passedIds);
        int flakyCount = flakyIds.size();
        int uniquePassed = passedIds.size() - flakyCount;
        int uniqueFailed = failedIds.size() - flakyCount;
        return new int[]{uniquePassed, uniqueFailed, skippedCount, flakyCount};
    }

    // ─── TestNG/JUnit listener tests ────────────────────────────────────────────

    @Test(description = "All tests pass on first attempt: flaky=0, failed=0")
    public void allPassNoRetry() {
        List<String> passed = List.of("A", "B", "C");
        List<String> failed = Collections.emptyList();
        List<String> skipped = Collections.emptyList();
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 3, "passed");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 0, "flaky");
    }

    @Test(description = "Test fails on first attempt then passes on retry: flaky=1, failed=0, passed excludes it")
    public void oneTestFlakyFailThenPass() {
        // Test "A" fails first, then passes on retry → appears in both lists
        List<String> passed = List.of("A", "B");
        List<String> failed = List.of("A");
        List<String> skipped = Collections.emptyList();
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 1, "passed (B only)");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 1, "flaky (A)");
        // Verify total = 2 unique tests
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 2, "total");
    }

    @Test(description = "Test fails twice (permanent failure): failed=1, flaky=0")
    public void testFailsTwiceNeverPasses() {
        // "A" fails on attempt 1 and attempt 2 → only in failed list (deduplicated to 1)
        List<String> passed = List.of("B");
        List<String> failed = List.of("A", "A");
        List<String> skipped = Collections.emptyList();
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 1, "passed (B)");
        Assert.assertEquals(counts[1], 1, "failed (A)");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 0, "flaky");
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 2, "total");
    }

    @Test(description = "Test passes twice (retry of a pass): counted as passed once, not double-counted")
    public void testPassesTwiceCountedOnce() {
        // "A" ends up in passed list twice (shouldn't happen in practice, but the set deduplicates it)
        List<String> passed = List.of("A", "A");
        List<String> failed = Collections.emptyList();
        List<String> skipped = Collections.emptyList();
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 1, "passed (A once)");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 0, "flaky");
    }

    @Test(description = "Mixed: some pass, some fail permanently, some flaky, some skipped")
    public void mixedScenario() {
        // A: passes cleanly; B: flaky (fail then pass); C: fails permanently; D: skipped
        List<String> passed = List.of("A", "B");
        List<String> failed = List.of("B", "C");
        List<String> skipped = List.of("D");
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 1, "passed (A)");
        Assert.assertEquals(counts[1], 1, "failed (C)");
        Assert.assertEquals(counts[2], 1, "skipped (D)");
        Assert.assertEquals(counts[3], 1, "flaky (B)");
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 4, "total");
    }

    @Test(description = "Skipped test that also failed should not be double-counted in skipped")
    public void skippedAfterFailNotDoubledInSkipped() {
        // "A" failed, then was skipped on next run — the resolvedSet excludes it from skipped
        List<String> passed = Collections.emptyList();
        List<String> failed = List.of("A");
        List<String> skipped = List.of("A", "B");
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 0, "passed");
        Assert.assertEquals(counts[1], 1, "failed (A)");
        // A is in resolvedSet (failed set), so it should not be counted as skipped
        Assert.assertEquals(counts[2], 1, "skipped (B only)");
        Assert.assertEquals(counts[3], 0, "flaky");
    }

    // ─── Cucumber set-based tests ────────────────────────────────────────────────

    @Test(description = "Cucumber: all pass, no flaky")
    public void cucumberAllPass() {
        Set<String> passed = Set.of("feature:10", "feature:20");
        Set<String> failed = Collections.emptySet();
        int[] counts = computeCucumberCounts(passed, failed, 0);
        Assert.assertEquals(counts[0], 2, "passed");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 0, "flaky");
    }

    @Test(description = "Cucumber: one scenario flaky (fail then pass), one passes cleanly")
    public void cucumberOneFlaky() {
        // "feature:10" fails on attempt 1, passes on attempt 2 → in both sets
        Set<String> passed = new HashSet<>(Set.of("feature:10", "feature:20"));
        Set<String> failed = new HashSet<>(Set.of("feature:10"));
        int[] counts = computeCucumberCounts(passed, failed, 0);
        Assert.assertEquals(counts[0], 1, "passed (feature:20)");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 1, "flaky (feature:10)");
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 2, "total");
    }

    @Test(description = "Cucumber: permanent failure not counted as flaky")
    public void cucumberPermanentFailure() {
        Set<String> passed = new HashSet<>(Set.of("feature:20"));
        Set<String> failed = new HashSet<>(Set.of("feature:10"));
        int[] counts = computeCucumberCounts(passed, failed, 0);
        Assert.assertEquals(counts[0], 1, "passed");
        Assert.assertEquals(counts[1], 1, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 0, "flaky");
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 2, "total");
    }

    @Test(description = "Cucumber: ID sets naturally deduplicate retries — no double-counting")
    public void cucumberIdSetsDeduplicateRetries() {
        // A scenario retried 3 times: 2 failures then 1 pass.
        // Since we use sets, the scenario ID appears exactly once in each set.
        Set<String> passed = new HashSet<>(Set.of("feature:10"));
        Set<String> failed = new HashSet<>(Set.of("feature:10")); // same ID, not duplicated by set
        int[] counts = computeCucumberCounts(passed, failed, 0);
        // It's flaky (in both sets), so passed=0, failed=0, flaky=1
        Assert.assertEquals(counts[0], 0, "passed");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[3], 1, "flaky");
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 1, "total");
    }

    @Test(description = "Cucumber: skipped count comes from AtomicInteger, not from sets")
    public void cucumberSkippedCountIndependent() {
        Set<String> passed = new HashSet<>(Set.of("feature:10"));
        Set<String> failed = Collections.emptySet();
        int[] counts = computeCucumberCounts(passed, failed, 3);
        Assert.assertEquals(counts[0], 1, "passed");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 3, "skipped");
        Assert.assertEquals(counts[3], 0, "flaky");
    }

    @Test(description = "No tests at all: all counts are zero")
    public void noTests() {
        int[] listenerCounts = computeListenerCounts(
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        Assert.assertEquals(listenerCounts[0], 0);
        Assert.assertEquals(listenerCounts[1], 0);
        Assert.assertEquals(listenerCounts[2], 0);
        Assert.assertEquals(listenerCounts[3], 0);

        int[] cucumberCounts = computeCucumberCounts(
                Collections.emptySet(), Collections.emptySet(), 0);
        Assert.assertEquals(cucumberCounts[0], 0);
        Assert.assertEquals(cucumberCounts[1], 0);
        Assert.assertEquals(cucumberCounts[2], 0);
        Assert.assertEquals(cucumberCounts[3], 0);
    }

    @Test(description = "Multiple flaky tests: all counted correctly")
    public void multipleFlaky() {
        List<String> passed = new ArrayList<>(List.of("A", "B", "C"));
        List<String> failed = new ArrayList<>(List.of("A", "B")); // A and B are flaky; C passes clean
        List<String> skipped = Collections.emptyList();
        int[] counts = computeListenerCounts(passed, failed, skipped);
        Assert.assertEquals(counts[0], 1, "passed (C only)");
        Assert.assertEquals(counts[1], 0, "failed");
        Assert.assertEquals(counts[2], 0, "skipped");
        Assert.assertEquals(counts[3], 2, "flaky (A and B)");
        Assert.assertEquals(counts[0] + counts[1] + counts[2] + counts[3], 3, "total");
    }
}
