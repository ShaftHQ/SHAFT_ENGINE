package com.shaft.doctor.history;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;

/**
 * Computes New / Always-failing / Flaky / Regressed / Fixed smart tags from Allure history
 * (issue #5975 / S3-09).
 *
 * <p>FR-001/FR-003: tags come only from history; insufficient history never invents Flaky.
 * SC-001: passed then failed → Regressed. SC-002: first-seen failure → New (not Regressed).
 * Duration-anomaly is optional when timings exist.
 */
public final class SmartTagsComputer {
    public static final int DEFAULT_WINDOW = 10;
    public static final int DEFAULT_FLAKY_TRANSITION_THRESHOLD = 3;
    /** Need at least this many launches before Flaky is eligible (FR-003). */
    public static final int MIN_LAUNCHES_FOR_FLAKY = 3;
    /** Always-failing requires at least two failing launches. */
    public static final int MIN_LAUNCHES_FOR_ALWAYS_FAILING = 2;
    /** Duration anomaly needs a small baseline of timed launches. */
    public static final int MIN_LAUNCHES_FOR_DURATION = 3;

    private static final String EMPTY_MESSAGE =
            "No smart tags yet. Provide Allure history.jsonl (accumulateHistory=true) so "
                    + "New / Always-failing / Flaky / Regressed / Fixed can be assessed.";

    private SmartTagsComputer() {
    }

    public static SmartTagModels.SmartTagTable compute(AllureHistoryModels.HistoryView history) {
        return compute(history, DEFAULT_WINDOW, DEFAULT_FLAKY_TRANSITION_THRESHOLD);
    }

    public static SmartTagModels.SmartTagTable compute(
            AllureHistoryModels.HistoryView history, int windowSize, int flakyTransitionThreshold) {
        int window = normalizeWindow(windowSize);
        int threshold = normalizeThreshold(flakyTransitionThreshold);
        if (history == null || history.tests().isEmpty()) {
            return new SmartTagModels.SmartTagTable(
                    SmartTagModels.SCHEMA_VERSION,
                    true,
                    EMPTY_MESSAGE,
                    window,
                    threshold,
                    List.of(),
                    history == null ? List.of() : history.warnings());
        }

        List<SmartTagModels.SmartTagRow> rows = new ArrayList<>();
        for (AllureHistoryModels.TestHistorySeries series : history.tests()) {
            rows.add(rowFor(series, window, threshold));
        }
        rows.sort(Comparator.comparing(SmartTagModels.SmartTagRow::historyId));

        List<String> warnings = new ArrayList<>(history.warnings());
        boolean empty = rows.isEmpty();
        return new SmartTagModels.SmartTagTable(
                SmartTagModels.SCHEMA_VERSION,
                empty,
                empty ? EMPTY_MESSAGE : "",
                window,
                threshold,
                rows,
                warnings);
    }

    private static SmartTagModels.SmartTagRow rowFor(
            AllureHistoryModels.TestHistorySeries series, int window, int threshold) {
        List<AllureHistoryModels.LaunchStatus> launches =
                truncateNewestFirst(series.launches(), window);
        List<Outcome> outcomes = passFailOutcomesNewestFirst(launches);
        List<String> tags = statusTags(outcomes, threshold);
        boolean durationAnomaly = assessDurationAnomaly(launches);
        if (durationAnomaly) {
            tags.add(SmartTagModels.TAG_DURATION_ANOMALY);
        }

        return new SmartTagModels.SmartTagRow(
                series.historyId(),
                series.name(),
                series.fullName(),
                primaryTag(tags),
                tags,
                outcomes.size(),
                transitionCount(outcomes),
                durationAnomaly,
                statusAt(outcomes, 0),
                statusAt(outcomes, 1));
    }

    private static List<String> statusTags(List<Outcome> outcomes, int threshold) {
        List<String> tags = new ArrayList<>();
        if (outcomes.size() == 1) {
            addNewTag(outcomes.get(0), tags);
            return tags;
        }
        if (outcomes.size() < 2) {
            return tags;
        }
        addChangedStatusTag(outcomes.get(0), outcomes.get(1), tags);
        addAlwaysFailingTag(outcomes, tags);
        addFlakyTag(outcomes, threshold, tags);
        return tags;
    }

    private static void addNewTag(Outcome outcome, List<String> tags) {
        if (outcome.failed()) {
            // SC-002: first-seen failure is New, never Regressed / Flaky.
            tags.add(SmartTagModels.TAG_NEW);
        }
    }

    private static void addChangedStatusTag(Outcome newest, Outcome previous, List<String> tags) {
        if (newest.passed() && previous.failed()) {
            tags.add(SmartTagModels.TAG_FIXED);
        } else if (newest.failed() && previous.passed()) {
            // SC-001: passed then failed → Regressed (not New).
            tags.add(SmartTagModels.TAG_REGRESSED);
        }
    }

    private static void addAlwaysFailingTag(List<Outcome> outcomes, List<String> tags) {
        if (outcomes.size() >= MIN_LAUNCHES_FOR_ALWAYS_FAILING
                && outcomes.stream().allMatch(Outcome::failed)) {
            tags.add(SmartTagModels.TAG_ALWAYS_FAILING);
        }
    }

    private static void addFlakyTag(List<Outcome> outcomes, int threshold, List<String> tags) {
        // FR-003: never invent Flaky from insufficient history.
        if (isFlaky(outcomes, threshold)) {
            tags.add(SmartTagModels.TAG_FLAKY);
        }
    }

    private static boolean isFlaky(List<Outcome> outcomes, int threshold) {
        if (outcomes.size() < MIN_LAUNCHES_FOR_FLAKY || countFlips(outcomes) < threshold) {
            return false;
        }
        boolean allFail = outcomes.stream().allMatch(Outcome::failed);
        boolean allPass = outcomes.stream().allMatch(Outcome::passed);
        return !allFail && !allPass;
    }

    private static Integer transitionCount(List<Outcome> outcomes) {
        return outcomes.size() < 2 ? null : Integer.valueOf(countFlips(outcomes));
    }

    private static String statusAt(List<Outcome> outcomes, int index) {
        return outcomes.size() <= index ? "" : outcomes.get(index).status();
    }

    static String primaryTag(List<String> tags) {
        if (tags == null || tags.isEmpty()) {
            return "";
        }
        for (String candidate : List.of(
                SmartTagModels.TAG_FIXED,
                SmartTagModels.TAG_REGRESSED,
                SmartTagModels.TAG_NEW,
                SmartTagModels.TAG_ALWAYS_FAILING,
                SmartTagModels.TAG_FLAKY,
                SmartTagModels.TAG_DURATION_ANOMALY)) {
            if (tags.contains(candidate)) {
                return candidate;
            }
        }
        return tags.get(0);
    }

    /**
     * Newest duration is anomalous when enough timed launches exist and it exceeds
     * {@code max(median * 2, mean + 2 * stdev)}. Unknown timings → false (never invent).
     */
    static boolean assessDurationAnomaly(List<AllureHistoryModels.LaunchStatus> launchesNewestFirst) {
        if (launchesNewestFirst == null || launchesNewestFirst.size() < MIN_LAUNCHES_FOR_DURATION) {
            return false;
        }
        List<Long> timed = new ArrayList<>();
        for (AllureHistoryModels.LaunchStatus launch : launchesNewestFirst) {
            if (launch.durationMs() > 0L) {
                timed.add(launch.durationMs());
            }
        }
        if (timed.size() < MIN_LAUNCHES_FOR_DURATION) {
            return false;
        }
        long newest = timed.get(0);
        List<Long> baseline = timed.subList(1, timed.size());
        double mean = baseline.stream().mapToLong(Long::longValue).average().orElse(0.0);
        double variance = 0.0;
        for (long value : baseline) {
            double delta = value - mean;
            variance += delta * delta;
        }
        double stdev = Math.sqrt(variance / baseline.size());
        List<Long> sorted = new ArrayList<>(baseline);
        sorted.sort(Long::compareTo);
        long median = sorted.get(sorted.size() / 2);
        double threshold = Math.max(median * 2.0, mean + 2.0 * stdev);
        return newest > threshold && newest > mean;
    }

    private static List<Outcome> passFailOutcomesNewestFirst(
            List<AllureHistoryModels.LaunchStatus> launchesNewestFirst) {
        if (launchesNewestFirst == null || launchesNewestFirst.isEmpty()) {
            return List.of();
        }
        List<Outcome> outcomes = new ArrayList<>();
        for (AllureHistoryModels.LaunchStatus launch : launchesNewestFirst) {
            String status = launch.status();
            if (isPassed(status)) {
                outcomes.add(new Outcome(status, true));
            } else if (isFailedOrBroken(status)) {
                outcomes.add(new Outcome(status, false));
            }
        }
        return outcomes;
    }

    private static int countFlips(List<Outcome> outcomesNewestFirst) {
        // Flip count is order-independent for adjacent pairs; walk chronological (oldest→newest).
        int flips = 0;
        for (int i = outcomesNewestFirst.size() - 1; i > 0; i--) {
            if (outcomesNewestFirst.get(i).passed() != outcomesNewestFirst.get(i - 1).passed()) {
                flips++;
            }
        }
        return flips;
    }

    private static List<AllureHistoryModels.LaunchStatus> truncateNewestFirst(
            List<AllureHistoryModels.LaunchStatus> launches, int window) {
        if (launches == null || launches.isEmpty()) {
            return List.of();
        }
        if (launches.size() <= window) {
            return List.copyOf(launches);
        }
        return List.copyOf(launches.subList(0, window));
    }

    private static int normalizeWindow(int windowSize) {
        if (windowSize <= 0) {
            return DEFAULT_WINDOW;
        }
        return Math.min(windowSize, 50);
    }

    private static int normalizeThreshold(int threshold) {
        if (threshold <= 0) {
            return DEFAULT_FLAKY_TRANSITION_THRESHOLD;
        }
        return threshold;
    }

    private static boolean isPassed(String status) {
        return "passed".equalsIgnoreCase(normalize(status));
    }

    private static boolean isFailedOrBroken(String status) {
        String normalized = normalize(status);
        return "failed".equals(normalized) || "broken".equals(normalized);
    }

    private static String normalize(String status) {
        return status == null ? "" : status.trim().toLowerCase(Locale.ROOT);
    }

    private record Outcome(String status, boolean passed) {
        boolean failed() {
            return !passed;
        }
    }
}
