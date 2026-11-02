package com.shaft.doctor.history;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Computes dual flake definitions from Allure history + retries (issue #5968 / S3-02).
 *
 * <p>FR-001: retry-hidden and transition-count are independent. FR-002: CI commit metadata is
 * optional. FR-003: insufficient history yields {@code unknown}, not a 0% score. Edge: 100%
 * failing is not flaky.
 */
public final class DualFlakeComputer {
    public static final int DEFAULT_WINDOW = 10;
    public static final int DEFAULT_TRANSITION_THRESHOLD = 3;
    /** Need at least two launches to observe a transition; fewer ⇒ explicit unknown. */
    public static final int MIN_LAUNCHES_FOR_TRANSITIONS = 2;

    private static final String EMPTY_MESSAGE =
            "No flake rows yet. Provide Allure history.jsonl and/or allure-results retries "
                    + "(accumulateHistory=true) so retry-hidden and transitions can be assessed.";

    private DualFlakeComputer() {
    }

    /**
     * Builds a flake table from an already-ingested history view.
     *
     * @param history history + retries from {@link AllureHistoryIngestor}
     * @return dual-definition flake table
     */
    public static FlakeModels.FlakeTable compute(AllureHistoryModels.HistoryView history) {
        return compute(history, DEFAULT_WINDOW, DEFAULT_TRANSITION_THRESHOLD);
    }

    /**
     * Builds a flake table with an explicit transition window / threshold.
     *
     * @param history history + retries
     * @param windowSize max newest launches to consider (capped at 50)
     * @param transitionThreshold min flips to tag {@code transitions}
     * @return dual-definition flake table
     */
    public static FlakeModels.FlakeTable compute(
            AllureHistoryModels.HistoryView history, int windowSize, int transitionThreshold) {
        int window = normalizeWindow(windowSize);
        int threshold = normalizeThreshold(transitionThreshold);
        if (history == null) {
            return emptyTable(window, threshold);
        }

        Map<String, AllureHistoryModels.RetryGroup> retriesById = indexRetries(history.retries());
        Map<String, AllureHistoryModels.TestHistorySeries> seriesById = indexSeries(history.tests());
        Set<String> ids = new LinkedHashSet<>(seriesById.keySet());
        ids.addAll(retriesById.keySet());

        List<FlakeModels.FlakeRow> rows = new ArrayList<>();
        for (String historyId : ids) {
            rows.add(rowFor(historyId, seriesById.get(historyId), retriesById.get(historyId), window, threshold));
        }
        rows.sort(Comparator.comparing(FlakeModels.FlakeRow::historyId));

        List<String> warnings = new ArrayList<>(history.warnings());
        boolean empty = rows.isEmpty();
        return new FlakeModels.FlakeTable(
                FlakeModels.SCHEMA_VERSION,
                empty,
                empty ? EMPTY_MESSAGE : "",
                window,
                threshold,
                rows,
                warnings);
    }

    private static int normalizeWindow(int windowSize) {
        if (windowSize <= 0) {
            return DEFAULT_WINDOW;
        }
        return Math.min(windowSize, 50);
    }

    private static int normalizeThreshold(int transitionThreshold) {
        if (transitionThreshold <= 0) {
            return DEFAULT_TRANSITION_THRESHOLD;
        }
        return transitionThreshold;
    }

    private static FlakeModels.FlakeTable emptyTable(int window, int threshold) {
        return new FlakeModels.FlakeTable(
                FlakeModels.SCHEMA_VERSION, true, EMPTY_MESSAGE, window, threshold, List.of(), List.of());
    }

    private static Map<String, AllureHistoryModels.RetryGroup> indexRetries(
            List<AllureHistoryModels.RetryGroup> retries) {
        Map<String, AllureHistoryModels.RetryGroup> byId = new HashMap<>();
        for (AllureHistoryModels.RetryGroup group : retries) {
            byId.put(group.historyId(), group);
        }
        return byId;
    }

    private static Map<String, AllureHistoryModels.TestHistorySeries> indexSeries(
            List<AllureHistoryModels.TestHistorySeries> tests) {
        Map<String, AllureHistoryModels.TestHistorySeries> byId = new HashMap<>();
        for (AllureHistoryModels.TestHistorySeries series : tests) {
            byId.put(series.historyId(), series);
        }
        return byId;
    }

    private static FlakeModels.FlakeRow rowFor(
            String historyId,
            AllureHistoryModels.TestHistorySeries series,
            AllureHistoryModels.RetryGroup retries,
            int window,
            int threshold) {
        String name = displayName(series, retries);
        String fullName = series == null ? "" : series.fullName();
        boolean retryHidden = isRetryHidden(retries);
        List<AllureHistoryModels.LaunchStatus> launches = truncateNewestFirst(
                series == null ? List.of() : series.launches(), window);
        TransitionResult transitions = assessTransitions(launches, threshold);
        SameShaResult sameSha = assessSameSha(launches, threshold);
        return new FlakeModels.FlakeRow(
                historyId,
                name,
                fullName,
                retryHidden,
                retryHidden ? "retry-hidden" : "",
                transitions.assessment(),
                transitions.count(),
                launches.size(),
                sameSha.available(),
                sameSha.count(),
                buildTags(retryHidden, transitions.assessment()));
    }

    private static String displayName(
            AllureHistoryModels.TestHistorySeries series, AllureHistoryModels.RetryGroup retries) {
        if (series != null && !series.name().isBlank()) {
            return series.name();
        }
        return retries == null ? "" : retries.name();
    }

    private static List<String> buildTags(boolean retryHidden, String transitionAssessment) {
        List<String> tags = new ArrayList<>(2);
        if (retryHidden) {
            tags.add("retry-hidden");
        }
        if ("transitions".equals(transitionAssessment)) {
            tags.add("transitions");
        }
        return tags;
    }

    /**
     * Intra-run: a prior fail/broken attempt followed by a final pass (Doctor retry-correlation).
     */
    static boolean isRetryHidden(AllureHistoryModels.RetryGroup group) {
        if (group == null || group.attempts().size() < 2) {
            return false;
        }
        List<AllureHistoryModels.RetryAttempt> chronological = new ArrayList<>(group.attempts());
        chronological.sort(Comparator
                .comparingLong(AllureHistoryModels.RetryAttempt::start)
                .thenComparing(AllureHistoryModels.RetryAttempt::resultUuid));
        AllureHistoryModels.RetryAttempt last = chronological.getLast();
        if (!isPassed(last.status())) {
            return false;
        }
        for (int i = 0; i < chronological.size() - 1; i++) {
            if (isFailedOrBroken(chronological.get(i).status())) {
                return true;
            }
        }
        return false;
    }

    static TransitionResult assessTransitions(
            List<AllureHistoryModels.LaunchStatus> launchesNewestFirst, int threshold) {
        List<Boolean> outcomes = passFailOutcomes(launchesNewestFirst);
        if (outcomes.size() < MIN_LAUNCHES_FOR_TRANSITIONS) {
            // Explicit unknown — never report a fabricated 0% / zero-score as "known stable".
            return new TransitionResult("unknown", null);
        }
        int flips = countFlips(outcomes);
        if (outcomes.stream().noneMatch(Boolean::booleanValue)) {
            // 100% failing is not flaky (edge case).
            return new TransitionResult("always-failing", flips);
        }
        if (outcomes.stream().allMatch(Boolean::booleanValue)) {
            return new TransitionResult("always-passing", flips);
        }
        if (flips >= threshold) {
            return new TransitionResult("transitions", flips);
        }
        return new TransitionResult("below-threshold", flips);
    }

    private static List<Boolean> passFailOutcomes(
            List<AllureHistoryModels.LaunchStatus> launchesNewestFirst) {
        if (launchesNewestFirst == null || launchesNewestFirst.isEmpty()) {
            return List.of();
        }
        List<Boolean> outcomes = new ArrayList<>();
        for (int i = launchesNewestFirst.size() - 1; i >= 0; i--) {
            String status = launchesNewestFirst.get(i).status();
            if (isPassed(status)) {
                outcomes.add(Boolean.TRUE);
            } else if (isFailedOrBroken(status)) {
                outcomes.add(Boolean.FALSE);
            }
        }
        return outcomes;
    }

    private static int countFlips(List<Boolean> outcomes) {
        int flips = 0;
        for (int i = 1; i < outcomes.size(); i++) {
            if (!Objects.equals(outcomes.get(i - 1), outcomes.get(i))) {
                flips++;
            }
        }
        return flips;
    }

    static SameShaResult assessSameSha(
            List<AllureHistoryModels.LaunchStatus> launchesNewestFirst, int threshold) {
        if (launchesNewestFirst == null || launchesNewestFirst.isEmpty()) {
            return new SameShaResult(false, null);
        }
        String newestSha = firstCommitSha(launchesNewestFirst);
        if (newestSha.isBlank()) {
            return new SameShaResult(false, null);
        }
        List<AllureHistoryModels.LaunchStatus> cohort = launchesNewestFirst.stream()
                .filter(launch -> newestSha.equalsIgnoreCase(safeSha(launch)))
                .toList();
        TransitionResult result = assessTransitions(cohort, threshold);
        return new SameShaResult(true, result.count());
    }

    private static String firstCommitSha(List<AllureHistoryModels.LaunchStatus> launchesNewestFirst) {
        for (AllureHistoryModels.LaunchStatus launch : launchesNewestFirst) {
            String sha = safeSha(launch);
            if (!sha.isBlank()) {
                return sha;
            }
        }
        return "";
    }

    private static String safeSha(AllureHistoryModels.LaunchStatus launch) {
        return launch.commitSha() == null ? "" : launch.commitSha().trim();
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

    record TransitionResult(String assessment, Integer count) {
    }

    record SameShaResult(boolean available, Integer count) {
    }
}
