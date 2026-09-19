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
        int window = windowSize <= 0 ? DEFAULT_WINDOW : Math.min(windowSize, 50);
        int threshold = transitionThreshold <= 0 ? DEFAULT_TRANSITION_THRESHOLD : transitionThreshold;
        if (history == null) {
            return new FlakeModels.FlakeTable("1.0", true, EMPTY_MESSAGE, window, threshold, List.of(), List.of());
        }

        Map<String, AllureHistoryModels.RetryGroup> retriesById = new HashMap<>();
        for (AllureHistoryModels.RetryGroup group : history.retries()) {
            retriesById.put(group.historyId(), group);
        }

        Set<String> ids = new LinkedHashSet<>();
        Map<String, AllureHistoryModels.TestHistorySeries> seriesById = new HashMap<>();
        for (AllureHistoryModels.TestHistorySeries series : history.tests()) {
            ids.add(series.historyId());
            seriesById.put(series.historyId(), series);
        }
        ids.addAll(retriesById.keySet());

        List<FlakeModels.FlakeRow> rows = new ArrayList<>();
        for (String historyId : ids) {
            AllureHistoryModels.TestHistorySeries series = seriesById.get(historyId);
            AllureHistoryModels.RetryGroup retries = retriesById.get(historyId);
            rows.add(rowFor(historyId, series, retries, window, threshold));
        }
        rows.sort(Comparator.comparing(FlakeModels.FlakeRow::historyId));

        boolean empty = rows.isEmpty();
        List<String> warnings = new ArrayList<>(history.warnings());
        if (!empty && history.empty() && history.retries().isEmpty()) {
            // Defensive: ingest said empty but we somehow have rows.
            warnings.add("History view reported empty; flake rows came from retries only.");
        }
        return new FlakeModels.FlakeTable(
                "1.0",
                empty,
                empty ? EMPTY_MESSAGE : "",
                window,
                threshold,
                rows,
                warnings);
    }

    private static FlakeModels.FlakeRow rowFor(
            String historyId,
            AllureHistoryModels.TestHistorySeries series,
            AllureHistoryModels.RetryGroup retries,
            int window,
            int threshold) {
        String name = series != null && !series.name().isBlank()
                ? series.name()
                : retries != null ? retries.name() : "";
        String fullName = series == null ? "" : series.fullName();

        boolean retryHidden = isRetryHidden(retries);
        String retryTag = retryHidden ? "retry-hidden" : "";

        List<AllureHistoryModels.LaunchStatus> launches = series == null
                ? List.of()
                : truncateNewestFirst(series.launches(), window);
        TransitionResult transitions = assessTransitions(launches, threshold);
        SameShaResult sameSha = assessSameSha(launches, threshold);

        List<String> tags = new ArrayList<>();
        if (retryHidden) {
            tags.add("retry-hidden");
        }
        if ("transitions".equals(transitions.assessment())) {
            tags.add("transitions");
        }

        return new FlakeModels.FlakeRow(
                historyId,
                name,
                fullName,
                retryHidden,
                retryTag,
                transitions.assessment(),
                transitions.count(),
                launches.size(),
                sameSha.available(),
                sameSha.count(),
                tags);
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
        return chronological.subList(0, chronological.size() - 1).stream()
                .anyMatch(attempt -> isFailedOrBroken(attempt.status()));
    }

    static TransitionResult assessTransitions(
            List<AllureHistoryModels.LaunchStatus> launchesNewestFirst, int threshold) {
        if (launchesNewestFirst == null || launchesNewestFirst.size() < MIN_LAUNCHES_FOR_TRANSITIONS) {
            // Explicit unknown — never report a fabricated 0% / zero-score as "known stable".
            return new TransitionResult("unknown", null);
        }
        List<Boolean> outcomes = new ArrayList<>();
        for (int i = launchesNewestFirst.size() - 1; i >= 0; i--) {
            String status = launchesNewestFirst.get(i).status();
            if (isPassed(status)) {
                outcomes.add(Boolean.TRUE);
            } else if (isFailedOrBroken(status)) {
                outcomes.add(Boolean.FALSE);
            }
            // skipped/unknown statuses do not contribute to pass/fail polarity
        }
        if (outcomes.size() < MIN_LAUNCHES_FOR_TRANSITIONS) {
            return new TransitionResult("unknown", null);
        }
        boolean allFail = outcomes.stream().noneMatch(Boolean::booleanValue);
        boolean allPass = outcomes.stream().allMatch(Boolean::booleanValue);
        int flips = 0;
        for (int i = 1; i < outcomes.size(); i++) {
            if (!Objects.equals(outcomes.get(i - 1), outcomes.get(i))) {
                flips++;
            }
        }
        if (allFail) {
            // 100% failing is not flaky (edge case).
            return new TransitionResult("always-failing", flips);
        }
        if (allPass) {
            return new TransitionResult("always-passing", flips);
        }
        if (flips >= threshold) {
            return new TransitionResult("transitions", flips);
        }
        return new TransitionResult("below-threshold", flips);
    }

    static SameShaResult assessSameSha(
            List<AllureHistoryModels.LaunchStatus> launchesNewestFirst, int threshold) {
        if (launchesNewestFirst == null || launchesNewestFirst.isEmpty()) {
            return new SameShaResult(false, null);
        }
        boolean anySha = launchesNewestFirst.stream()
                .anyMatch(launch -> launch.commitSha() != null && !launch.commitSha().isBlank());
        if (!anySha) {
            return new SameShaResult(false, null);
        }
        String newestSha = "";
        for (AllureHistoryModels.LaunchStatus launch : launchesNewestFirst) {
            if (launch.commitSha() != null && !launch.commitSha().isBlank()) {
                newestSha = launch.commitSha().trim();
                break;
            }
        }
        if (newestSha.isBlank()) {
            return new SameShaResult(false, null);
        }
        String sha = newestSha;
        List<AllureHistoryModels.LaunchStatus> cohort = launchesNewestFirst.stream()
                .filter(launch -> sha.equalsIgnoreCase(
                        launch.commitSha() == null ? "" : launch.commitSha().trim()))
                .toList();
        TransitionResult result = assessTransitions(cohort, threshold);
        return new SameShaResult(true, result.count());
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
