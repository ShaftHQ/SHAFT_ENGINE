package com.shaft.intellij.testindex;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Lightweight reader for S3-01 {@code history.jsonl}, used to decorate the local SHAFT Tests tree
 * with S3-09 smart tags without waiting for an MCP round-trip (issue #5975).
 *
 * <p>The doctor module remains the canonical reporting implementation. This plugin-side reader
 * applies the same deterministic status rules and never invents Flaky from insufficient history.
 */
public final class SmartTagHistoryReader {
    static final int FLAKY_TRANSITION_THRESHOLD = 3;
    static final int MIN_LAUNCHES_FOR_FLAKY = 3;

    private SmartTagHistoryReader() {
    }

    /**
     * Reads smart tags keyed by historyId, name, fullName, and {@code qualifiedClass#method}.
     * Missing/malformed history is an empty map, never a UI crash.
     */
    public static Map<String, List<String>> read(Path historyJsonl) {
        if (historyJsonl == null || !Files.isRegularFile(historyJsonl)) {
            return Map.of();
        }
        Map<String, Series> series = new LinkedHashMap<>();
        try {
            for (String line : Files.readAllLines(historyJsonl, StandardCharsets.UTF_8)) {
                ingestLine(line, series);
            }
        } catch (IOException | RuntimeException ignored) {
            return Map.of();
        }

        Map<String, List<String>> tagsByKey = new HashMap<>();
        for (Series value : series.values()) {
            List<String> tags = tags(value.observations());
            if (tags.isEmpty()) {
                continue;
            }
            put(tagsByKey, value.historyId(), tags);
            put(tagsByKey, value.name(), tags);
            put(tagsByKey, value.fullName(), tags);
            put(tagsByKey, toMethodKey(value.fullName()), tags);
        }
        return Map.copyOf(tagsByKey);
    }

    private static void ingestLine(String line, Map<String, Series> series) {
        if (line == null || line.isBlank()) {
            return;
        }
        JsonObject launch = JsonParser.parseString(line).getAsJsonObject();
        long timestamp = longValue(launch, "timestamp");
        JsonObject results = object(launch, "testResults");
        if (results == null) {
            results = object(launch, "tests");
        }
        if (results == null) {
            return;
        }
        for (Map.Entry<String, JsonElement> entry : results.entrySet()) {
            if (!entry.getValue().isJsonObject()) {
                continue;
            }
            JsonObject result = entry.getValue().getAsJsonObject();
            String historyId = firstNonBlank(entry.getKey(), text(result, "historyId"));
            if (historyId.isBlank()) {
                continue;
            }
            Series item = series.computeIfAbsent(historyId, Series::new);
            item.note(text(result, "name"), text(result, "fullName"));
            item.add(new Observation(timestamp, text(result, "status")));
        }
    }

    static List<String> tags(List<Observation> raw) {
        List<Observation> outcomes = knownOutcomes(raw);
        if (outcomes.isEmpty()) {
            return List.of();
        }
        if (outcomes.size() == 1) {
            return firstSeenTags(outcomes.get(0));
        }

        List<String> tags = changedStatusTags(outcomes.get(0), outcomes.get(1));
        addAlwaysFailing(outcomes, tags);
        addFlaky(outcomes, tags);
        return List.copyOf(tags);
    }

    private static List<Observation> knownOutcomes(List<Observation> raw) {
        return raw.stream()
                .filter(observation -> isPassed(observation.status()) || isFailed(observation.status()))
                .sorted(Comparator.comparingLong(Observation::timestamp).reversed())
                .limit(10)
                .toList();
    }

    private static List<String> firstSeenTags(Observation outcome) {
        return isFailed(outcome.status()) ? List.of("New") : List.of();
    }

    private static List<String> changedStatusTags(Observation newest, Observation previous) {
        List<String> tags = new ArrayList<>();
        if (isPassed(newest.status()) && isFailed(previous.status())) {
            tags.add("Fixed");
        } else if (isFailed(newest.status()) && isPassed(previous.status())) {
            tags.add("Regressed");
        }
        return tags;
    }

    private static void addAlwaysFailing(List<Observation> outcomes, List<String> tags) {
        if (outcomes.stream().allMatch(outcome -> isFailed(outcome.status()))) {
            tags.add("Always-failing");
        }
    }

    private static void addFlaky(List<Observation> outcomes, List<String> tags) {
        if (isFlaky(outcomes)) {
            tags.add("Flaky");
        }
    }

    private static boolean isFlaky(List<Observation> outcomes) {
        if (outcomes.size() < MIN_LAUNCHES_FOR_FLAKY
                || countFlips(outcomes) < FLAKY_TRANSITION_THRESHOLD) {
            return false;
        }
        boolean allFail = outcomes.stream().allMatch(outcome -> isFailed(outcome.status()));
        boolean allPass = outcomes.stream().allMatch(outcome -> isPassed(outcome.status()));
        return !allFail && !allPass;
    }

    private static int countFlips(List<Observation> outcomesNewestFirst) {
        int flips = 0;
        for (int index = 1; index < outcomesNewestFirst.size(); index++) {
            if (isPassed(outcomesNewestFirst.get(index - 1).status())
                    != isPassed(outcomesNewestFirst.get(index).status())) {
                flips++;
            }
        }
        return flips;
    }

    private static String toMethodKey(String fullName) {
        if (fullName == null || fullName.isBlank()) {
            return "";
        }
        int separator = fullName.lastIndexOf('.');
        return separator < 0 ? fullName : fullName.substring(0, separator) + "#" + fullName.substring(separator + 1);
    }

    private static void put(Map<String, List<String>> sink, String key, List<String> tags) {
        if (key != null && !key.isBlank()) {
            sink.put(key, tags);
        }
    }

    private static JsonObject object(JsonObject root, String name) {
        if (!root.has(name) || !root.get(name).isJsonObject()) {
            return null;
        }
        return root.getAsJsonObject(name);
    }

    private static String text(JsonObject object, String name) {
        return object.has(name) && !object.get(name).isJsonNull()
                ? object.get(name).getAsString().trim()
                : "";
    }

    private static long longValue(JsonObject object, String name) {
        return object.has(name) && object.get(name).isJsonPrimitive()
                ? object.get(name).getAsLong()
                : 0L;
    }

    private static String firstNonBlank(String first, String second) {
        return first != null && !first.isBlank() ? first : (second == null ? "" : second);
    }

    private static boolean isPassed(String status) {
        return "passed".equals(normalize(status));
    }

    private static boolean isFailed(String status) {
        String normalized = normalize(status);
        return "failed".equals(normalized) || "broken".equals(normalized);
    }

    private static String normalize(String status) {
        return status == null ? "" : status.trim().toLowerCase(Locale.ROOT);
    }

    record Observation(long timestamp, String status) {
    }

    private static final class Series {
        private final String historyId;
        private String name = "";
        private String fullName = "";
        private final List<Observation> observations = new ArrayList<>();

        private Series(String historyId) {
            this.historyId = historyId;
        }

        void note(String nextName, String nextFullName) {
            if (nextName != null && !nextName.isBlank()) {
                name = nextName;
            }
            if (nextFullName != null && !nextFullName.isBlank()) {
                fullName = nextFullName;
            }
        }

        void add(Observation observation) {
            observations.add(observation);
        }

        String historyId() {
            return historyId;
        }

        String name() {
            return name;
        }

        String fullName() {
            return fullName;
        }

        List<Observation> observations() {
            return observations;
        }
    }
}
