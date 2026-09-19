package com.shaft.doctor.history;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.BufferedReader;
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
import java.util.stream.Stream;

/**
 * Append-only Allure history + Doctor JSON ingest for the Reporting canvas (issue #5967).
 *
 * <p>Never deletes, renames, or replaces the {@code allure-results} directory root. Missing
 * {@code history.jsonl} yields an empty view rather than an error.
 */
public final class AllureHistoryIngestor {
    private static final ObjectMapper JSON = JsonMapper.builder().build();
    private static final int DEFAULT_LIMIT = 10;
    private static final int MAX_LIMIT = 50;
    private static final String EMPTY_MESSAGE =
            "No Allure history yet. Re-run with allure.accumulateHistory=true (default) so "
                    + "target/history.jsonl is appended across launches.";

    private AllureHistoryIngestor() {
    }

    /**
     * Ingests Allure history.jsonl, optional Doctor JSON, and optional allure-results retries.
     *
     * @param historyJsonl path to Allure 3 history.jsonl (may be missing)
     * @param doctorJson optional Doctor report JSON (may be missing)
     * @param allureResultsRoot optional allure-results directory for intra-launch retries
     * @param limitPerHistoryId max launches retained per historyId (1..50, default 10)
     * @return immutable history view; {@code empty=true} when history.jsonl is missing/blank
     */
    public static AllureHistoryModels.HistoryView ingest(
            Path historyJsonl,
            Path doctorJson,
            Path allureResultsRoot,
            int limitPerHistoryId) {
        int limit = normalizeLimit(limitPerHistoryId);
        List<String> warnings = new ArrayList<>();
        Map<String, AllureHistoryModels.SeriesBuilder> series = new LinkedHashMap<>();
        int launchCount = 0;

        Path historyPath = historyJsonl == null ? null : historyJsonl.toAbsolutePath().normalize();
        if (historyPath == null || !Files.isRegularFile(historyPath)) {
            return new AllureHistoryModels.HistoryView(
                    "1.0",
                    true,
                    EMPTY_MESSAGE,
                    pathString(historyPath),
                    pathString(doctorJson),
                    pathString(allureResultsRoot),
                    false,
                    true,
                    limit,
                    0,
                    List.of(),
                    readRetries(allureResultsRoot, warnings),
                    warnings);
        }

        try (BufferedReader reader = Files.newBufferedReader(historyPath, StandardCharsets.UTF_8)) {
            String line;
            int lineNumber = 0;
            while ((line = reader.readLine()) != null) {
                lineNumber++;
                if (line.isBlank()) {
                    continue;
                }
                try {
                    JsonNode launch = JSON.readTree(line);
                    launchCount++;
                    ingestLaunch(launch, series);
                } catch (RuntimeException exception) {
                    warnings.add("history.jsonl line " + lineNumber + " skipped: " + exception.getMessage());
                }
            }
        } catch (IOException exception) {
            warnings.add("Unable to read history.jsonl: " + exception.getMessage());
            return new AllureHistoryModels.HistoryView(
                    "1.0",
                    true,
                    EMPTY_MESSAGE,
                    pathString(historyPath),
                    pathString(doctorJson),
                    pathString(allureResultsRoot),
                    false,
                    true,
                    limit,
                    0,
                    List.of(),
                    readRetries(allureResultsRoot, warnings),
                    warnings);
        }

        joinDoctor(doctorJson, series, warnings);
        List<AllureHistoryModels.TestHistorySeries> tests = new ArrayList<>();
        for (AllureHistoryModels.SeriesBuilder builder : series.values()) {
            tests.add(builder.build(limit));
        }
        tests.sort(Comparator.comparing(AllureHistoryModels.TestHistorySeries::historyId));

        boolean empty = tests.isEmpty();
        return new AllureHistoryModels.HistoryView(
                "1.0",
                empty,
                empty ? EMPTY_MESSAGE : "",
                pathString(historyPath),
                pathString(doctorJson),
                pathString(allureResultsRoot),
                false,
                true,
                limit,
                launchCount,
                tests,
                readRetries(allureResultsRoot, warnings),
                warnings);
    }

    private static void ingestLaunch(
            JsonNode launch, Map<String, AllureHistoryModels.SeriesBuilder> series) {
        String launchUuid = text(launch, "uuid", "id");
        String launchName = text(launch, "name", "reportName");
        long timestamp = launch.path("timestamp").asLong(0L);
        JsonNode testResults = launch.get("testResults");
        if (testResults == null || !testResults.isObject()) {
            testResults = launch.get("tests");
        }
        if (testResults == null || !testResults.isObject()) {
            return;
        }
        for (Map.Entry<String, JsonNode> entry : testResults.properties()) {
            String historyId = entry.getKey();
            JsonNode result = entry.getValue();
            if (historyId == null || historyId.isBlank()) {
                historyId = text(result, "historyId", "id");
            }
            if (historyId == null || historyId.isBlank()) {
                continue;
            }
            String name = text(result, "name", "title");
            String fullName = text(result, "fullName");
            String status = text(result, "status");
            String details = statusDetails(result);
            long duration = result.path("duration").asLong(
                    Math.max(0L, result.path("stop").asLong(0L) - result.path("start").asLong(0L)));
            AllureHistoryModels.SeriesBuilder builder =
                    series.computeIfAbsent(historyId, AllureHistoryModels.SeriesBuilder::new);
            builder.noteNames(name, fullName);
            builder.addLaunch(new AllureHistoryModels.LaunchStatus(
                    launchUuid, launchName, timestamp, status, details, duration, "HISTORY",
                    commitSha(launch, result)));
        }
    }

    /**
     * Optional CI/git SHA from launch or result metadata. Never required for flake v1 (FR-002).
     */
    private static String commitSha(JsonNode launch, JsonNode result) {
        String fromResult = firstNonBlank(
                text(result, "commitSha", "commit", "sha", "gitCommit"),
                text(result.path("labels"), "commit", "sha"),
                nestedText(result, "git", "commit", "sha", "commitSha"),
                nestedText(result, "ci", "commit", "sha", "commitSha"));
        if (!fromResult.isBlank()) {
            return fromResult;
        }
        return firstNonBlank(
                text(launch, "commitSha", "commit", "sha", "gitCommit"),
                nestedText(launch, "git", "commit", "sha", "commitSha"),
                nestedText(launch, "ci", "commit", "sha", "commitSha"));
    }

    private static String nestedText(JsonNode root, String objectField, String... fields) {
        if (root == null || root.isNull()) {
            return "";
        }
        JsonNode child = root.get(objectField);
        if (child == null || child.isNull()) {
            return "";
        }
        if (child.isTextual()) {
            return child.asText("").trim();
        }
        return text(child, fields);
    }

    private static void joinDoctor(
            Path doctorJson,
            Map<String, AllureHistoryModels.SeriesBuilder> series,
            List<String> warnings) {
        if (doctorJson == null || !Files.isRegularFile(doctorJson)) {
            return;
        }
        try {
            JsonNode root = JSON.readTree(Files.readString(doctorJson, StandardCharsets.UTF_8));
            Map<String, DoctorBits> byHistoryId = new HashMap<>();
            collectDoctorBits(root, byHistoryId);
            for (Map.Entry<String, DoctorBits> entry : byHistoryId.entrySet()) {
                AllureHistoryModels.SeriesBuilder builder = series.get(entry.getKey());
                if (builder != null) {
                    builder.joinDoctor(entry.getValue().cause(), entry.getValue().summary());
                }
            }
        } catch (IOException | RuntimeException exception) {
            warnings.add("Doctor JSON not joined: " + exception.getMessage());
        }
    }

    private static void collectDoctorBits(JsonNode node, Map<String, DoctorBits> sink) {
        if (node == null || node.isNull()) {
            return;
        }
        if (node.isObject()) {
            String historyId = text(node, "historyId");
            if (historyId.isBlank() && node.path("attributes").isObject()) {
                historyId = text(node.path("attributes"), "historyId");
            }
            if (!historyId.isBlank()) {
                String cause = firstNonBlank(
                        text(node, "primaryCause"),
                        text(node.path("diagnosis"), "primaryCause"),
                        text(node, "cause"));
                String summary = firstNonBlank(
                        text(node, "summary"),
                        text(node.path("diagnosis"), "summary"),
                        text(node, "message"));
                sink.merge(historyId, new DoctorBits(cause, summary), DoctorBits::merge);
            }
            for (JsonNode child : node) {
                collectDoctorBits(child, sink);
            }
        } else if (node.isArray()) {
            for (JsonNode child : node) {
                collectDoctorBits(child, sink);
            }
        }
    }

    private static List<AllureHistoryModels.RetryGroup> readRetries(
            Path allureResultsRoot, List<String> warnings) {
        if (allureResultsRoot == null || !Files.isDirectory(allureResultsRoot)) {
            return List.of();
        }
        Map<String, List<AllureHistoryModels.RetryAttempt>> byHistoryId = new LinkedHashMap<>();
        Map<String, String> names = new HashMap<>();
        try (Stream<Path> walk = Files.list(allureResultsRoot)) {
            List<Path> resultFiles = walk
                    .filter(path -> path.getFileName().toString().toLowerCase(Locale.ROOT)
                            .endsWith("-result.json"))
                    .sorted()
                    .toList();
            for (Path resultFile : resultFiles) {
                try {
                    JsonNode result = JSON.readTree(Files.readString(resultFile, StandardCharsets.UTF_8));
                    String historyId = text(result, "historyId");
                    if (historyId.isBlank()) {
                        continue;
                    }
                    String uuid = text(result, "uuid", "id");
                    String status = text(result, "status");
                    long start = result.path("start").asLong(0L);
                    long stop = result.path("stop").asLong(0L);
                    names.putIfAbsent(historyId, text(result, "name", "fullName"));
                    byHistoryId
                            .computeIfAbsent(historyId, key -> new ArrayList<>())
                            .add(new AllureHistoryModels.RetryAttempt(uuid, status, start, stop, "RETRY"));
                } catch (IOException | RuntimeException exception) {
                    warnings.add("Retry scan skipped " + resultFile.getFileName() + ": "
                            + exception.getMessage());
                }
            }
        } catch (IOException exception) {
            warnings.add("allure-results retry scan failed: " + exception.getMessage());
            return List.of();
        }
        List<AllureHistoryModels.RetryGroup> groups = new ArrayList<>();
        for (Map.Entry<String, List<AllureHistoryModels.RetryAttempt>> entry : byHistoryId.entrySet()) {
            List<AllureHistoryModels.RetryAttempt> attempts = new ArrayList<>(entry.getValue());
            if (attempts.size() < 2) {
                continue;
            }
            attempts.sort(Comparator.comparingLong(AllureHistoryModels.RetryAttempt::start).reversed());
            groups.add(new AllureHistoryModels.RetryGroup(
                    entry.getKey(), names.getOrDefault(entry.getKey(), ""), attempts));
        }
        groups.sort(Comparator.comparing(AllureHistoryModels.RetryGroup::historyId));
        return groups;
    }

    private static int normalizeLimit(int limitPerHistoryId) {
        if (limitPerHistoryId <= 0) {
            return DEFAULT_LIMIT;
        }
        return Math.min(limitPerHistoryId, MAX_LIMIT);
    }

    private static String pathString(Path path) {
        return path == null ? "" : path.toAbsolutePath().normalize().toString().replace('\\', '/');
    }

    private static String text(JsonNode node, String... fields) {
        if (node == null || node.isNull()) {
            return "";
        }
        for (String field : fields) {
            JsonNode value = node.get(field);
            if (value != null && !value.isNull() && !value.asText("").isBlank()) {
                return value.asText().trim();
            }
        }
        return "";
    }

    private static String statusDetails(JsonNode result) {
        JsonNode details = result.get("statusDetails");
        if (details != null && details.isObject()) {
            return text(details, "message", "trace");
        }
        if (details != null && details.isTextual()) {
            return details.asText("").trim();
        }
        return text(result, "statusMessage", "message");
    }

    private static String firstNonBlank(String... values) {
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                return value.trim();
            }
        }
        return "";
    }

    private record DoctorBits(String cause, String summary) {
        static DoctorBits merge(DoctorBits left, DoctorBits right) {
            return new DoctorBits(
                    firstNonBlank(left.cause, right.cause),
                    firstNonBlank(left.summary, right.summary));
        }
    }
}
