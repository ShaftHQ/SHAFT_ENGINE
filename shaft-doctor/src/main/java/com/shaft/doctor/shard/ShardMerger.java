package com.shaft.doctor.shard;

import com.shaft.doctor.model.ExecutionIntelligence;
import com.shaft.tools.internal.support.ReportHtmlTheme;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Merges N per-shard blobs (raw Allure results + optional traces + optional doctor
 * {@code ExecutionIntelligence}) produced by independent {@code -Dshaft.shard=N/M} runs into one
 * Allure result set plus a timeline "speedboard" HTML and a cross-shard flaky-clustering summary.
 *
 * <p>Merging raw Allure results is simple file union: every {@code *-result.json}/
 * {@code *-container.json}/attachment file is already independently UUID-named by the Allure
 * writer, so shards never collide on filename and the merge is a plain copy, not a JSON-level
 * merge. Flaky clustering groups merged results by {@code fullName} and flags any test observed
 * with both a passed and a failed/broken outcome across the merged set -- Playwright's
 * {@code merge-reports} has no equivalent failure intelligence.
 */
public final class ShardMerger {
    private static final ObjectMapper JSON = new JsonMapper();

    private ShardMerger() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Merges the given shard blob directories into {@code outputDirectory}.
     *
     * @param shardRoots shard blob root directories, in merge order
     * @param outputDirectory destination directory; created if absent
     * @return merge outcome (paths, counts, flaky clusters, warnings)
     */
    public static MergedReport merge(List<Path> shardRoots, Path outputDirectory) {
        Path output = outputDirectory.toAbsolutePath().normalize();
        Path mergedAllureResults = output.resolve("allure-results");
        Path mergedTraces = output.resolve("shaft-traces");
        List<String> warnings = new ArrayList<>();
        List<AllureResultSummary> allResults = new ArrayList<>();
        List<ShardIntelligence> shardIntelligence = new ArrayList<>();

        try {
            Files.createDirectories(mergedAllureResults);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not create merged Allure results directory.", exception);
        }

        int shardCount = 0;
        for (Path shardRoot : shardRoots) {
            ShardBlob blob = ShardBlob.of(shardRoot);
            if (!Files.isDirectory(blob.root())) {
                warnings.add("Shard blob does not exist, skipped: " + blob.root());
                continue;
            }
            shardCount++;
            String shardId = blob.root().getFileName() == null ? blob.root().toString() : blob.root().getFileName().toString();
            allResults.addAll(copyAllureResults(blob, shardId, mergedAllureResults, warnings));
            copyTraces(blob, shardId, mergedTraces, warnings);
            readShardIntelligence(blob, shardId, warnings).ifPresent(shardIntelligence::add);
        }

        Map<String, List<AllureResultSummary>> byFullName = new LinkedHashMap<>();
        for (AllureResultSummary result : allResults) {
            byFullName.computeIfAbsent(result.fullName(), key -> new ArrayList<>()).add(result);
        }
        List<FlakyCluster> flakyClusters = flakyClusters(byFullName);
        Path speedboard = output.resolve("speedboard.html");
        writeSpeedboard(speedboard, allResults, flakyClusters, shardCount, shardIntelligence);

        return new MergedReport(mergedAllureResults, speedboard, shardCount, allResults.size(),
                flakyClusters, List.copyOf(shardIntelligence), List.copyOf(warnings));
    }

    /**
     * Copies a shard's {@code shaft-traces} directory (if present) through untouched into
     * {@code <output>/shaft-traces/<shardId>/}, namespaced by shard id since two shards may
     * otherwise persist traces for differently-numbered attempts of the same test id.
     */
    private static void copyTraces(ShardBlob blob, String shardId, Path mergedTraces, List<String> warnings) {
        if (!Files.isDirectory(blob.traces())) {
            return;
        }
        Path destination = mergedTraces.resolve(shardId);
        try (Stream<Path> paths = Files.walk(blob.traces())) {
            for (Path source : paths.toList()) {
                Path target = destination.resolve(blob.traces().relativize(source).toString());
                if (Files.isDirectory(source)) {
                    Files.createDirectories(target);
                } else {
                    Files.createDirectories(target.getParent());
                    Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                }
            }
        } catch (IOException exception) {
            warnings.add("Could not copy shaft-traces for shard " + shardId + ": " + exception.getMessage());
        }
    }

    /**
     * Reads and reduces a shard's optional {@code execution-intelligence.json} (produced by an
     * out-of-band {@code shaft-doctor analyze} run against that shard's Allure results) into a
     * {@link ShardIntelligence} digest for cross-shard doctor-triage aggregation. A shard without
     * one, or with an unreadable one, is skipped with a warning rather than failing the merge.
     */
    private static java.util.Optional<ShardIntelligence> readShardIntelligence(
            ShardBlob blob, String shardId, List<String> warnings) {
        if (!Files.isRegularFile(blob.executionIntelligence())) {
            return java.util.Optional.empty();
        }
        try {
            ExecutionIntelligence intelligence = JSON.readValue(
                    Files.readString(blob.executionIntelligence(), StandardCharsets.UTF_8), ExecutionIntelligence.class);
            return java.util.Optional.of(new ShardIntelligence(
                    shardId,
                    intelligence.primaryCause() == null ? "UNKNOWN" : intelligence.primaryCause().name(),
                    intelligence.confidence() == null ? "UNKNOWN" : intelligence.confidence().name(),
                    intelligence.failingAttempts(),
                    intelligence.hiddenRetryFailures(),
                    intelligence.recurringFailures(),
                    intelligence.summary()));
        } catch (IOException | RuntimeException malformed) {
            warnings.add("Could not read execution-intelligence.json for shard " + shardId + ": " + malformed.getMessage());
            return java.util.Optional.empty();
        }
    }

    private static List<AllureResultSummary> copyAllureResults(
            ShardBlob blob, String shardId, Path mergedAllureResults, List<String> warnings) {
        List<AllureResultSummary> summaries = new ArrayList<>();
        if (!Files.isDirectory(blob.allureResults())) {
            warnings.add("Shard " + shardId + " has no allure-results directory.");
            return summaries;
        }
        try (Stream<Path> files = Files.list(blob.allureResults())) {
            for (Path file : files.filter(Files::isRegularFile).toList()) {
                copyIntoMerged(file, mergedAllureResults, shardId, warnings);
                if (file.getFileName().toString().endsWith("-result.json")) {
                    readResultSummary(file, shardId).ifPresent(summaries::add);
                }
            }
        } catch (IOException exception) {
            warnings.add("Could not list allure-results for shard " + shardId + ": " + exception.getMessage());
        }
        return summaries;
    }

    private static void copyIntoMerged(Path file, Path mergedAllureResults, String shardId, List<String> warnings) {
        try {
            Path destination = mergedAllureResults.resolve(file.getFileName());
            Files.copy(file, destination, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException exception) {
            warnings.add("Could not copy " + file.getFileName() + " from shard " + shardId + ": " + exception.getMessage());
        }
    }

    private static java.util.Optional<AllureResultSummary> readResultSummary(Path file, String shardId) {
        try {
            JsonNode root = JSON.readTree(Files.readString(file, StandardCharsets.UTF_8));
            String fullName = textOr(root, "fullName", textOr(root, "name", file.getFileName().toString()));
            String status = textOr(root, "status", "unknown").toLowerCase(Locale.ROOT);
            long start = root.path("start").asLong(0L);
            long stop = root.path("stop").asLong(0L);
            long durationMs = Math.max(0L, stop - start);
            return java.util.Optional.of(new AllureResultSummary(fullName, status, durationMs, shardId));
        } catch (IOException | RuntimeException malformed) {
            return java.util.Optional.empty();
        }
    }

    private static String textOr(JsonNode node, String field, String fallback) {
        JsonNode value = node.path(field);
        return value.isMissingNode() || value.isNull() || value.asText().isBlank() ? fallback : value.asText();
    }

    private static List<FlakyCluster> flakyClusters(Map<String, List<AllureResultSummary>> byFullName) {
        List<FlakyCluster> clusters = new ArrayList<>();
        byFullName.forEach((fullName, results) -> {
            int passed = (int) results.stream().filter(r -> "passed".equals(r.status())).count();
            int failed = (int) results.stream().filter(r -> "failed".equals(r.status()) || "broken".equals(r.status())).count();
            if (passed > 0 && failed > 0) {
                List<String> shardIds = results.stream().map(AllureResultSummary::shardId).distinct().toList();
                clusters.add(new FlakyCluster(fullName, passed, failed, shardIds));
            }
        });
        clusters.sort((a, b) -> Integer.compare(b.failedCount(), a.failedCount()));
        return List.copyOf(clusters);
    }

    private static void writeSpeedboard(
            Path destination, List<AllureResultSummary> results, List<FlakyCluster> flakyClusters, int shardCount,
            List<ShardIntelligence> shardIntelligence) {
        List<AllureResultSummary> slowestFirst = new ArrayList<>(results);
        slowestFirst.sort((a, b) -> Long.compare(b.durationMs(), a.durationMs()));

        StringBuilder html = new StringBuilder();
        html.append("<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">")
                .append("<title>SHAFT Merged Report Speedboard</title>")
                // Converge the Doctor speedboard onto the shared SHAFT report theme (issue #3534):
                // ReportHtmlTheme.style() supplies the canonical, theme-aware palette, and the
                // speedboard-specific layout below draws its colors from that palette (border,
                // surface, pass/fail/warn) instead of the former hardcoded off-palette hexes.
                .append("<style>").append(ReportHtmlTheme.style())
                .append("body{margin:24px}table{border-collapse:collapse;width:100%;margin:12px 0}")
                .append("th,td{border:1px solid var(--shaft-border);padding:6px 10px;text-align:left}")
                .append("th{background:var(--shaft-surface)}")
                .append("tr.flaky{background:color-mix(in srgb, var(--shaft-warn) 20%, transparent)}")
                .append(".failed{color:var(--shaft-fail)}.passed{color:var(--shaft-pass)}</style></head><body>")
                .append("<h1>SHAFT Merged Report Speedboard</h1>")
                .append("<p>Shards merged: ").append(shardCount)
                .append(" &middot; Total results: ").append(results.size())
                .append(" &middot; Flaky clusters: ").append(flakyClusters.size()).append("</p>");

        html.append("<h2>Doctor triage across shards</h2>");
        if (shardIntelligence.isEmpty()) {
            html.append("<p>No per-shard execution-intelligence.json was available.</p>");
        } else {
            html.append("<table><tr><th>Shard</th><th>Primary Cause</th><th>Confidence</th>")
                    .append("<th>Failing Attempts</th><th>Hidden Retry Failures</th><th>Recurring Failures</th>")
                    .append("<th>Summary</th></tr>");
            for (ShardIntelligence intelligence : shardIntelligence) {
                html.append("<tr><td>").append(escape(intelligence.shardId())).append("</td><td>")
                        .append(escape(intelligence.primaryCause())).append("</td><td>")
                        .append(escape(intelligence.confidence())).append("</td><td>")
                        .append(intelligence.failingAttempts()).append("</td><td>")
                        .append(intelligence.hiddenRetryFailures()).append("</td><td>")
                        .append(intelligence.recurringFailures()).append("</td><td>")
                        .append(escape(intelligence.summary())).append("</td></tr>");
            }
            html.append("</table>");
        }

        html.append("<h2>Flaky clusters (inconsistent pass/fail across shards)</h2>");
        if (flakyClusters.isEmpty()) {
            html.append("<p>No flaky clusters detected.</p>");
        } else {
            html.append("<table><tr><th>Test</th><th>Passed</th><th>Failed</th><th>Shards</th></tr>");
            for (FlakyCluster cluster : flakyClusters) {
                html.append("<tr class=\"flaky\"><td>").append(escape(cluster.fullName())).append("</td><td>")
                        .append(cluster.passedCount()).append("</td><td>").append(cluster.failedCount())
                        .append("</td><td>").append(escape(String.join(", ", cluster.shardIds()))).append("</td></tr>");
            }
            html.append("</table>");
        }

        html.append("<h2>Slowest tests</h2><table><tr><th>Test</th><th>Duration (ms)</th><th>Status</th><th>Shard</th></tr>");
        for (AllureResultSummary result : slowestFirst) {
            String statusClass = "failed".equals(result.status()) || "broken".equals(result.status()) ? "failed" : "passed";
            html.append("<tr><td>").append(escape(result.fullName())).append("</td><td>")
                    .append(result.durationMs()).append("</td><td class=\"").append(statusClass).append("\">")
                    .append(escape(result.status())).append("</td><td>").append(escape(result.shardId())).append("</td></tr>");
        }
        html.append("</table></body></html>\n");

        try {
            Files.createDirectories(destination.getParent());
            Files.writeString(destination, html.toString(), StandardCharsets.UTF_8);
        } catch (IOException exception) {
            throw new IllegalStateException("Could not write speedboard HTML.", exception);
        }
    }

    private static String escape(String value) {
        return value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private record AllureResultSummary(String fullName, String status, long durationMs, String shardId) {
    }
}
