package com.shaft.intellij.java;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Loads and flattens the SHAFT Capture generation report's readiness findings for a generated test
 * class (issue #3425 B3): blockers, warnings, flaky steps, unsupported events, and required inputs,
 * so they can be surfaced to the developer instead of sitting only inside a JSON report nobody
 * opens. The actual surfacing is {@link CaptureReadinessNotifier}'s job (issue #3705) -- this class
 * is purely the report-loading/parsing/flattening logic it reuses, with no delivery mechanism of
 * its own.
 *
 * <p>The generated source lives under the generation output root, whose report is written to
 * {@code <outputRoot>/target/shaft-capture/generation-report.json}; {@link #reportFor} walks up
 * from the file to find that report and only returns it when the report's {@code sourcePath} names
 * the given file.</p>
 */
public final class CaptureReportAnnotator {
    private static final String REPORT_RELATIVE_PATH = "target/shaft-capture/generation-report.json";
    private static final int MAX_PARENT_HOPS = 12;
    private static final int MAX_FINDINGS = 12;
    /** Per-report parse cache keyed by path, invalidated by last-modified time. */
    private static final Map<String, CachedReport> REPORT_CACHE = new ConcurrentHashMap<>();

    private CaptureReportAnnotator() {
        throw new IllegalStateException("Utility class");
    }

    private record CachedReport(long lastModified, JsonObject report) {
    }

    static JsonObject reportFor(Path sourcePath) {
        Path current = sourcePath.getParent();
        for (int hop = 0; hop < MAX_PARENT_HOPS && current != null; hop++, current = current.getParent()) {
            Path reportFile = current.resolve(REPORT_RELATIVE_PATH);
            JsonObject report = readReport(reportFile);
            if (report != null && reportNamesFile(report, sourcePath)) {
                return report;
            }
        }
        return null;
    }

    private static boolean reportNamesFile(JsonObject report, Path sourcePath) {
        String reported = stringField(report, "sourcePath");
        if (reported.isBlank()) {
            return false;
        }
        String reportedName = Path.of(reported.replace('\\', '/')).getFileName().toString();
        return reportedName.equals(sourcePath.getFileName().toString());
    }

    private static JsonObject readReport(Path reportFile) {
        try {
            if (!Files.isRegularFile(reportFile)) {
                return null;
            }
            long lastModified = Files.getLastModifiedTime(reportFile).toMillis();
            CachedReport cached = REPORT_CACHE.get(reportFile.toString());
            if (cached != null && cached.lastModified() == lastModified) {
                return cached.report();
            }
            JsonElement parsed = JsonParser.parseString(Files.readString(reportFile));
            if (!parsed.isJsonObject()) {
                return null;
            }
            REPORT_CACHE.put(reportFile.toString(), new CachedReport(lastModified, parsed.getAsJsonObject()));
            return parsed.getAsJsonObject();
        } catch (Exception unreadableReport) {
            return null;
        }
    }

    /**
     * Flattens the report's actionable findings, labeled by category, capped so a pathological
     * report can never flood the editor.
     */
    static List<String> findings(JsonObject report) {
        List<String> findings = new ArrayList<>();
        appendArray(findings, report, "readinessWarnings", "");
        appendArray(findings, report, "warnings", "");
        appendArray(findings, report, "flakySteps", "potentially flaky — ");
        appendArray(findings, report, "unsupportedEvents", "not converted to code — ");
        appendArray(findings, report, "requiredUserInputs", "input required before running — ");
        appendArray(findings, report, "fallbackLocators", "fallback locator — ");
        return findings.size() > MAX_FINDINGS ? findings.subList(0, MAX_FINDINGS) : findings;
    }

    private static void appendArray(List<String> findings, JsonObject report, String key, String prefix) {
        if (!report.has(key) || !report.get(key).isJsonArray()) {
            return;
        }
        for (JsonElement item : report.getAsJsonArray(key)) {
            if (item.isJsonPrimitive() && !item.getAsString().isBlank()) {
                findings.add(prefix + item.getAsString());
            }
        }
    }

    private static String stringField(JsonObject object, String key) {
        return object.has(key) && object.get(key).isJsonPrimitive() ? object.get(key).getAsString() : "";
    }
}
