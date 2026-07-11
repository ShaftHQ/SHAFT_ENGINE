package com.shaft.intellij.java;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Surfaces the SHAFT Capture generation report's readiness findings as file-level IDE
 * annotations on the generated test class (issue #3425 B3): blockers, warnings, flaky steps,
 * unsupported events, and required inputs show up where the developer actually reads the code,
 * instead of only inside a JSON report nobody opens.
 *
 * <p>The generated source lives under the generation output root, whose report is written to
 * {@code <outputRoot>/target/shaft-capture/generation-report.json}; this annotator walks up from
 * the file to find that report and only annotates when the report's {@code sourcePath} names the
 * annotated file.</p>
 */
public final class CaptureReportAnnotator implements Annotator {
    private static final String REPORT_RELATIVE_PATH = "target/shaft-capture/generation-report.json";
    private static final int MAX_PARENT_HOPS = 12;
    private static final int MAX_FINDINGS = 12;
    /** Per-report parse cache keyed by path, invalidated by last-modified time. */
    private static final Map<String, CachedReport> REPORT_CACHE = new ConcurrentHashMap<>();

    private record CachedReport(long lastModified, JsonObject report) {
    }

    @Override
    public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (!(element instanceof PsiFile file) || file.getVirtualFile() == null
                || !file.getName().endsWith(".java")) {
            return;
        }
        Path sourcePath;
        try {
            sourcePath = Path.of(file.getVirtualFile().getPath());
        } catch (RuntimeException invalidPath) {
            return;
        }
        JsonObject report = reportFor(sourcePath);
        if (report == null) {
            return;
        }
        for (String finding : findings(report)) {
            holder.newAnnotation(HighlightSeverity.WEAK_WARNING, "SHAFT Capture readiness: " + finding)
                    .fileLevel()
                    .create();
        }
    }

    private static JsonObject reportFor(Path sourcePath) {
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
