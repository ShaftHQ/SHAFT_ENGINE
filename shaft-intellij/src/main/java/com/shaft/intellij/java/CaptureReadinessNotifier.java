package com.shaft.intellij.java;

import com.google.gson.JsonObject;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.shaft.intellij.notifications.ShaftNotifier;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Surfaces the SHAFT Capture generation report's readiness findings as a single, dismissable IDE
 * notification when the generated test class is actually opened (issue #3705), replacing {@link
 * CaptureReportAnnotator}'s previous per-finding file-level banners -- those stacked, truncated
 * with no expand affordance, and could never be dismissed because the {@code Annotator} SPI simply
 * re-created them on every highlighting pass.
 *
 * <p>Registered as a project-wide {@link FileEditorManagerListener} (see plugin.xml's {@code
 * <projectListeners>}, matching {@code FailedRunDoctorNotifier}'s wiring style) instead of an {@code
 * Annotator}, so this fires once per real file-open action rather than continuously. {@link
 * CaptureReportAnnotator#findings(JsonObject)} and {@link CaptureReportAnnotator#reportFor(Path)}
 * continue to do all the report-loading and finding-flattening work unchanged.</p>
 *
 * <p>{@code <projectListeners>} registration gives every project its own instance for the life of
 * that project (same reasoning as {@code FailedRunDoctorNotifier}'s {@code lastNotifiedAt}), so the
 * {@link #gate} instance field scopes dedup correctly without a static map leaking across
 * projects.</p>
 */
public final class CaptureReadinessNotifier implements FileEditorManagerListener {
    private final NotificationGate gate = new NotificationGate();

    @Override
    public void fileOpened(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
        if (!file.getName().endsWith(".java")) {
            return;
        }
        Path sourcePath;
        try {
            sourcePath = Path.of(file.getPath());
        } catch (RuntimeException invalidPath) {
            return;
        }
        JsonObject report = CaptureReportAnnotator.reportFor(sourcePath);
        if (report == null) {
            return;
        }
        gate.evaluate(sourcePath.toString(), report).ifPresent(plan -> fire(source.getProject(), plan));
    }

    @Override
    public void fileClosed(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
        gate.forget(file.getPath());
    }

    private static void fire(@Nullable Project project, NotificationPlan plan) {
        if (project == null) {
            return;
        }
        if (plan.warning()) {
            ShaftNotifier.warn(project, plan.title(), plan.content());
        } else {
            ShaftNotifier.info(project, plan.title(), plan.content());
        }
    }

    /**
     * A ready-to-fire notification: title, the full un-truncated findings content, and whether it
     * should be shown as a warning (BLOCKED readiness or any {@code readinessWarnings}/{@code
     * warnings}) or as plain info.
     */
    record NotificationPlan(String title, String content, boolean warning) {
    }

    /**
     * Decides whether a report is worth surfacing and, if so, what to show -- pure and fully unit
     * testable without any IntelliJ platform types (mirrors {@link CaptureReportAnnotator#findings}'s
     * PSI-free style). Returns {@link Optional#empty()} for a report with no actionable findings at
     * all, matching the previous annotator's behavior of creating zero annotations in that case.
     *
     * @param report parsed generation report
     * @return the notification to fire, or empty when there is nothing to say
     */
    static Optional<NotificationPlan> plan(JsonObject report) {
        List<String> findings = CaptureReportAnnotator.findings(report);
        if (findings.isEmpty()) {
            return Optional.empty();
        }
        String title = "SHAFT Capture readiness: " + findings.size()
                + (findings.size() == 1 ? " finding" : " findings");
        String content = findings.stream()
                .map(finding -> "&#8226; " + finding)
                .collect(Collectors.joining("<br>"));
        return Optional.of(new NotificationPlan(title, content, isWarning(report)));
    }

    private static boolean isWarning(JsonObject report) {
        return isBlocked(report) || hasEntries(report, "readinessWarnings") || hasEntries(report, "warnings");
    }

    private static boolean isBlocked(JsonObject report) {
        return report.has("readiness") && report.get("readiness").isJsonPrimitive()
                && "BLOCKED".equals(report.get("readiness").getAsString());
    }

    private static boolean hasEntries(JsonObject report, String key) {
        return report.has(key) && report.get(key).isJsonArray() && !report.getAsJsonArray(key).isEmpty();
    }

    /**
     * Fire-once, refire-on-reopen dedup: remembers the last report content notified per source-file
     * key so a still-open, unchanged file is not re-notified, while a changed report or a fresh
     * {@link #forget} (the file being closed, see {@link #fileClosed}) always notifies again.
     */
    static final class NotificationGate {
        private final Map<String, JsonObject> lastNotified = new HashMap<>();

        Optional<NotificationPlan> evaluate(String key, JsonObject report) {
            JsonObject previous = lastNotified.get(key);
            if (previous != null && previous.equals(report)) {
                return Optional.empty();
            }
            Optional<NotificationPlan> notificationPlan = plan(report);
            notificationPlan.ifPresent(ignored -> lastNotified.put(key, report));
            return notificationPlan;
        }

        void forget(String key) {
            lastNotified.remove(key);
        }
    }
}
