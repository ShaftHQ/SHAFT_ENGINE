package com.shaft.tools.io.internal;

import com.google.gson.GsonBuilder;
import com.shaft.tools.internal.support.HTMLHelper;
import com.shaft.tools.internal.support.ReportHtmlTheme;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Aggregates validation checkpoints and attaches a summarized HTML report to Allure.
 *
 * <p>Each call to {@link #increment(CheckpointType, String, CheckpointStatus)} stores one checkpoint
 * entry and updates pass/fail counters. Call {@link #attach()} once at the end of execution to
 * publish the generated report attachment.
 */
public class CheckpointCounter {
    private static final List<CheckpointEntry> checkpoints = new CopyOnWriteArrayList<>();
    private static final AtomicInteger passedCheckpoints = new AtomicInteger(0);
    private static final AtomicInteger failedCheckpoints = new AtomicInteger(0);
    /**
     * Monotonic, process-wide checkpoint id source. Unlike the former per-attach ordinal (which
     * regenerated 1..N on every {@link #attach()}), an id assigned here is stable for the life of
     * the JVM, so Doctor / MCP and the report checkpoint-browser can correlate a checkpoint to a
     * test across attaches and re-generations (issues #3532 D / #3534 P3).
     */
    private static final AtomicInteger checkpointSequence = new AtomicInteger(0);

    /**
     * One recorded checkpoint. Carries a {@code stable} id assigned at capture time plus the
     * owning test's identity (as resolved from the current report context), so downstream
     * consumers can attribute and deep-link a checkpoint to its test.
     *
     * @param id stable, process-wide checkpoint id
     * @param type checkpoint type string ({@code ASSERTION}/{@code VERIFICATION})
     * @param message the checkpoint message
     * @param status checkpoint status string ({@code PASS}/{@code FAIL})
     * @param testClass owning test class name, or {@code ""} when unknown
     * @param testMethod owning test method name, or {@code ""} when unknown
     */
    record CheckpointEntry(int id, String type, String message, String status, String testClass,
                           String testMethod) {
        /**
         * Builds a stable {@code class#method} test identifier, or {@code ""} when no identity was
         * captured. Used by MCP / the report checkpoint-browser to correlate and "jump to test".
         *
         * @return the {@code class#method} test id, or an empty string
         */
        String testId() {
            if (testClass.isEmpty() && testMethod.isEmpty()) {
                return "";
            }
            return testClass + "#" + testMethod;
        }
    }

    /**
     * Creates a new checkpoint counter instance.
     */
    public CheckpointCounter() {
        super();
    }

    /**
     * Creates a new checkpoint entry and updates the pass/fail counters. The entry is stamped with
     * a stable, process-wide id and the current test's identity (class/method), so it can be
     * attributed and deep-linked to its owning test even though the checkpoint list is cumulative.
     *
     * @param type the checkpoint type (assertion or verification)
     * @param message the checkpoint message
     * @param status the final status of the checkpoint
     */
    public static void increment(CheckpointType type, String message, CheckpointStatus status) {
        checkpoints.add(new CheckpointEntry(checkpointSequence.incrementAndGet(), type.toString(),
                message, status.toString(), currentTestClass(), currentTestMethod()));

        if (status == CheckpointStatus.PASS) {
            passedCheckpoints.incrementAndGet();
        } else {
            failedCheckpoints.incrementAndGet();
        }
    }

    /**
     * Resolves the current test class from the per-thread report context, best-effort. Identity
     * lookup must never prevent a checkpoint from being recorded, so any failure yields {@code ""}.
     *
     * @return the current test class name, or {@code ""} when unavailable
     */
    private static String currentTestClass() {
        try {
            return ReportContext.getTestClassName();
        } catch (RuntimeException e) {
            return "";
        }
    }

    /**
     * Resolves the current test method from the per-thread report context, best-effort.
     *
     * @return the current test method name, or {@code ""} when unavailable
     */
    private static String currentTestMethod() {
        try {
            return ReportContext.getTestMethodName();
        } catch (RuntimeException e) {
            return "";
        }
    }

    /**
     * Attaches the accumulated checkpoint summary HTML report to the current report context.
     *
     * <p>If no checkpoints were recorded, this method does nothing.
     */
    public static void attach() {
        if (checkpoints.isEmpty()) {
            return;
        }
        StringBuilder detailsBuilder = new StringBuilder();
        for (CheckpointEntry entry : checkpoints) {
            detailsBuilder.append(String.format(
                    HTMLHelper.CHECKPOINT_DETAILS_FORMAT.getValue(),
                    entry.status(),
                    entry.id(),
                    ReportHtmlTheme.escapeHtml(entry.type()),
                    ReportHtmlTheme.escapeHtml(entry.message()),
                    ReportHtmlTheme.statusClass(entry.status()),
                    entry.status()));
        }

        long assertionsPassed = typeStatusCount(ASSERTION, PASS);
        long assertionsFailed = typeStatusCount(ASSERTION, FAIL);
        long verificationsPassed = typeStatusCount(VERIFICATION, PASS);
        long verificationsFailed = typeStatusCount(VERIFICATION, FAIL);

        ReportManagerHelper.attach("HTML",
                "SHAFT Overview",
                HTMLHelper.CHECKPOINT_COUNTER.getValue()
                        .replace("${CHECKPOINTS_PASSED_PERCENTAGE_LABEL}", String.valueOf(Math.round(passedCheckpoints.get() * 100d / checkpoints.size())))
                        .replace("${CHECKPOINTS_PASSED_DEGREES}", String.valueOf(passedCheckpoints.get() * 360d / checkpoints.size()))
                        .replace("${CHECKPOINTS_TOTAL}", String.valueOf(checkpoints.size()))
                        .replace("${CHECKPOINTS_PASSED}", String.valueOf(passedCheckpoints.get()))
                        .replace("${CHECKPOINTS_FAILED}", String.valueOf(failedCheckpoints.get()))
                        .replace("${TRACES_CAPTURED}", String.valueOf(capturedTraceCount()))
                        .replace("${ASSERTIONS_PASSED}", String.valueOf(assertionsPassed))
                        .replace("${ASSERTIONS_FAILED}", String.valueOf(assertionsFailed))
                        .replace("${ASSERTIONS_TOTAL}", String.valueOf(assertionsPassed + assertionsFailed))
                        .replace("${VERIFICATIONS_PASSED}", String.valueOf(verificationsPassed))
                        .replace("${VERIFICATIONS_FAILED}", String.valueOf(verificationsFailed))
                        .replace("${VERIFICATIONS_TOTAL}", String.valueOf(verificationsPassed + verificationsFailed))
                        .replace("${CHECKPOINTS_DETAILS}", detailsBuilder));

        attachCheckpointsJson();
    }

    private static final String ASSERTION = CheckpointType.ASSERTION.toString();
    private static final String VERIFICATION = CheckpointType.VERIFICATION.toString();
    private static final String PASS = CheckpointStatus.PASS.toString();
    private static final String FAIL = CheckpointStatus.FAIL.toString();

    /**
     * Counts checkpoints of a given type ({@code ASSERTION}/{@code VERIFICATION}) with a given
     * status ({@code PASS}/{@code FAIL}) from the accumulated list, for the per-type breakdown.
     *
     * @param type the checkpoint type string
     * @param status the checkpoint status string
     * @return the number of matching checkpoints
     */
    static long typeStatusCount(String type, String status) {
        return checkpoints.stream()
                .filter(entry -> entry.type().equals(type) && entry.status().equals(status))
                .count();
    }

    /**
     * Attaches a machine-readable JSON view of the accumulated checkpoints alongside the HTML
     * overview, so Doctor / MCP and the #3504 plugin widgets can consume structured checkpoint
     * fields (id, type, message, status) instead of scraping the HTML report (issue #3516 D).
     */
    static void attachCheckpointsJson() {
        ReportManagerHelper.attach("json", "Checkpoints", checkpointsJson());
    }

    /**
     * Builds the machine-readable checkpoint payload (totals plus one row per checkpoint with a
     * stable id, owning test identity, type, message, and status). The stable id and {@code testId}
     * let Doctor / MCP and the report checkpoint-browser correlate and deep-link a checkpoint to its
     * test across attaches (issues #3532 D / #3534 P3).
     *
     * @return the pretty-printed JSON payload
     */
    static String checkpointsJson() {
        List<Map<String, Object>> rows = new ArrayList<>();
        for (CheckpointEntry entry : checkpoints) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("id", entry.id());
            row.put("testId", entry.testId());
            row.put("testClass", entry.testClass());
            row.put("testMethod", entry.testMethod());
            row.put("type", entry.type());
            row.put("message", entry.message());
            row.put("status", entry.status());
            rows.add(row);
        }
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("total", checkpoints.size());
        payload.put("passed", passedCheckpoints.get());
        payload.put("failed", failedCheckpoints.get());
        payload.put("byType", Map.of(
                "assertion", Map.of("passed", typeStatusCount(ASSERTION, PASS), "failed", typeStatusCount(ASSERTION, FAIL)),
                "verification", Map.of("passed", typeStatusCount(VERIFICATION, PASS), "failed", typeStatusCount(VERIFICATION, FAIL))));
        payload.put("checkpoints", rows);
        return new GsonBuilder().setPrettyPrinting().create().toJson(payload);
    }

    /**
     * Counts persisted SHAFT trace directories (issue #3504 overview KPI, wired to the retry-aware
     * traces from #3503) so the report answers "how many failing attempts left a timeline?" at a
     * glance. Best-effort: any IO issue reports zero rather than failing report generation.
     *
     * @return number of per-test trace directories under {@code target/shaft-traces}
     */
    private static long capturedTraceCount() {
        java.nio.file.Path tracesRoot = java.nio.file.Path.of("target", "shaft-traces");
        if (!java.nio.file.Files.isDirectory(tracesRoot)) {
            return 0;
        }
        try (var entries = java.nio.file.Files.list(tracesRoot)) {
            return entries.filter(java.nio.file.Files::isDirectory).count();
        } catch (java.io.IOException | RuntimeException e) {
            return 0;
        }
    }
}
