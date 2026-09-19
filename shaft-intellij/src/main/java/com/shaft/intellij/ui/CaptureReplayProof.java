package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

/**
 * Replay-proof gate for Capture codegen results (issue #5962 / S2-06).
 *
 * <p>{@code capture_generate_replay} marks {@code report.status=SUCCESS} only when compile and
 * execute both pass. {@code UNCONFIRMED} means a draft that must not use the primary Keep/insert
 * production path; callers may still offer an explicit "copy unconfirmed" escape hatch.
 */
final class CaptureReplayProof {
    static final String SUCCESS = "SUCCESS";
    static final String UNCONFIRMED = "UNCONFIRMED";
    static final String FAILED = "FAILED";

    private CaptureReplayProof() {
    }

    static String reportStatus(JsonObject raw) {
        if (raw == null || !raw.has("report") || !raw.get("report").isJsonObject()) {
            return "";
        }
        JsonObject report = raw.getAsJsonObject("report");
        if (!report.has("status") || !report.get("status").isJsonPrimitive()) {
            return "";
        }
        return report.get("status").getAsString().trim();
    }

    /** Primary Keep/insert/create may run only when the prover returned {@link #SUCCESS}. */
    static boolean isProductionReady(JsonObject raw) {
        return SUCCESS.equals(reportStatus(raw));
    }

    static boolean isUnconfirmed(JsonObject raw) {
        return UNCONFIRMED.equals(reportStatus(raw));
    }

    /**
     * Loud canvas / sticky-strip label: SUCCESS vs UNCONFIRMED (or FAILED / unknown).
     */
    static String canvasLabel(JsonObject raw) {
        String status = reportStatus(raw);
        return switch (status) {
            case SUCCESS -> "SUCCESS — compiled and replay-proven; Keep is allowed";
            case UNCONFIRMED -> "UNCONFIRMED — draft only; Keep/insert blocked until replay proves SUCCESS";
            case FAILED -> "FAILED — generation/compile/replay did not pass; Keep blocked";
            default -> status.isBlank()
                    ? "No replay report — treat as unproven; Keep blocked"
                    : status + " — Keep blocked until SUCCESS";
        };
    }

    /**
     * Compile + execute evidence for the blocked Keep path (FR-003).
     */
    static String evidenceSummary(JsonObject raw) {
        if (raw == null || !raw.has("report") || !raw.get("report").isJsonObject()) {
            return "No compile/replay report in the result.";
        }
        JsonObject report = raw.getAsJsonObject("report");
        StringBuilder evidence = new StringBuilder();
        evidence.append("report.status=").append(reportStatus(raw));
        appendValidation(evidence, "compilation", validation(report, "compilation"));
        appendValidation(evidence, "replay", validation(report, "replay"));
        return evidence.toString();
    }

    private static JsonObject validation(JsonObject report, String key) {
        JsonElement value = report.get(key);
        return value != null && value.isJsonObject() ? value.getAsJsonObject() : new JsonObject();
    }

    private static void appendValidation(StringBuilder evidence, String label, JsonObject validation) {
        String status = string(validation, "status", "SKIPPED");
        evidence.append("; ").append(label).append("=").append(status);
        JsonElement diagnostics = validation.get("diagnostics");
        if (diagnostics == null || !diagnostics.isJsonArray()) {
            return;
        }
        JsonArray array = diagnostics.getAsJsonArray();
        int shown = 0;
        for (JsonElement diagnostic : array) {
            if (!diagnostic.isJsonPrimitive()) {
                continue;
            }
            evidence.append("; ").append(diagnostic.getAsString());
            shown++;
            if (shown >= 3) {
                break;
            }
        }
    }

    private static String string(JsonObject object, String key, String fallback) {
        if (object == null || !object.has(key) || !object.get(key).isJsonPrimitive()) {
            return fallback;
        }
        String value = object.get(key).getAsString();
        return value == null || value.isBlank() ? fallback : value;
    }
}
