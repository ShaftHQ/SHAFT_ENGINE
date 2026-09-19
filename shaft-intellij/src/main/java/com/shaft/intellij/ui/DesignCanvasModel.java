package com.shaft.intellij.ui;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.util.ArrayList;
import java.util.List;

/**
 * Parsed {@code design_analyze} payload for the Design canvas (issue #5948).
 */
final class DesignCanvasModel {
    private String status = "";
    private String message = "Paste a story";
    private String actor = "";
    private String outcome = "";
    private int blockingCount;
    private boolean gherkinGenerationAllowed;
    private final List<String[]> acRows = new ArrayList<>();
    private final List<String[]> gapRows = new ArrayList<>();
    private final List<String[]> oracleRows = new ArrayList<>();
    private final List<String> waivableBlockingIds = new ArrayList<>();

    static DesignCanvasModel fromJson(String raw) {
        DesignCanvasModel model = new DesignCanvasModel();
        JsonObject root = parseAnalysisObject(raw);
        if (root == null) {
            model.message = raw == null || raw.isBlank() ? "No analysis result." : raw;
            return model;
        }
        model.status = string(root, "status");
        model.message = string(root, "message");
        model.blockingCount = root.has("blockingCount") ? root.get("blockingCount").getAsInt() : 0;
        model.gherkinGenerationAllowed = root.has("gherkinGenerationAllowed")
                && root.get("gherkinGenerationAllowed").getAsBoolean();
        JsonObject pack = root.has("pack") && root.get("pack").isJsonObject()
                ? root.getAsJsonObject("pack") : new JsonObject();
        model.actor = string(pack, "actor");
        model.outcome = string(pack, "outcome");
        fillCriteria(model, pack.get("acceptanceCriteria"));
        fillGaps(model, root.get("gaps"));
        fillOracles(model, root.get("oracles"));
        return model;
    }

    String status() {
        return status;
    }

    String message() {
        return message;
    }

    String actor() {
        return actor;
    }

    String outcome() {
        return outcome;
    }

    int blockingCount() {
        return blockingCount;
    }

    boolean gherkinGenerationAllowed() {
        return gherkinGenerationAllowed;
    }

    List<String[]> acRows() {
        return acRows;
    }

    List<String[]> gapRows() {
        return gapRows;
    }

    List<String[]> oracleRows() {
        return oracleRows;
    }

    List<String> waivableBlockingIds() {
        return waivableBlockingIds;
    }

    boolean acceptEnabled() {
        return !waivableBlockingIds.isEmpty();
    }

    String badgeText() {
        if (status.isBlank()) {
            return "Draft";
        }
        return status.replace('_', ' ') + " · blocking " + blockingCount;
    }

    String packStrip() {
        return "Actor: " + (actor.isBlank() ? "—" : actor)
                + "  ·  Outcome: " + (outcome.isBlank() ? "—" : outcome)
                + "  ·  AC " + acRows.size()
                + "  ·  blocking " + blockingCount;
    }

    String acceptedGapIdsArgument() {
        return String.join(",", waivableBlockingIds);
    }


    private static JsonObject parseAnalysisObject(String raw) {
        try {
            JsonElement parsed = JsonParser.parseString(raw == null ? "" : raw);
            if (parsed.isJsonObject() && parsed.getAsJsonObject().has("status")) {
                return parsed.getAsJsonObject();
            }
        } catch (RuntimeException ignored) {
            // Live MCP envelopes still go through unwrap.
        }
        return AssistantMarkdown.jsonObjectFromMcpOutput(raw);
    }

    static List<String> lexiconSuggestions(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        JsonElement suggestions = root == null ? null : root.get("suggestions");
        if (suggestions == null || !suggestions.isJsonArray()) {
            return List.of();
        }
        List<String> values = new ArrayList<>();
        for (JsonElement item : suggestions.getAsJsonArray()) {
            if (item.isJsonPrimitive()) {
                values.add(item.getAsString());
            }
        }
        return values;
    }

    static List<String[]> exampleRows(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        JsonElement rows = root == null ? null : root.get("rows");
        if (rows == null || !rows.isJsonArray()) {
            return List.of();
        }
        return copyExampleRows(rows.getAsJsonArray());
    }

    static List<String[]> lintRows(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        JsonElement findings = root == null ? null : root.get("findings");
        if (findings == null || !findings.isJsonArray()) {
            return List.of();
        }
        List<String[]> rows = new ArrayList<>();
        for (JsonElement item : findings.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject row = item.getAsJsonObject();
            rows.add(new String[]{string(row, "id"), string(row, "level"), string(row, "rule"), string(row, "message")});
        }
        return rows;
    }

    static boolean lintAcceptBlocked(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        return root != null && root.has("acceptBlocked") && root.get("acceptBlocked").getAsBoolean();
    }

    static List<String[]> gapMapRows(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        JsonElement steps = root == null ? null : root.get("steps");
        if (steps == null || !steps.isJsonArray()) {
            return List.of();
        }
        List<String[]> rows = new ArrayList<>();
        for (JsonElement item : steps.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject row = item.getAsJsonObject();
            rows.add(new String[]{
                    string(row, "stepText"),
                    string(row, "classification"),
                    string(row, "shaftType"),
                    string(row, "shaftMethod"),
                    string(row, "note")
            });
        }
        return rows;
    }

    
    static List<String[]> handoffRows(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        if (root == null) {
            return List.of();
        }
        List<String[]> rows = new ArrayList<>();
        rows.add(new String[]{
                string(root, "status"),
                string(root, "message"),
                string(root, "optionalUrl"),
                ""
        });
        JsonElement unmet = root.get("unmetConditions");
        if (unmet != null && unmet.isJsonArray()) {
            for (JsonElement item : unmet.getAsJsonArray()) {
                if (item.isJsonPrimitive()) {
                    rows.add(new String[]{"unmet", item.getAsString(), "", ""});
                }
            }
        }
        JsonElement scenarios = root.get("scenarios");
        if (scenarios != null && scenarios.isJsonArray()) {
            for (JsonElement item : scenarios.getAsJsonArray()) {
                if (item.isJsonPrimitive()) {
                    rows.add(new String[]{"scenario", item.getAsString(), "", ""});
                }
            }
        }
        JsonElement prefill = root.get("automationPrefill");
        if (prefill != null && prefill.isJsonObject()) {
            for (var entry : prefill.getAsJsonObject().entrySet()) {
                if (entry.getValue().isJsonPrimitive()) {
                    rows.add(new String[]{"prefill", entry.getKey(), entry.getValue().getAsString(), ""});
                }
            }
        }
        return rows;
    }

    static List<String[]> readinessRows(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        if (root == null) {
            return List.of();
        }
        List<String[]> rows = new ArrayList<>();
        rows.add(new String[]{
                string(root, "status"),
                string(root, "message"),
                Boolean.toString(root.has("handoffAllowed") && root.get("handoffAllowed").getAsBoolean()),
                ""
        });
        JsonElement unmet = root.get("unmetConditions");
        if (unmet != null && unmet.isJsonArray()) {
            for (JsonElement item : unmet.getAsJsonArray()) {
                if (item.isJsonPrimitive()) {
                    rows.add(new String[]{"unmet", item.getAsString(), "", ""});
                }
            }
        }
        return rows;
    }

    static boolean handoffAllowed(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        return root != null && root.has("handoffAllowed") && root.get("handoffAllowed").getAsBoolean();
    }

    static List<String[]> coverageRows(String raw) {
        JsonObject root = parseAnalysisObject(raw);
        if (root == null) {
            return List.of();
        }
        List<String[]> rows = new ArrayList<>();
        addCoverageState(rows, root.get("covered"), "covered", "");
        addCoverageState(rows, root.get("uncovered"), "uncovered", "");
        addWaivedRows(rows, root.get("waived"));
        return rows;
    }

    private static void addCoverageState(List<String[]> rows, JsonElement element, String state, String reason) {
        if (element == null || !element.isJsonArray()) {
            return;
        }
        for (JsonElement item : element.getAsJsonArray()) {
            if (item.isJsonPrimitive()) {
                rows.add(new String[]{item.getAsString(), state, reason});
            }
        }
    }

    private static void addWaivedRows(List<String[]> rows, JsonElement element) {
        if (element == null || !element.isJsonArray()) {
            return;
        }
        for (JsonElement item : element.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject row = item.getAsJsonObject();
            rows.add(new String[]{string(row, "id"), "waived", string(row, "reason")});
        }
    }

    private static List<String[]> copyExampleRows(JsonArray array) {
        List<String[]> rows = new ArrayList<>();
        for (JsonElement item : array) {
            addExampleRow(rows, item);
        }
        return rows;
    }

    private static void addExampleRow(List<String[]> rows, JsonElement item) {
        if (!item.isJsonObject()) {
            return;
        }
        JsonObject row = item.getAsJsonObject();
        rows.add(new String[]{string(row, "id"), string(row, "kind"),
                row.has("cells") ? row.get("cells").toString() : ""});
    }

    private static void fillCriteria(DesignCanvasModel model, JsonElement element) {
        if (element == null || !element.isJsonArray()) {
            return;
        }
        for (JsonElement item : element.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject row = item.getAsJsonObject();
            model.acRows.add(new String[]{string(row, "id"), string(row, "text")});
        }
    }

    private static void fillGaps(DesignCanvasModel model, JsonElement element) {
        if (element == null || !element.isJsonArray()) {
            return;
        }
        JsonArray array = element.getAsJsonArray();
        for (JsonElement item : array) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject row = item.getAsJsonObject();
            boolean waivable = row.has("waivable") && row.get("waivable").getAsBoolean();
            boolean accepted = row.has("accepted") && row.get("accepted").getAsBoolean();
            String severity = string(row, "severity");
            String id = string(row, "id");
            model.gapRows.add(new String[]{
                    id,
                    string(row, "kind"),
                    severity,
                    string(row, "rank"),
                    joined(row.get("tracedAcIds")),
                    string(row, "question"),
                    accepted ? "yes" : "no"
            });
            if (waivable && !accepted && "blocking".equals(severity) && !id.isBlank()) {
                model.waivableBlockingIds.add(id);
            }
        }
    }

    private static void fillOracles(DesignCanvasModel model, JsonElement element) {
        if (element == null || !element.isJsonArray()) {
            return;
        }
        for (JsonElement item : element.getAsJsonArray()) {
            if (!item.isJsonObject()) {
                continue;
            }
            JsonObject row = item.getAsJsonObject();
            model.oracleRows.add(new String[]{string(row, "acId"), string(row, "oracle"), string(row, "evidence")});
        }
    }

    private static String joined(JsonElement element) {
        if (element == null || !element.isJsonArray()) {
            return "";
        }
        List<String> values = new ArrayList<>();
        for (JsonElement item : element.getAsJsonArray()) {
            values.add(item.getAsString());
        }
        return String.join(", ", values);
    }

    private static String string(JsonObject object, String key) {
        if (object == null || !object.has(key) || object.get(key).isJsonNull()) {
            return "";
        }
        JsonElement value = object.get(key);
        return value.isJsonPrimitive() ? value.getAsString() : value.toString();
    }
}
