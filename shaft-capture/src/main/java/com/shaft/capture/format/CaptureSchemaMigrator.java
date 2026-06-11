package com.shaft.capture.format;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.capture.model.CaptureSession;

/**
 * Deterministic migrations for supported older capture schemas.
 */
public final class CaptureSchemaMigrator {
    public static final String LEGACY_SCHEMA_VERSION = "0.9";

    private CaptureSchemaMigrator() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Migrates a supported capture tree to the current schema.
     *
     * @param input parsed capture tree
     * @return migrated deep copy
     */
    public static ObjectNode migrate(JsonNode input) {
        if (input == null || !input.isObject()) {
            throw new CaptureFormatException("Capture root must be a JSON object.");
        }
        ObjectNode migrated = ((ObjectNode) input).deepCopy();
        String version = migrated.path("schemaVersion").asText("");
        if (CaptureSession.CURRENT_SCHEMA_VERSION.equals(version)) {
            return migrated;
        }
        if (!LEGACY_SCHEMA_VERSION.equals(version)) {
            throw new CaptureFormatException("Unsupported capture schema version '" + version
                    + "'. Supported versions are " + LEGACY_SCHEMA_VERSION + " and "
                    + CaptureSession.CURRENT_SCHEMA_VERSION + ".");
        }
        migrated.put("schemaVersion", CaptureSession.CURRENT_SCHEMA_VERSION);
        if (!migrated.has("status") && migrated.has("state")) {
            migrated.set("status", migrated.remove("state"));
        }
        migrated.putIfAbsent("checkpoints", migrated.arrayNode());
        migrated.putIfAbsent("dataReferences", migrated.arrayNode());
        migrated.putIfAbsent("extensions", migrated.objectNode());
        if (!migrated.has("redactionSummary")) {
            ObjectNode summary = migrated.putObject("redactionSummary");
            summary.put("redactedValueCount", 0);
            summary.put("removedAttributeCount", 0);
            summary.put("redactedUrlParameterCount", 0);
            summary.putArray("appliedRules");
        }
        JsonNode events = migrated.path("events");
        if (events instanceof ArrayNode array) {
            for (JsonNode item : array) {
                if (item instanceof ObjectNode event && !event.has("type") && event.has("eventType")) {
                    event.set("type", event.remove("eventType"));
                }
                if (item instanceof ObjectNode event && event.path("context") instanceof ObjectNode context) {
                    context.putIfAbsent("replayStatus", context.textNode("NOT_REPLAYED"));
                    context.putIfAbsent("evidence", context.arrayNode());
                    context.putIfAbsent("extensions", context.objectNode());
                }
            }
        }
        return migrated;
    }
}
