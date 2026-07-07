package com.shaft.capture.format;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;
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
     * Supports 0.9 → 1.0 and 1.0 → 1.1 deterministic migration chains.
     *
     * @param input parsed capture tree
     * @return migrated deep copy at CURRENT_SCHEMA_VERSION
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
        if (LEGACY_SCHEMA_VERSION.equals(version)) {
            migrated = migrateFrom0_9To1_0(migrated);
        } else if ("1.0".equals(version)) {
            // Already at 1.0, proceed to 1.1 migration below
        } else {
            throw new CaptureFormatException("Unsupported capture schema version '" + version
                    + "'. Supported versions are " + LEGACY_SCHEMA_VERSION + ", 1.0, and "
                    + CaptureSession.CURRENT_SCHEMA_VERSION + ".");
        }
        // Migrate from 1.0 to 1.1
        if ("1.0".equals(migrated.path("schemaVersion").asText(""))) {
            migrated = migrateFrom1_0To1_1(migrated);
        }
        return migrated;
    }

    /**
     * Migrates a 0.9 session tree to 1.0 by renaming eventType→type, state→status,
     * and ensuring required 1.0 fields exist.
     */
    private static ObjectNode migrateFrom0_9To1_0(ObjectNode tree) {
        tree.put("schemaVersion", "1.0");
        if (!tree.has("status") && tree.has("state")) {
            tree.set("status", tree.remove("state"));
        }
        tree.putIfAbsent("checkpoints", tree.arrayNode());
        tree.putIfAbsent("dataReferences", tree.arrayNode());
        tree.putIfAbsent("extensions", tree.objectNode());
        if (!tree.has("redactionSummary")) {
            ObjectNode summary = tree.putObject("redactionSummary");
            summary.put("redactedValueCount", 0);
            summary.put("removedAttributeCount", 0);
            summary.put("redactedUrlParameterCount", 0);
            summary.putArray("appliedRules");
        }
        JsonNode events = tree.path("events");
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
        return tree;
    }

    /**
     * Migrates a 1.0 session tree to 1.1 by stamping schemaVersion and adding
     * absent defaults without rewriting existing data (putIfAbsent style).
     */
    private static ObjectNode migrateFrom1_0To1_1(ObjectNode tree) {
        tree.put("schemaVersion", "1.1");
        // Additive 1.1 enhancements: ensure events and other structures have any new
        // required defaults without modifying existing content
        JsonNode events = tree.path("events");
        if (events instanceof ArrayNode array) {
            for (JsonNode item : array) {
                if (item instanceof ObjectNode event) {
                    // Ensure each event has required 1.1 fields (none currently added for UI-only events)
                    if (event.path("context") instanceof ObjectNode context) {
                        context.putIfAbsent("replayStatus", context.textNode("NOT_REPLAYED"));
                        context.putIfAbsent("evidence", context.arrayNode());
                        context.putIfAbsent("extensions", context.objectNode());
                    }
                }
            }
        }
        return tree;
    }
}
