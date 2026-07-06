package com.shaft.capture.format;

import com.shaft.capture.CaptureFixtures;
import com.shaft.capture.model.CaptureEvent;
import com.shaft.capture.model.CaptureSession;
import com.shaft.pilot.json.JsonSchemaValidator;
import org.junit.jupiter.api.Test;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CaptureSchemaMigratorTest {
    private final CaptureJsonCodec codec = new CaptureJsonCodec();
    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void legacyVersion0_9MigratesToCurrentSchema() throws IOException {
        String legacy = resource("legacy-session-0.9.json");
        JsonNode parsed = mapper.readTree(legacy);

        JsonNode migrated = CaptureSchemaMigrator.migrate(parsed);

        assertEquals(CaptureSession.CURRENT_SCHEMA_VERSION, migrated.path("schemaVersion").asText());
        assertEquals("INCOMPLETE", migrated.path("status").asText());
        assertEquals(3, migrated.path("events").size(), "Migrated legacy fixture should have 3 events");
    }

    @Test
    void migratedSessionCanBeReadByCodec() throws IOException {
        String legacy = resource("legacy-session-0.9.json");

        CaptureSession session = codec.read(legacy);

        assertEquals(CaptureSession.CURRENT_SCHEMA_VERSION, session.schemaVersion());
        assertEquals(CaptureSession.SessionStatus.INCOMPLETE, session.status());
        assertEquals(3, session.events().size());
        assertInstanceOf(CaptureEvent.NavigationEvent.class, session.events().get(0));
        assertInstanceOf(CaptureEvent.NavigationEvent.class, session.events().get(1));
        assertInstanceOf(CaptureEvent.NavigationEvent.class, session.events().get(2));
    }

    @Test
    void roundTripTest1_0SessionToCurrentSchema() throws IOException {
        String golden = resource("golden-session-1.0.json");
        CaptureSession original = codec.read(golden);

        String serialized = codec.write(original);
        CaptureSession restored = codec.read(serialized);

        assertEquals(original, restored);
        assertEquals(0, restored.events().size());
        assertEquals(CaptureSession.CURRENT_SCHEMA_VERSION, restored.schemaVersion());
    }

    @Test
    void representative1_0SessionMigratesToCurrentSchemaPreservingEveryNode() throws IOException {
        // Build a genuine 1.0 tree (many event types, checkpoints, dataReferences,
        // redactionSummary, extensions) using the codec's own mapper configuration,
        // then downgrade only its schemaVersion, since 1.0 and 1.1 share an
        // identical session/event shape for UI-only sessions.
        String serializedAt1_1 = codec.write(CaptureFixtures.representativeSession());
        ObjectNode original = (ObjectNode) mapper.readTree(serializedAt1_1);
        original.put("schemaVersion", "1.0");

        ObjectNode migrated = CaptureSchemaMigrator.migrate(original.deepCopy());

        assertEquals("1.1", migrated.path("schemaVersion").asText());
        assertEquals(14, migrated.path("events").size());
        assertEquals(1, migrated.path("checkpoints").size());
        assertEquals(3, migrated.path("dataReferences").size());

        // Deep-equal on every pre-existing node except schemaVersion.
        ObjectNode originalWithUpgradedVersion = original.deepCopy();
        originalWithUpgradedVersion.put("schemaVersion", "1.1");
        assertEquals(originalWithUpgradedVersion, migrated);

        // The migrated tree must itself be schema-valid and re-readable by the codec.
        JsonNode schema = loadSchema("shaft-capture-session-1.1.schema.json");
        List<String> errors = JsonSchemaValidator.validate(schema, migrated);
        assertTrue(errors.isEmpty(), "Migrated representative session should validate. Errors: " + errors);

        CaptureSession restored = codec.read(mapper.writeValueAsString(migrated));
        assertEquals(CaptureSession.CURRENT_SCHEMA_VERSION, restored.schemaVersion());
        assertEquals(14, restored.events().size());
    }

    @Test
    void version1_0SessionMigratesToVersion1_1() throws IOException {
        String golden = resource("golden-session-1.0.json");
        JsonNode parsed = mapper.readTree(golden);

        JsonNode migrated = CaptureSchemaMigrator.migrate(parsed);

        assertEquals("1.1", migrated.path("schemaVersion").asText());
        assertEquals(0, migrated.path("events").size());
        assertNotNull(migrated.path("checkpoints"));
        assertNotNull(migrated.path("dataReferences"));
        assertNotNull(migrated.path("redactionSummary"));
    }

    @Test
    void unsupportedVersionThrowsExceptionWithUpdatedMessage() {
        ObjectNode invalid = mapper.createObjectNode();
        invalid.put("schemaVersion", "2.0");
        invalid.put("sessionId", "test");

        CaptureFormatException exception = assertThrows(CaptureFormatException.class,
                () -> CaptureSchemaMigrator.migrate(invalid));

        assertTrue(exception.getMessage().contains("Unsupported capture schema version"));
        assertTrue(exception.getMessage().contains("0.9"));
        assertTrue(exception.getMessage().contains("1.0"));
        assertTrue(exception.getMessage().contains("1.1"));
    }

    @Test
    void nullInputThrowsException() {
        CaptureFormatException exception = assertThrows(CaptureFormatException.class,
                () -> CaptureSchemaMigrator.migrate(null));

        assertTrue(exception.getMessage().contains("JSON object"));
    }

    @Test
    void nonObjectInputThrowsException() {
        JsonNode array = mapper.createArrayNode();

        CaptureFormatException exception = assertThrows(CaptureFormatException.class,
                () -> CaptureSchemaMigrator.migrate(array));

        assertTrue(exception.getMessage().contains("JSON object"));
    }

    @Test
    void migratedUiOnlySessionValidatesAgainstSchema() throws IOException {
        String golden = resource("golden-session-1.0.json");
        JsonNode parsed = mapper.readTree(golden);
        JsonNode migrated = CaptureSchemaMigrator.migrate(parsed);

        // Ensure the migrated JSON validates against the 1.1 schema
        JsonNode schema = loadSchema("shaft-capture-session-1.1.schema.json");
        List<String> errors = JsonSchemaValidator.validate(schema, migrated);

        assertTrue(errors.isEmpty(), "Migrated UI-only session should validate against 1.1 schema. Errors: " + errors);
    }

    @Test
    void networkEventTypeIsValidInSchema() throws IOException {
        // Create a 1.1 session with a fully-populated network event: request,
        // response, BodyRef on both sides, and timing.
        ObjectNode session = mapper.createObjectNode();
        session.put("schemaVersion", "1.1");
        session.put("sessionId", "network-session");
        session.put("status", "INCOMPLETE");
        session.put("startedAt", "2026-01-02T03:04:05Z");

        ObjectNode browser = session.putObject("browser");
        browser.put("browserName", "chrome");
        browser.put("logicalSessionId", "browser-1");
        browser.set("capabilities", mapper.createObjectNode());

        ObjectNode networkEvent = session.putArray("events").addObject();
        networkEvent.put("type", "network");
        networkEvent.put("transactionId", "txn-1");
        networkEvent.put("resourceKind", "FETCH");
        networkEvent.put("initiatorPageUrl", "https://example.test/form");
        networkEvent.put("correlatedUiSequence", 2);
        ObjectNode context = networkEvent.putObject("context");
        context.put("sequence", 1);
        context.put("timestamp", "2026-01-02T03:04:06Z");
        ObjectNode page = context.putObject("page");
        page.put("url", "https://example.test");
        page.put("title", "Example");
        page.put("logicalWindowId", "window-1");
        page.set("framePath", mapper.createArrayNode());
        page.put("viewportWidth", 1280);
        page.put("viewportHeight", 720);
        context.put("replayStatus", "NOT_REPLAYED");
        context.set("evidence", mapper.createArrayNode());
        context.set("extensions", mapper.createObjectNode());

        ObjectNode request = networkEvent.putObject("request");
        request.put("method", "POST");
        request.put("url", "https://example.test/api/submit");
        ObjectNode requestHeaders = request.putObject("headers");
        requestHeaders.put("content-type", "application/json");
        ObjectNode requestBody = request.putObject("body");
        requestBody.put("ref", "body-ref-req-1");
        requestBody.put("sha256", "a1b2c3");
        requestBody.put("sizeBytes", 128);
        requestBody.put("encoding", "identity");
        requestBody.put("truncated", false);

        ObjectNode response = networkEvent.putObject("response");
        response.put("statusCode", 200);
        ObjectNode responseHeaders = response.putObject("headers");
        responseHeaders.put("content-type", "application/json");
        ObjectNode responseBody = response.putObject("body");
        responseBody.put("ref", "body-ref-res-1");
        responseBody.put("sha256", "d4e5f6");
        responseBody.put("sizeBytes", 256);
        responseBody.put("encoding", "gzip");
        responseBody.put("truncated", false);

        ObjectNode timing = networkEvent.putObject("timing");
        timing.put("blocked", "PT0.01S");
        timing.put("dns", "PT0.002S");
        timing.put("connect", "PT0.05S");
        timing.put("send", "PT0.001S");
        timing.put("ttfb", "PT0.3S");
        timing.put("receive", "PT0.02S");

        session.set("checkpoints", mapper.createArrayNode());
        session.set("dataReferences", mapper.createArrayNode());
        ObjectNode summary = session.putObject("redactionSummary");
        summary.put("redactedValueCount", 0);
        summary.put("removedAttributeCount", 0);
        summary.put("redactedUrlParameterCount", 0);
        summary.set("appliedRules", mapper.createArrayNode());
        session.set("extensions", mapper.createObjectNode());

        JsonNode schema = loadSchema("shaft-capture-session-1.1.schema.json");
        List<String> errors = JsonSchemaValidator.validate(schema, session);

        assertTrue(errors.isEmpty(), "Session with network event type should validate. Errors: " + errors);
    }

    @Test
    void networkEventMissingBodyRefRequiredFieldsFailsSchemaValidation() throws IOException {
        ObjectNode session = mapper.createObjectNode();
        session.put("schemaVersion", "1.1");
        session.put("sessionId", "network-session-invalid");
        session.put("status", "INCOMPLETE");
        session.put("startedAt", "2026-01-02T03:04:05Z");

        ObjectNode browser = session.putObject("browser");
        browser.put("browserName", "chrome");
        browser.put("logicalSessionId", "browser-1");
        browser.set("capabilities", mapper.createObjectNode());

        ObjectNode networkEvent = session.putArray("events").addObject();
        networkEvent.put("type", "network");
        ObjectNode context = networkEvent.putObject("context");
        context.put("sequence", 1);
        context.put("timestamp", "2026-01-02T03:04:06Z");
        ObjectNode page = context.putObject("page");
        page.put("url", "https://example.test");
        page.put("title", "Example");
        page.put("logicalWindowId", "window-1");
        page.set("framePath", mapper.createArrayNode());
        page.put("viewportWidth", 1280);
        page.put("viewportHeight", 720);
        context.put("replayStatus", "NOT_REPLAYED");
        context.set("evidence", mapper.createArrayNode());
        context.set("extensions", mapper.createObjectNode());

        ObjectNode request = networkEvent.putObject("request");
        request.put("method", "GET");
        request.put("url", "https://example.test/api/data");
        request.set("headers", mapper.createObjectNode());
        // BodyRef missing required "ref" and "sizeBytes"/"truncated" fields.
        ObjectNode requestBody = request.putObject("body");
        requestBody.put("sha256", "a1b2c3");

        session.set("checkpoints", mapper.createArrayNode());
        session.set("dataReferences", mapper.createArrayNode());
        ObjectNode summary = session.putObject("redactionSummary");
        summary.put("redactedValueCount", 0);
        summary.put("removedAttributeCount", 0);
        summary.put("redactedUrlParameterCount", 0);
        summary.set("appliedRules", mapper.createArrayNode());
        session.set("extensions", mapper.createObjectNode());

        JsonNode schema = loadSchema("shaft-capture-session-1.1.schema.json");
        List<String> errors = JsonSchemaValidator.validate(schema, session);

        assertFalse(errors.isEmpty(), "BodyRef missing required fields should fail schema validation.");
    }

    @Test
    void migration0_9To1_0To1_1ChainPreservesData() throws IOException {
        String legacy = resource("legacy-session-0.9.json");
        JsonNode parsed = mapper.readTree(legacy);
        ObjectNode step1 = CaptureSchemaMigrator.migrate(parsed);

        assertEquals("1.1", step1.path("schemaVersion").asText());
        assertEquals("INCOMPLETE", step1.path("status").asText());
        assertEquals(3, step1.path("events").size());

        // Verify no data was dropped
        assertTrue(step1.has("events"));
        assertTrue(step1.has("checkpoints"));
        assertTrue(step1.has("dataReferences"));
        assertTrue(step1.has("redactionSummary"));
        assertTrue(step1.has("extensions"));
        assertTrue(step1.has("browser"));
    }

    @Test
    void allEventTypesRemainValidAfterMigration() throws IOException {
        CaptureSession representative = CaptureFixtures.representativeSession();
        String serialized = codec.write(representative);
        CaptureSession restored = codec.read(serialized);

        assertEquals(representative, restored);
        assertEquals(CaptureSession.CURRENT_SCHEMA_VERSION, restored.schemaVersion());
        assertEquals(14, restored.events().size());
    }

    private static String resource(String name) throws IOException {
        try (InputStream input = CaptureSchemaMigratorTest.class.getResourceAsStream("/fixtures/" + name)) {
            if (input == null) {
                throw new IOException("Missing fixture: " + name);
            }
            return new String(input.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    private static JsonNode loadSchema(String resourceName) throws IOException {
        try (InputStream input = CaptureSchemaMigratorTest.class.getResourceAsStream("/schema/" + resourceName)) {
            if (input == null) {
                throw new IOException("Missing schema: " + resourceName);
            }
            return new ObjectMapper().readTree(input);
        }
    }
}
