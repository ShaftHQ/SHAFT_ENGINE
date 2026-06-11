package com.shaft.capture.format;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.shaft.capture.model.CaptureSession;
import com.shaft.pilot.json.JsonSchemaValidator;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;

/**
 * Stable human-readable JSON codec with schema validation and migration.
 */
public final class CaptureJsonCodec {
    private static final String SCHEMA_RESOURCE = "/schema/shaft-capture-session-1.0.schema.json";
    private final ObjectMapper mapper;
    private final JsonNode schema;
    private final DefaultPrettyPrinter printer;

    /**
     * Creates the default deterministic codec.
     */
    public CaptureJsonCodec() {
        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        mapper.disable(SerializationFeature.WRITE_DURATIONS_AS_TIMESTAMPS);
        mapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
        mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        printer = new DefaultPrettyPrinter();
        DefaultIndenter indenter = new DefaultIndenter("  ", "\n");
        printer.indentArraysWith(indenter);
        printer.indentObjectsWith(indenter);
        schema = loadSchema();
    }

    /**
     * Serializes and validates a current capture session.
     *
     * @param session session to serialize
     * @return stable JSON ending with a newline
     */
    public String write(CaptureSession session) {
        if (session == null) {
            throw new CaptureFormatException("Capture session is required.");
        }
        if (!CaptureSession.CURRENT_SCHEMA_VERSION.equals(session.schemaVersion())) {
            throw new CaptureFormatException("Only current capture schema version "
                    + CaptureSession.CURRENT_SCHEMA_VERSION + " can be written.");
        }
        try {
            JsonNode tree = mapper.valueToTree(session);
            validate(tree);
            return mapper.writer(printer).writeValueAsString(tree) + "\n";
        } catch (IllegalArgumentException | JsonProcessingException exception) {
            if (exception instanceof CaptureFormatException captureFormatException) {
                throw captureFormatException;
            }
            throw new CaptureFormatException("Capture session could not be serialized.", exception);
        }
    }

    /**
     * Reads, migrates, validates, and deserializes capture JSON.
     *
     * @param json capture JSON
     * @return current immutable session
     */
    public CaptureSession read(String json) {
        try {
            JsonNode parsed = mapper.readTree(json == null ? "" : json);
            JsonNode migrated = CaptureSchemaMigrator.migrate(parsed);
            validate(migrated);
            return mapper.treeToValue(migrated, CaptureSession.class);
        } catch (CaptureFormatException exception) {
            throw exception;
        } catch (JsonProcessingException exception) {
            throw new CaptureFormatException("Capture JSON is malformed, truncated, or semantically invalid.",
                    exception);
        } catch (IllegalArgumentException exception) {
            throw new CaptureFormatException("Capture JSON is semantically invalid.", exception);
        }
    }

    /**
     * Reads a capture file.
     *
     * @param path capture path
     * @return current immutable session
     */
    public CaptureSession read(Path path) {
        try {
            return read(Files.readString(path, StandardCharsets.UTF_8));
        } catch (IOException exception) {
            throw new CaptureFormatException("Capture file could not be read.", exception);
        }
    }

    /**
     * Validates and atomically writes a capture file.
     *
     * @param path destination path
     * @param session session to write
     */
    public void write(Path path, CaptureSession session) {
        String json = write(session);
        atomicWrite(path, json);
    }

    /**
     * Validates a current-schema JSON tree.
     *
     * @param tree capture tree
     */
    public void validate(JsonNode tree) {
        List<String> errors = JsonSchemaValidator.validate(schema, tree);
        if (!errors.isEmpty()) {
            throw new CaptureFormatException("Capture JSON failed schema validation: " + String.join(" ", errors));
        }
    }

    private JsonNode loadSchema() {
        try (InputStream input = CaptureJsonCodec.class.getResourceAsStream(SCHEMA_RESOURCE)) {
            if (input == null) {
                throw new CaptureFormatException("Bundled capture JSON schema is missing.");
            }
            return mapper.readTree(input);
        } catch (IOException exception) {
            throw new CaptureFormatException("Bundled capture JSON schema could not be read.", exception);
        }
    }

    private static void atomicWrite(Path destination, String content) {
        Path absolute = destination.toAbsolutePath().normalize();
        Path parent = absolute.getParent();
        if (parent == null) {
            throw new CaptureFormatException("Capture destination requires a parent directory.");
        }
        Path temporary = null;
        try {
            Files.createDirectories(parent);
            temporary = Files.createTempFile(parent, "." + absolute.getFileName(), ".tmp");
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            moveReplacing(temporary, absolute);
        } catch (IOException exception) {
            throw new CaptureFormatException("Capture file could not be written atomically.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished temporary artifact.
                }
            }
        }
    }

    private static void moveReplacing(Path temporary, Path destination) throws IOException {
        boolean atomic = true;
        for (int attempt = 1; attempt <= 50; attempt++) {
            try {
                if (atomic) {
                    Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE,
                            StandardCopyOption.REPLACE_EXISTING);
                } else {
                    Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING);
                }
                return;
            } catch (AtomicMoveNotSupportedException ignored) {
                atomic = false;
            } catch (AccessDeniedException exception) {
                if (attempt == 50) {
                    throw exception;
                }
                sleepBeforeMoveRetry();
            }
        }
    }

    private static void sleepBeforeMoveRetry() throws IOException {
        try {
            Thread.sleep(10);
        } catch (InterruptedException exception) {
            Thread.currentThread().interrupt();
            throw new IOException("Interrupted while publishing capture file.", exception);
        }
    }
}
