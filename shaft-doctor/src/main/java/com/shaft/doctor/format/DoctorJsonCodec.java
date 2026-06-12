package com.shaft.doctor.format;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.shaft.doctor.model.Diagnosis;
import com.shaft.doctor.model.EvidenceBundle;
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
import java.util.Map;

/**
 * Deterministic JSON codec for Doctor evidence, diagnosis, and reports.
 */
public final class DoctorJsonCodec {
    private static final String EVIDENCE_SCHEMA =
            "/schema/shaft-doctor-evidence-1.0.schema.json";
    private static final String DIAGNOSIS_SCHEMA =
            "/schema/shaft-doctor-diagnosis-1.0.schema.json";

    private final ObjectMapper mapper;
    private final DefaultPrettyPrinter printer;
    private final JsonNode evidenceSchema;
    private final JsonNode diagnosisSchema;

    /**
     * Creates the stable codec.
     */
    public DoctorJsonCodec() {
        mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        mapper.enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);
        mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        mapper.setSerializationInclusion(JsonInclude.Include.ALWAYS);
        printer = new DefaultPrettyPrinter();
        DefaultIndenter indenter = new DefaultIndenter("  ", "\n");
        printer.indentArraysWith(indenter);
        printer.indentObjectsWith(indenter);
        evidenceSchema = loadSchema(EVIDENCE_SCHEMA);
        diagnosisSchema = loadSchema(DIAGNOSIS_SCHEMA);
    }

    /**
     * Serializes and validates an evidence bundle.
     *
     * @param bundle bundle to serialize
     * @return stable JSON ending in a newline
     */
    public String write(EvidenceBundle bundle) {
        return writeValidated(bundle, evidenceSchema, "Evidence bundle");
    }

    /**
     * Serializes and validates a diagnosis.
     *
     * @param diagnosis diagnosis to serialize
     * @return stable JSON ending in a newline
     */
    public String write(Diagnosis diagnosis) {
        return writeValidated(diagnosis, diagnosisSchema, "Diagnosis");
    }

    /**
     * Reads and validates evidence bundle JSON.
     *
     * @param json evidence JSON
     * @return immutable bundle
     */
    public EvidenceBundle readBundle(String json) {
        try {
            JsonNode tree = mapper.readTree(json == null ? "" : json);
            validate(evidenceSchema, tree, "Evidence bundle");
            return mapper.treeToValue(tree, EvidenceBundle.class);
        } catch (DoctorFormatException exception) {
            throw exception;
        } catch (JsonProcessingException | IllegalArgumentException exception) {
            throw new DoctorFormatException("Evidence bundle is malformed, truncated, or invalid.", exception);
        }
    }

    /**
     * Reads an evidence bundle file.
     *
     * @param path bundle path
     * @return immutable bundle
     */
    public EvidenceBundle readBundle(Path path) {
        try {
            return readBundle(Files.readString(path, StandardCharsets.UTF_8));
        } catch (IOException exception) {
            throw new DoctorFormatException("Evidence bundle could not be read.", exception);
        }
    }

    /**
     * Writes a bundle atomically.
     *
     * @param path destination
     * @param bundle bundle
     */
    public void write(Path path, EvidenceBundle bundle) {
        atomicWrite(path, write(bundle));
    }

    /**
     * Writes a diagnosis atomically.
     *
     * @param path destination
     * @param diagnosis diagnosis
     */
    public void write(Path path, Diagnosis diagnosis) {
        atomicWrite(path, write(diagnosis));
    }

    /**
     * Writes the combined machine-readable report atomically.
     *
     * @param path destination
     * @param bundle evidence bundle
     * @param diagnosis diagnosis
     */
    public void writeReport(Path path, EvidenceBundle bundle, Diagnosis diagnosis) {
        try {
            JsonNode bundleTree = mapper.readTree(write(bundle));
            JsonNode diagnosisTree = mapper.readTree(write(diagnosis));
            String json = mapper.writer(printer).writeValueAsString(
                    Map.of("bundle", bundleTree, "diagnosis", diagnosisTree)) + "\n";
            atomicWrite(path, json);
        } catch (JsonProcessingException exception) {
            throw new DoctorFormatException("Doctor report could not be serialized.", exception);
        }
    }

    private String writeValidated(Object value, JsonNode schema, String label) {
        if (value == null) {
            throw new DoctorFormatException(label + " is required.");
        }
        try {
            JsonNode tree = mapper.valueToTree(value);
            validate(schema, tree, label);
            return mapper.writer(printer).writeValueAsString(tree) + "\n";
        } catch (DoctorFormatException exception) {
            throw exception;
        } catch (IllegalArgumentException | JsonProcessingException exception) {
            throw new DoctorFormatException(label + " could not be serialized.", exception);
        }
    }

    private static void validate(JsonNode schema, JsonNode tree, String label) {
        List<String> errors = JsonSchemaValidator.validate(schema, tree);
        if (!errors.isEmpty()) {
            throw new DoctorFormatException(label + " failed schema validation: "
                    + String.join(" ", errors));
        }
    }

    private JsonNode loadSchema(String resource) {
        try (InputStream input = DoctorJsonCodec.class.getResourceAsStream(resource)) {
            if (input == null) {
                throw new DoctorFormatException("Bundled Doctor JSON schema is missing.");
            }
            return mapper.readTree(input);
        } catch (IOException exception) {
            throw new DoctorFormatException("Bundled Doctor JSON schema could not be read.", exception);
        }
    }

    private static void atomicWrite(Path destination, String content) {
        Path absolute = destination.toAbsolutePath().normalize();
        Path parent = absolute.getParent();
        if (parent == null) {
            throw new DoctorFormatException("Doctor destination requires a parent directory.");
        }
        Path temporary = null;
        try {
            Files.createDirectories(parent);
            temporary = Files.createTempFile(parent, "." + absolute.getFileName(), ".tmp");
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            moveReplacing(temporary, absolute);
        } catch (IOException exception) {
            throw new DoctorFormatException("Doctor output could not be written atomically.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished temporary file.
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
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interruptedException) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing Doctor output.", interruptedException);
                }
            }
        }
    }
}
