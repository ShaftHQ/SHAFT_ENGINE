package com.shaft.capture.storage;

import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.shaft.capture.format.CaptureFormatException;
import com.shaft.capture.model.RedactionSummary;
import com.shaft.capture.privacy.ClassifiedValue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.Comparator;

/**
 * Writes ordinary captured values to deterministic external JSON while excluding secrets.
 */
public final class ExternalTestDataWriter {
    private final ObjectMapper mapper = new ObjectMapper()
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS);

    /**
     * Atomically writes persistable classified values.
     *
     * @param destination external data JSON path
     * @param values classified values
     */
    public void write(Path destination, Collection<ClassifiedValue> values) {
        if (destination == null) {
            throw new IllegalArgumentException("External data destination is required.");
        }
        ObjectNode root = mapper.createObjectNode();
        root.put("schemaVersion", "1.0");
        ObjectNode data = root.putObject("values");
        RedactionSummary summary = RedactionSummary.empty();
        if (values != null) {
            var ordered = values.stream()
                    .filter(java.util.Objects::nonNull)
                    .sorted(Comparator.comparing(value -> value.reference().id()))
                    .toList();
            for (ClassifiedValue value : ordered) {
                summary = summary.merge(value.summary());
            }
            ordered.stream()
                    .filter(ClassifiedValue::persistable)
                    .forEach(value -> data.put(value.reference().id(), value.externalizedValue()));
        }
        ObjectNode summaryNode = root.putObject("redactionSummary");
        summaryNode.put("redactedValueCount", summary.redactedValueCount());
        summaryNode.put("removedAttributeCount", summary.removedAttributeCount());
        summaryNode.put("redactedUrlParameterCount", summary.redactedUrlParameterCount());
        var rules = summaryNode.putArray("appliedRules");
        summary.appliedRules().forEach(rules::add);
        DefaultPrettyPrinter printer = new DefaultPrettyPrinter();
        DefaultIndenter indenter = new DefaultIndenter("  ", "\n");
        printer.indentArraysWith(indenter);
        printer.indentObjectsWith(indenter);
        try {
            String json = mapper.writer(printer).writeValueAsString(root) + "\n";
            atomicWrite(destination, json);
        } catch (IOException exception) {
            throw new CaptureFormatException("External capture test data could not be serialized.", exception);
        }
    }

    private static void atomicWrite(Path destination, String json) {
        Path absolute = destination.toAbsolutePath().normalize();
        Path temporary = null;
        try {
            Files.createDirectories(absolute.getParent());
            temporary = Files.createTempFile(absolute.getParent(), "." + absolute.getFileName(), ".tmp");
            Files.writeString(temporary, json, StandardCharsets.UTF_8);
            moveReplacing(temporary, absolute);
        } catch (IOException exception) {
            throw new CaptureFormatException("External capture test data could not be written atomically.",
                    exception);
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
                try {
                    Thread.sleep(10);
                } catch (InterruptedException interrupted) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while publishing external capture data.", interrupted);
                }
            }
        }
    }
}
