package com.shaft.doctor.repair;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

final class RepairProposalStore {
    private final ObjectMapper mapper = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .enable(SerializationFeature.INDENT_OUTPUT)
            .enable(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS)
            .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

    RepairProposal read(Path path) {
        try {
            return mapper.readValue(path.toFile(), RepairProposal.class);
        } catch (IOException exception) {
            throw new IllegalArgumentException("Doctor repair proposal could not be read.", exception);
        }
    }

    void write(Path path, RepairProposal proposal) {
        Path absolute = path.toAbsolutePath().normalize();
        Path temporary = null;
        try {
            Files.createDirectories(absolute.getParent());
            temporary = Files.createTempFile(absolute.getParent(), ".doctor-repair-", ".tmp");
            Files.writeString(temporary,
                    mapper.writerWithDefaultPrettyPrinter().writeValueAsString(proposal) + "\n",
                    StandardCharsets.UTF_8);
            Files.move(temporary, absolute, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException exception) {
            throw new IllegalStateException("Doctor repair proposal could not be persisted.", exception);
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort cleanup of an unpublished manifest.
                }
            }
        }
    }
}
