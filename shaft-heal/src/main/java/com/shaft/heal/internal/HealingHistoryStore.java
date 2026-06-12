package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.LocatorFingerprint;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

final class HealingHistoryStore {
    private static final Object LOCK = new Object();
    private static final ObjectMapper JSON = new ObjectMapper();
    private final HealingConfiguration configuration;

    HealingHistoryStore(HealingConfiguration configuration) {
        this.configuration = configuration;
    }

    Optional<HistoryRecord> find(String pageKey, String originalLocator, String context) {
        if (!configuration.historyEnabled()) {
            return Optional.empty();
        }
        String key = key(pageKey, originalLocator, context);
        synchronized (LOCK) {
            return load().records().stream()
                    .filter(record -> record.key().equals(key))
                    .filter(this::valid)
                    .findFirst();
        }
    }

    void save(
            String pageKey,
            String originalLocator,
            String context,
            LocatorFingerprint fingerprint,
            String visualReference) {
        if (!configuration.historyEnabled()) {
            return;
        }
        synchronized (LOCK) {
            HistoryDocument document = load();
            Map<String, HistoryRecord> records = new LinkedHashMap<>();
            document.records().stream().filter(this::retained).filter(this::valid)
                    .forEach(record -> records.put(record.key(), record));
            HistoryRecord record = new HistoryRecord(
                    HistoryRecord.CURRENT_SCHEMA_VERSION,
                    key(pageKey, originalLocator, context),
                    pageKey,
                    originalLocator,
                    context,
                    fingerprint,
                    visualReference,
                    Instant.now().toString(),
                    "");
            records.put(record.key(), record.withChecksum(checksum(record)));
            List<HistoryRecord> retained = records.values().stream()
                    .sorted(Comparator.comparing(HistoryRecord::updatedAt).reversed())
                    .limit(configuration.historyMaxEntries())
                    .toList();
            write(new HistoryDocument(HistoryDocument.CURRENT_SCHEMA_VERSION, retained));
        }
    }

    static String key(String pageKey, String originalLocator, String context) {
        return HealingSupport.sha256(pageKey + "\n" + originalLocator + "\n" + context);
    }

    private HistoryDocument load() {
        Path path = configuration.historyPath();
        if (!Files.isRegularFile(path)) {
            return new HistoryDocument(HistoryDocument.CURRENT_SCHEMA_VERSION, List.of());
        }
        try {
            HistoryDocument document = JSON.readValue(path.toFile(), HistoryDocument.class);
            if (!HistoryDocument.CURRENT_SCHEMA_VERSION.equals(document.schemaVersion())) {
                return new HistoryDocument(HistoryDocument.CURRENT_SCHEMA_VERSION, List.of());
            }
            return document;
        } catch (IOException exception) {
            return new HistoryDocument(HistoryDocument.CURRENT_SCHEMA_VERSION, List.of());
        }
    }

    private void write(HistoryDocument document) {
        Path path = configuration.historyPath();
        Path temporary = null;
        try {
            Path parent = path.getParent();
            if (parent == null) {
                return;
            }
            Files.createDirectories(parent);
            temporary = Files.createTempFile(parent, ".shaft-heal-", ".tmp");
            Files.writeString(
                    temporary,
                    JSON.writerWithDefaultPrettyPrinter().writeValueAsString(document) + "\n",
                    StandardCharsets.UTF_8);
            try {
                Files.move(
                        temporary,
                        path,
                        StandardCopyOption.ATOMIC_MOVE,
                        StandardCopyOption.REPLACE_EXISTING);
            } catch (AtomicMoveNotSupportedException exception) {
                Files.move(temporary, path, StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (IOException ignored) {
            // Optional history must never change the action result.
        } finally {
            if (temporary != null) {
                try {
                    Files.deleteIfExists(temporary);
                } catch (IOException ignored) {
                    // Best-effort temporary file cleanup.
                }
            }
        }
    }

    private boolean valid(HistoryRecord record) {
        return HistoryRecord.CURRENT_SCHEMA_VERSION.equals(record.schemaVersion())
                && record.checksum().equals(checksum(record));
    }

    private boolean retained(HistoryRecord record) {
        try {
            Instant cutoff = Instant.now().minus(configuration.historyRetention());
            return Instant.parse(record.updatedAt()).isAfter(cutoff);
        } catch (RuntimeException exception) {
            return false;
        }
    }

    private static String checksum(HistoryRecord record) {
        try {
            Map<String, Object> content = new LinkedHashMap<>();
            content.put("schemaVersion", record.schemaVersion());
            content.put("key", record.key());
            content.put("pageKey", record.pageKey());
            content.put("originalLocator", record.originalLocator());
            content.put("context", record.context());
            content.put("fingerprint", record.fingerprint());
            content.put("visualReference", record.visualReference());
            content.put("updatedAt", record.updatedAt());
            return HealingSupport.sha256(JSON.writeValueAsString(content));
        } catch (IOException exception) {
            return "";
        }
    }
}
