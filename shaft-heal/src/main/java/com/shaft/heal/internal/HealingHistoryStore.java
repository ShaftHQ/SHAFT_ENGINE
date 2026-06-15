package com.shaft.heal.internal;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.shaft.heal.HealingConfiguration;
import com.shaft.heal.model.HealingContext;
import com.shaft.heal.model.HealingPlatform;
import com.shaft.heal.model.LocatorFingerprint;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.channels.FileChannel;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

final class HealingHistoryStore {
    private static final Object LOCK = new Object();
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final ObjectMapper CHECKSUM_JSON = new ObjectMapper()
            .configure(SerializationFeature.ORDER_MAP_ENTRIES_BY_KEYS, true);
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
            return withFileLock(() -> load().records().stream()
                    .filter(record -> record.key().equals(key))
                    .filter(this::valid)
                    .findFirst());
        }
    }

    void save(
            String pageKey,
            String originalLocator,
            String context,
            LocatorFingerprint fingerprint,
            String visualReference) {
        save(pageKey, originalLocator, context, null, fingerprint, visualReference);
    }

    void save(
            String pageKey,
            String originalLocator,
            String context,
            HealingContext contextMetadata,
            LocatorFingerprint fingerprint,
            String visualReference) {
        if (!configuration.historyEnabled()) {
            return;
        }
        synchronized (LOCK) {
            withFileLock(() -> {
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
                        contextMetadata,
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
                return null;
            });
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
            if (HistoryDocument.CURRENT_SCHEMA_VERSION.equals(document.schemaVersion())) {
                return document;
            }
            if ("1.0".equals(document.schemaVersion())) {
                HistoryDocument migrated = migrate(document);
                write(migrated);
                return migrated;
            }
            quarantine(path);
            return empty();
        } catch (IOException exception) {
            quarantine(path);
            return empty();
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
            content.put("contextMetadata", record.contextMetadata());
            content.put("fingerprint", record.fingerprint());
            content.put("visualReference", record.visualReference());
            content.put("updatedAt", record.updatedAt());
            return HealingSupport.sha256(CHECKSUM_JSON.writeValueAsString(content));
        } catch (IOException exception) {
            return "";
        }
    }

    private HistoryDocument migrate(HistoryDocument legacy) {
        List<HistoryRecord> records = legacy.records().stream()
                .filter(this::validLegacy)
                .map(this::migrate)
                .sorted(Comparator.comparing(HistoryRecord::updatedAt).reversed())
                .limit(configuration.historyMaxEntries())
                .toList();
        return new HistoryDocument(HistoryDocument.CURRENT_SCHEMA_VERSION, records);
    }

    private HistoryRecord migrate(HistoryRecord legacy) {
        LocatorFingerprint old = legacy.fingerprint();
        LocatorFingerprint fingerprint = new LocatorFingerprint(
                LocatorFingerprint.CURRENT_SCHEMA_VERSION,
                old.tagName(),
                old.accessibleName(),
                old.associatedLabel(),
                old.visibleText(),
                old.id(),
                old.name(),
                old.role(),
                old.type(),
                old.placeholder(),
                old.title(),
                old.testIds(),
                old.semanticAttributes(),
                old.domPathChecksum(),
                HealingPlatform.WEB,
                Map.of(),
                old.bounds(),
                true,
                true,
                old.selected(),
                "");
        HealingContext contextMetadata = new HealingContext(
                HealingContext.CURRENT_SCHEMA_VERSION,
                HealingPlatform.WEB,
                "",
                "",
                "",
                "",
                contextValue(legacy.context(), "frame"),
                contextValue(legacy.context(), "shadowHost"),
                contextValue(legacy.context(), "shadowContent"));
        HistoryRecord migrated = new HistoryRecord(
                HistoryRecord.CURRENT_SCHEMA_VERSION,
                legacy.key(),
                legacy.pageKey(),
                legacy.originalLocator(),
                legacy.context(),
                contextMetadata,
                fingerprint,
                legacy.visualReference(),
                legacy.updatedAt(),
                "");
        return migrated.withChecksum(checksum(migrated));
    }

    private boolean validLegacy(HistoryRecord record) {
        return "1.0".equals(record.schemaVersion())
                && record.checksum().equals(legacyChecksum(record));
    }

    private static String legacyChecksum(HistoryRecord record) {
        try {
            LocatorFingerprint fingerprint = record.fingerprint();
            Map<String, Object> legacyFingerprint = new LinkedHashMap<>();
            legacyFingerprint.put("schemaVersion", fingerprint.schemaVersion());
            legacyFingerprint.put("tagName", fingerprint.tagName());
            legacyFingerprint.put("accessibleName", fingerprint.accessibleName());
            legacyFingerprint.put("associatedLabel", fingerprint.associatedLabel());
            legacyFingerprint.put("visibleText", fingerprint.visibleText());
            legacyFingerprint.put("id", fingerprint.id());
            legacyFingerprint.put("name", fingerprint.name());
            legacyFingerprint.put("role", fingerprint.role());
            legacyFingerprint.put("type", fingerprint.type());
            legacyFingerprint.put("placeholder", fingerprint.placeholder());
            legacyFingerprint.put("title", fingerprint.title());
            legacyFingerprint.put("testIds", fingerprint.testIds());
            legacyFingerprint.put("semanticAttributes", fingerprint.semanticAttributes());
            legacyFingerprint.put("domPathChecksum", fingerprint.domPathChecksum());
            Map<String, Object> content = new LinkedHashMap<>();
            content.put("schemaVersion", record.schemaVersion());
            content.put("key", record.key());
            content.put("pageKey", record.pageKey());
            content.put("originalLocator", record.originalLocator());
            content.put("context", record.context());
            content.put("fingerprint", legacyFingerprint);
            content.put("visualReference", record.visualReference());
            content.put("updatedAt", record.updatedAt());
            return HealingSupport.sha256(JSON.writeValueAsString(content));
        } catch (IOException exception) {
            return "";
        }
    }

    private <T> T withFileLock(Supplier<T> action) {
        Path path = configuration.historyPath();
        Path parent = path.getParent();
        if (parent == null) {
            return action.get();
        }
        try {
            Files.createDirectories(parent);
            Path lockPath = parent.resolve(path.getFileName() + ".lock");
            try (FileChannel channel = FileChannel.open(
                    lockPath,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.WRITE);
                 var ignored = channel.lock()) {
                return action.get();
            }
        } catch (IOException exception) {
            return action.get();
        }
    }

    private static HistoryDocument empty() {
        return new HistoryDocument(HistoryDocument.CURRENT_SCHEMA_VERSION, List.of());
    }

    private static String contextValue(String context, String name) {
        if (context == null || context.isBlank()) {
            return "";
        }
        String prefix = name + "=";
        for (String item : context.split(";")) {
            if (item.startsWith(prefix)) {
                return item.substring(prefix.length());
            }
        }
        return "";
    }

    private static void quarantine(Path path) {
        try {
            Path parent = path.getParent();
            if (parent == null || !Files.exists(path)) {
                return;
            }
            Path target = parent.resolve(path.getFileName() + ".corrupt-" + System.currentTimeMillis());
            Files.move(path, target, StandardCopyOption.REPLACE_EXISTING);
            try (var candidates = Files.list(parent)) {
                List<Path> old = candidates
                        .filter(value -> value.getFileName().toString()
                                .startsWith(path.getFileName() + ".corrupt-"))
                        .sorted(Comparator.comparingLong(HealingHistoryStore::lastModified).reversed())
                        .skip(3)
                        .toList();
                for (Path candidate : old) {
                    Files.deleteIfExists(candidate);
                }
            }
        } catch (IOException ignored) {
            // Corrupt optional history is ignored when quarantine is unavailable.
        }
    }

    private static long lastModified(Path path) {
        try {
            return Files.getLastModifiedTime(path).toMillis();
        } catch (IOException exception) {
            return 0;
        }
    }
}
