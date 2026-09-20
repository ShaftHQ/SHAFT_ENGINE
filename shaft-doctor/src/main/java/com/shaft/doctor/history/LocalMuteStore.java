package com.shaft.doctor.history;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.ArrayNode;
import tools.jackson.databind.node.ObjectNode;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

/**
 * Persists a local flake mute / quarantine list (issue #5974 / S3-08).
 *
 * <p>Default store is {@link LocalMuteModels#DEFAULT_LOCAL_STORE_RELATIVE} (gitignored). Teams may
 * pass an explicit project path to share mutes. Recover clears a mute after
 * {@code recoverAfterPasses} consecutive local passes. Never writes Maven Surefire excludes in v1
 * (SC-002 / FR-002).
 */
public final class LocalMuteStore {
    private static final ObjectMapper JSON = JsonMapper.builder().build();

    private final Path storePath;
    private final int defaultRecoverAfterPasses;
    private final Map<String, LocalMuteModels.MuteEntry> entriesByTestId = new LinkedHashMap<>();

    private LocalMuteStore(Path storePath, int defaultRecoverAfterPasses) {
        this.storePath = Objects.requireNonNull(storePath, "storePath");
        this.defaultRecoverAfterPasses = defaultRecoverAfterPasses <= 0
                ? LocalMuteModels.DEFAULT_RECOVER_AFTER_PASSES
                : defaultRecoverAfterPasses;
    }

    /** Loads (or creates an empty) mute store at the given path. */
    public static LocalMuteStore open(Path storePath) {
        return open(storePath, LocalMuteModels.DEFAULT_RECOVER_AFTER_PASSES);
    }

    /** Loads (or creates an empty) mute store with a custom default recover threshold. */
    public static LocalMuteStore open(Path storePath, int defaultRecoverAfterPasses) {
        LocalMuteStore store = new LocalMuteStore(storePath, defaultRecoverAfterPasses);
        store.loadQuietly();
        return store;
    }

    /** Resolves the default local (gitignored) store under a project root. */
    public static Path defaultStorePath(Path projectRoot) {
        Path root = projectRoot == null ? Path.of(".") : projectRoot;
        return root.resolve(LocalMuteModels.DEFAULT_LOCAL_STORE_RELATIVE).normalize();
    }

    /** @return absolute store path string */
    public String storePathString() {
        return storePath.toAbsolutePath().normalize().toString();
    }

    /** @return default recover-after-passes for new mutes */
    public int defaultRecoverAfterPasses() {
        return defaultRecoverAfterPasses;
    }

    /**
     * Mutes a test. Reason is required (FR-001). Does not touch Surefire (FR-002 / SC-002).
     *
     * @param testId test id to mute
     * @param reason non-blank mute reason
     * @param recoverAfterPasses optional override; {@code null}/≤0 uses store default
     * @param writeSurefireExcludes ignored in v1 — always refused
     * @return updated mute table
     */
    public LocalMuteModels.MuteTable mute(
            String testId,
            String reason,
            Integer recoverAfterPasses,
            Boolean writeSurefireExcludes) {
        String id = requireTestId(testId);
        String why = reason == null ? "" : reason.trim();
        if (why.isBlank()) {
            throw new IllegalArgumentException("Mute reason is required (FR-001).");
        }
        List<String> warnings = surefireWarnings(writeSurefireExcludes);
        int recover = recoverAfterPasses == null || recoverAfterPasses <= 0
                ? defaultRecoverAfterPasses
                : recoverAfterPasses;
        long now = System.currentTimeMillis();
        LocalMuteModels.MuteEntry existing = entriesByTestId.get(id);
        LocalMuteModels.MuteEntry entry = new LocalMuteModels.MuteEntry(
                id,
                why,
                existing == null ? now : existing.mutedAtMillis(),
                existing == null ? 0 : existing.consecutivePasses(),
                recover,
                LocalMuteModels.MuteStatus.MUTED);
        entriesByTestId.put(id, entry);
        persist();
        return table(warnings);
    }

    /** Removes a mute for {@code testId} when present. */
    public LocalMuteModels.MuteTable unmute(String testId) {
        entriesByTestId.remove(requireTestId(testId));
        persist();
        return table(List.of());
    }

    /** Lists active muted entries. */
    public LocalMuteModels.MuteTable list() {
        return table(List.of());
    }

    /**
     * Feeds a local run outcome for a muted test (FR-003). Passes increment the streak; fails
     * reset it. When the streak reaches {@code recoverAfterPasses}, the mute is cleared.
     */
    public LocalMuteModels.MuteTable observe(String testId, boolean passed) {
        String id = requireTestId(testId);
        LocalMuteModels.MuteEntry current = entriesByTestId.get(id);
        if (current == null || !current.muted()) {
            return table(List.of());
        }
        if (!passed) {
            entriesByTestId.put(id, withPasses(current, 0));
            persist();
            return table(List.of());
        }
        int next = current.consecutivePasses() + 1;
        if (next >= current.recoverAfterPasses()) {
            entriesByTestId.remove(id);
            persist();
            LocalMuteModels.MuteEntry recovered = new LocalMuteModels.MuteEntry(
                    current.testId(),
                    current.reason(),
                    current.mutedAtMillis(),
                    next,
                    current.recoverAfterPasses(),
                    LocalMuteModels.MuteStatus.RECOVERED);
            List<LocalMuteModels.MuteEntry> rows = new ArrayList<>(activeEntries());
            rows.add(0, recovered);
            return LocalMuteModels.MuteTable.of(
                    storePathString(), defaultRecoverAfterPasses, rows, List.of());
        }
        entriesByTestId.put(id, withPasses(current, next));
        persist();
        return table(List.of());
    }

    /** @return true when the test is currently muted */
    public boolean isMuted(String testId) {
        if (testId == null || testId.isBlank()) {
            return false;
        }
        LocalMuteModels.MuteEntry entry = entriesByTestId.get(testId.trim());
        return entry != null && entry.muted();
    }

    /** @return active mute entry, or {@code null} */
    public LocalMuteModels.MuteEntry get(String testId) {
        if (testId == null || testId.isBlank()) {
            return null;
        }
        return entriesByTestId.get(testId.trim());
    }

    /**
     * SC-002 guard: v1 never writes Surefire excludes.
     *
     * @return always {@code false}
     */
    public boolean writeSurefireExcludesIfOptedIn(Path projectRoot) {
        return false;
    }

    private LocalMuteModels.MuteTable table(List<String> warnings) {
        return LocalMuteModels.MuteTable.of(
                storePathString(), defaultRecoverAfterPasses, activeEntries(), warnings);
    }

    private List<LocalMuteModels.MuteEntry> activeEntries() {
        return entriesByTestId.values().stream()
                .filter(LocalMuteModels.MuteEntry::muted)
                .sorted(Comparator.comparing(LocalMuteModels.MuteEntry::testId))
                .toList();
    }

    private static LocalMuteModels.MuteEntry withPasses(LocalMuteModels.MuteEntry entry, int passes) {
        return new LocalMuteModels.MuteEntry(
                entry.testId(),
                entry.reason(),
                entry.mutedAtMillis(),
                passes,
                entry.recoverAfterPasses(),
                LocalMuteModels.MuteStatus.MUTED);
    }

    private static String requireTestId(String testId) {
        if (testId == null || testId.isBlank()) {
            throw new IllegalArgumentException("testId is required.");
        }
        return testId.trim();
    }

    private static List<String> surefireWarnings(Boolean writeSurefireExcludes) {
        if (writeSurefireExcludes == null || !writeSurefireExcludes) {
            return List.of();
        }
        return List.of(
                "writeSurefireExcludes requested but refused: Maven Surefire excludes are not "
                        + "written by default (FR-002 / SC-002). A later child issue may add opt-in.");
    }

    private void loadQuietly() {
        if (!Files.isRegularFile(storePath)) {
            return;
        }
        try {
            JsonNode root = JSON.readTree(Files.readString(storePath, StandardCharsets.UTF_8));
            if (root == null || !root.isObject()) {
                return;
            }
            JsonNode entries = root.get("entries");
            if (entries == null || !entries.isArray()) {
                return;
            }
            for (JsonNode node : entries) {
                LocalMuteModels.MuteEntry entry = readEntry(node);
                if (entry != null && entry.muted() && !entry.testId().isBlank()) {
                    entriesByTestId.put(entry.testId(), entry);
                }
            }
        } catch (IOException | RuntimeException ignored) {
            // Corrupt store → start empty; next persist rewrites.
        }
    }

    private LocalMuteModels.MuteEntry readEntry(JsonNode node) {
        if (node == null || !node.isObject()) {
            return null;
        }
        LocalMuteModels.MuteStatus status = parseStatus(text(node, "status"));
        if (status != LocalMuteModels.MuteStatus.MUTED) {
            return null;
        }
        return new LocalMuteModels.MuteEntry(
                text(node, "testId"),
                text(node, "reason"),
                node.path("mutedAtMillis").asLong(0L),
                node.path("consecutivePasses").asInt(0),
                node.path("recoverAfterPasses").asInt(defaultRecoverAfterPasses),
                status);
    }

    private static LocalMuteModels.MuteStatus parseStatus(String raw) {
        if (raw == null || raw.isBlank()) {
            return LocalMuteModels.MuteStatus.MUTED;
        }
        try {
            return LocalMuteModels.MuteStatus.valueOf(raw.trim().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException ex) {
            return LocalMuteModels.MuteStatus.MUTED;
        }
    }

    private void persist() {
        try {
            Path parent = storePath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            ObjectNode root = JSON.createObjectNode();
            root.put("schemaVersion", LocalMuteModels.SCHEMA_VERSION);
            root.put("recoverAfterPasses", defaultRecoverAfterPasses);
            root.put("writeSurefireExcludes", false);
            ArrayNode array = root.putArray("entries");
            for (LocalMuteModels.MuteEntry entry : activeEntries()) {
                ObjectNode row = array.addObject();
                row.put("testId", entry.testId());
                row.put("reason", entry.reason());
                row.put("mutedAtMillis", entry.mutedAtMillis());
                row.put("consecutivePasses", entry.consecutivePasses());
                row.put("recoverAfterPasses", entry.recoverAfterPasses());
                row.put("status", entry.status().name());
            }
            Files.writeString(
                    storePath,
                    JSON.writerWithDefaultPrettyPrinter().writeValueAsString(root) + "\n",
                    StandardCharsets.UTF_8);
        } catch (IOException ex) {
            throw new IllegalStateException("Failed to persist local mute store: " + storePath, ex);
        }
    }

    private static String text(JsonNode node, String field) {
        JsonNode value = node.get(field);
        return value == null || value.isNull() ? "" : value.asText("");
    }
}
