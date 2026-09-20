package com.shaft.intellij.testindex;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * IntelliJ-side local flake mute store (issue #5974 / S3-08).
 *
 * <p>Same JSON schema as {@code com.shaft.doctor.history.LocalMuteStore}: default path
 * {@code .shaft/local-mutes.json} (gitignored). Mute requires a reason. Recover clears after N
 * consecutive local passes observed via {@link ShaftTestIndex}. Never writes Maven Surefire
 * excludes (SC-002).
 */
public final class LocalFlakeMuteStore {
    public static final String DEFAULT_RELATIVE = ".shaft/local-mutes.json";
    public static final int DEFAULT_RECOVER_AFTER_PASSES = 3;

    private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();

    /** One muted row for panel decoration. */
    public record MuteEntry(
            String testId,
            String reason,
            long mutedAtMillis,
            int consecutivePasses,
            int recoverAfterPasses) {
    }

    private final Path storePath;
    private final int defaultRecoverAfterPasses;
    private final Map<String, MuteEntry> entriesByTestId = new LinkedHashMap<>();

    public LocalFlakeMuteStore(Path storePath) {
        this(storePath, DEFAULT_RECOVER_AFTER_PASSES);
    }

    public LocalFlakeMuteStore(Path storePath, int defaultRecoverAfterPasses) {
        this.storePath = Objects.requireNonNull(storePath, "storePath");
        this.defaultRecoverAfterPasses = defaultRecoverAfterPasses <= 0
                ? DEFAULT_RECOVER_AFTER_PASSES
                : defaultRecoverAfterPasses;
        loadQuietly();
    }

    /** Opens the default project-local mute store, or an empty in-memory store when unbound. */
    public static LocalFlakeMuteStore forProject(@Nullable Project project) {
        if (project == null || project.getBasePath() == null) {
            return new LocalFlakeMuteStore(Path.of(System.getProperty("java.io.tmpdir"), "shaft-local-mutes.json"));
        }
        return new LocalFlakeMuteStore(Path.of(project.getBasePath()).resolve(DEFAULT_RELATIVE));
    }

    public boolean isMuted(@Nullable String testId) {
        return testId != null && !testId.isBlank() && entriesByTestId.containsKey(testId.trim());
    }

    public @Nullable MuteEntry get(@Nullable String testId) {
        if (testId == null || testId.isBlank()) {
            return null;
        }
        return entriesByTestId.get(testId.trim());
    }

    public List<MuteEntry> list() {
        return entriesByTestId.values().stream()
                .sorted(Comparator.comparing(MuteEntry::testId))
                .toList();
    }

    /**
     * Mutes {@code testId}. Reason must be non-blank (FR-001).
     *
     * @throws IllegalArgumentException when reason is blank
     */
    public void mute(String testId, String reason, int recoverAfterPasses) {
        String id = requireTestId(testId);
        String why = reason == null ? "" : reason.trim();
        if (why.isBlank()) {
            throw new IllegalArgumentException("Mute reason is required.");
        }
        int recover = recoverAfterPasses <= 0 ? defaultRecoverAfterPasses : recoverAfterPasses;
        MuteEntry existing = entriesByTestId.get(id);
        long mutedAt = existing == null ? System.currentTimeMillis() : existing.mutedAtMillis();
        int passes = existing == null ? 0 : existing.consecutivePasses();
        entriesByTestId.put(id, new MuteEntry(id, why, mutedAt, passes, recover));
        persist();
    }

    public void unmute(String testId) {
        entriesByTestId.remove(requireTestId(testId));
        persist();
    }

    /**
     * Observes a local run outcome from {@link ShaftTestIndex}. Returns {@code true} when the mute
     * was cleared by recovery.
     */
    public boolean observe(String testId, boolean passed) {
        String id = requireTestId(testId);
        MuteEntry current = entriesByTestId.get(id);
        if (current == null) {
            return false;
        }
        if (!passed) {
            entriesByTestId.put(id, new MuteEntry(
                    current.testId(), current.reason(), current.mutedAtMillis(), 0, current.recoverAfterPasses()));
            persist();
            return false;
        }
        int next = current.consecutivePasses() + 1;
        if (next >= current.recoverAfterPasses()) {
            entriesByTestId.remove(id);
            persist();
            return true;
        }
        entriesByTestId.put(id, new MuteEntry(
                current.testId(), current.reason(), current.mutedAtMillis(), next, current.recoverAfterPasses()));
        persist();
        return false;
    }

    private static String requireTestId(String testId) {
        if (testId == null || testId.isBlank()) {
            throw new IllegalArgumentException("testId is required.");
        }
        return testId.trim();
    }

    private void loadQuietly() {
        if (!Files.isRegularFile(storePath)) {
            return;
        }
        try {
            JsonObject root = JsonParser.parseString(Files.readString(storePath, StandardCharsets.UTF_8))
                    .getAsJsonObject();
            JsonArray entries = root.has("entries") && root.get("entries").isJsonArray()
                    ? root.getAsJsonArray("entries")
                    : null;
            if (entries == null) {
                return;
            }
            for (JsonElement element : entries) {
                MuteEntry entry = readMutedEntry(element);
                if (entry != null) {
                    entriesByTestId.put(entry.testId(), entry);
                }
            }
        } catch (IOException | RuntimeException ignored) {
            // Corrupt store → start empty.
        }
    }

    private MuteEntry readMutedEntry(JsonElement element) {
        if (element == null || !element.isJsonObject()) {
            return null;
        }
        JsonObject node = element.getAsJsonObject();
        String status = text(node, "status");
        if (!status.isBlank() && !"MUTED".equalsIgnoreCase(status)) {
            return null;
        }
        String testId = text(node, "testId");
        if (testId.isBlank()) {
            return null;
        }
        int recover = intVal(node, "recoverAfterPasses");
        return new MuteEntry(
                testId,
                text(node, "reason"),
                longVal(node, "mutedAtMillis"),
                intVal(node, "consecutivePasses"),
                recover <= 0 ? defaultRecoverAfterPasses : recover);
    }

    private void persist() {
        try {
            Path parent = storePath.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            JsonObject root = new JsonObject();
            root.addProperty("schemaVersion", "1.0");
            root.addProperty("recoverAfterPasses", defaultRecoverAfterPasses);
            root.addProperty("writeSurefireExcludes", false);
            JsonArray array = new JsonArray();
            for (MuteEntry entry : list()) {
                JsonObject row = new JsonObject();
                row.addProperty("testId", entry.testId());
                row.addProperty("reason", entry.reason());
                row.addProperty("mutedAtMillis", entry.mutedAtMillis());
                row.addProperty("consecutivePasses", entry.consecutivePasses());
                row.addProperty("recoverAfterPasses", entry.recoverAfterPasses());
                row.addProperty("status", "MUTED");
                array.add(row);
            }
            root.add("entries", array);
            Files.writeString(storePath, GSON.toJson(root) + "\n", StandardCharsets.UTF_8);
        } catch (IOException ex) {
            throw new IllegalStateException("Failed to persist local mute store: " + storePath, ex);
        }
    }

    private static String text(JsonObject node, String field) {
        return node.has(field) && !node.get(field).isJsonNull() ? node.get(field).getAsString() : "";
    }

    private static long longVal(JsonObject node, String field) {
        return node.has(field) && node.get(field).isJsonPrimitive() ? node.get(field).getAsLong() : 0L;
    }

    private static int intVal(JsonObject node, String field) {
        return node.has(field) && node.get(field).isJsonPrimitive() ? node.get(field).getAsInt() : 0;
    }
}
