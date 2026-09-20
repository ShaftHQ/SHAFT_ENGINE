package com.shaft.doctor.history;

import java.util.List;

/**
 * Local flake mute / quarantine models for the SHAFT Tests panel and CLI (issue #5974 / S3-08).
 *
 * <p>FR-001: mute with required reason. FR-002: never changes Maven Surefire skip by default.
 * FR-003: recover when local consecutive passes reach the configured threshold.
 * SC-002: Surefire excludes are never written unless a later child explicitly adds opt-in.
 */
public final class LocalMuteModels {
    /** Shared schema version for mute store / table responses. */
    public static final String SCHEMA_VERSION = "1.0";

    /** Default consecutive local passes required before auto-recover clears a mute. */
    public static final int DEFAULT_RECOVER_AFTER_PASSES = 3;

    /** Default gitignored store relative to the project root. */
    public static final String DEFAULT_LOCAL_STORE_RELATIVE = ".shaft/local-mutes.json";

    private LocalMuteModels() {
    }

    /** Active mute lifecycle state for one test id. */
    public enum MuteStatus {
        /** Present on the local quarantine list. */
        MUTED,
        /** Cleared after stable local history (returned in observe responses, not persisted). */
        RECOVERED
    }

    /**
     * One muted test entry.
     *
     * @param testId stable test id (class or {@code class#method})
     * @param reason required human reason for the mute
     * @param mutedAtMillis epoch millis when muted
     * @param consecutivePasses streak of local passes since last fail (or since mute)
     * @param recoverAfterPasses threshold for auto-recover
     * @param status MUTED while listed; RECOVERED only in observe responses
     */
    public record MuteEntry(
            String testId,
            String reason,
            long mutedAtMillis,
            int consecutivePasses,
            int recoverAfterPasses,
            MuteStatus status) {
        public MuteEntry {
            testId = testId == null ? "" : testId.trim();
            reason = reason == null ? "" : reason.trim();
            consecutivePasses = Math.max(0, consecutivePasses);
            recoverAfterPasses = recoverAfterPasses <= 0
                    ? DEFAULT_RECOVER_AFTER_PASSES
                    : recoverAfterPasses;
            status = status == null ? MuteStatus.MUTED : status;
        }

        /** @return true when this entry is still on the local mute list */
        public boolean muted() {
            return status == MuteStatus.MUTED;
        }
    }

    /**
     * Mute table for MCP {@code report_mute} / CLI {@code shaft report mute|unmute|mutes}.
     *
     * @param schemaVersion response schema
     * @param empty whether no muted entries remain
     * @param emptyMessage user-facing empty-state when {@code empty}
     * @param storePath resolved store path used for this operation
     * @param recoverAfterPasses default recover threshold for new mutes
     * @param writeSurefireExcludes always false in v1 (SC-002)
     * @param surefireExcludeWritten always false in v1
     * @param entries active muted rows (and optional RECOVERED observe results)
     * @param warnings non-fatal notes (e.g. Surefire opt-in refused)
     */
    public record MuteTable(
            String schemaVersion,
            boolean empty,
            String emptyMessage,
            String storePath,
            int recoverAfterPasses,
            boolean writeSurefireExcludes,
            boolean surefireExcludeWritten,
            List<MuteEntry> entries,
            List<String> warnings) {
        public MuteTable {
            schemaVersion = schemaVersion == null || schemaVersion.isBlank()
                    ? SCHEMA_VERSION
                    : schemaVersion.trim();
            emptyMessage = emptyMessage == null ? "" : emptyMessage;
            storePath = storePath == null ? "" : storePath;
            recoverAfterPasses = recoverAfterPasses <= 0
                    ? DEFAULT_RECOVER_AFTER_PASSES
                    : recoverAfterPasses;
            entries = entries == null ? List.of() : List.copyOf(entries);
            warnings = warnings == null ? List.of() : List.copyOf(warnings);
        }

        /** Empty-state table when nothing is muted. */
        public static MuteTable empty(String storePath, int recoverAfterPasses, List<String> warnings) {
            return new MuteTable(
                    SCHEMA_VERSION,
                    true,
                    "No locally muted tests. Mute from the SHAFT Tests panel or "
                            + "`shaft report mute testId=... reason=...` — Maven Surefire is unchanged.",
                    storePath,
                    recoverAfterPasses,
                    false,
                    false,
                    List.of(),
                    warnings == null ? List.of() : warnings);
        }

        /** Populated table of active mutes (or observe results). */
        public static MuteTable of(
                String storePath,
                int recoverAfterPasses,
                List<MuteEntry> entries,
                List<String> warnings) {
            List<MuteEntry> safe = entries == null ? List.of() : entries;
            if (safe.isEmpty()) {
                return empty(storePath, recoverAfterPasses, warnings);
            }
            return new MuteTable(
                    SCHEMA_VERSION,
                    false,
                    "",
                    storePath,
                    recoverAfterPasses,
                    false,
                    false,
                    safe,
                    warnings == null ? List.of() : warnings);
        }
    }
}
