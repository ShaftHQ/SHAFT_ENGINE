package com.shaft.doctor.history;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Local mute / recover lifecycle (issue #5974 / S3-08). */
class LocalMuteStoreTest {

    @TempDir
    Path temp;

    @Test
    void muteRequiresReasonAndPersistsWithoutSurefireExclude() throws Exception {
        Path storeFile = temp.resolve(".shaft/local-mutes.json");
        LocalMuteStore store = LocalMuteStore.open(storeFile, 3);

        LocalMuteModels.MuteTable table = store.mute(
                "com.example.FlakyTest#testFlip", "timeout flake on agent disk", null, false);

        assertFalse(table.empty());
        assertEquals(1, table.entries().size());
        assertEquals("com.example.FlakyTest#testFlip", table.entries().get(0).testId());
        assertEquals("timeout flake on agent disk", table.entries().get(0).reason());
        assertFalse(table.writeSurefireExcludes());
        assertFalse(table.surefireExcludeWritten());
        assertTrue(Files.isRegularFile(storeFile));
        String raw = Files.readString(storeFile);
        assertTrue(raw.contains("timeout flake on agent disk"));
        assertFalse(raw.contains("<exclude>"));
        assertFalse(store.writeSurefireExcludesIfOptedIn(temp));
        assertTrue(Files.notExists(temp.resolve("pom.xml")));
    }

    @Test
    void muteRejectsBlankReason() {
        LocalMuteStore store = LocalMuteStore.open(temp.resolve("mutes.json"));
        assertThrows(IllegalArgumentException.class,
                () -> store.mute("com.example.T#m", "  ", null, false));
    }

    @Test
    void observeRecoversAfterConfiguredConsecutivePasses() {
        LocalMuteStore store = LocalMuteStore.open(temp.resolve("mutes.json"), 3);
        store.mute("FlakyTest", "noise", 3, false);

        store.observe("FlakyTest", true);
        store.observe("FlakyTest", true);
        assertTrue(store.isMuted("FlakyTest"));

        LocalMuteModels.MuteTable recovered = store.observe("FlakyTest", true);
        assertFalse(store.isMuted("FlakyTest"));
        assertTrue(recovered.entries().stream()
                .anyMatch(e -> e.status() == LocalMuteModels.MuteStatus.RECOVERED
                        && "FlakyTest".equals(e.testId())));
        assertTrue(store.list().empty());
    }

    @Test
    void failResetsConsecutivePassStreak() {
        LocalMuteStore store = LocalMuteStore.open(temp.resolve("mutes.json"), 3);
        store.mute("FlakyTest", "noise", null, false);
        store.observe("FlakyTest", true);
        store.observe("FlakyTest", true);
        store.observe("FlakyTest", false);
        assertEquals(0, store.get("FlakyTest").consecutivePasses());
        store.observe("FlakyTest", true);
        store.observe("FlakyTest", true);
        assertTrue(store.isMuted("FlakyTest"));
        store.observe("FlakyTest", true);
        assertFalse(store.isMuted("FlakyTest"));
    }

    @Test
    void writeSurefireExcludesFlagIsRefusedWithWarning() {
        LocalMuteStore store = LocalMuteStore.open(temp.resolve("mutes.json"));
        LocalMuteModels.MuteTable table = store.mute("T#m", "reason", null, true);
        assertFalse(table.surefireExcludeWritten());
        assertFalse(table.writeSurefireExcludes());
        assertTrue(table.warnings().stream().anyMatch(w -> w.contains("Surefire")));
        assertFalse(store.writeSurefireExcludesIfOptedIn(temp));
    }

    @Test
    void unmuteRemovesEntry() {
        LocalMuteStore store = LocalMuteStore.open(temp.resolve("mutes.json"));
        store.mute("T#m", "reason", null, false);
        assertTrue(store.unmute("T#m").empty());
        assertFalse(store.isMuted("T#m"));
    }

    @Test
    void defaultStorePathUsesGitignoredRelativeLocation() {
        assertEquals(
                temp.resolve(".shaft/local-mutes.json").normalize(),
                LocalMuteStore.defaultStorePath(temp));
    }

    @Test
    void reloadPreservesMutedEntries() {
        Path storeFile = temp.resolve("shared-mutes.json");
        LocalMuteStore.open(storeFile).mute("Shared#test", "team opt-in file", 2, false);

        LocalMuteStore reloaded = LocalMuteStore.open(storeFile);
        assertTrue(reloaded.isMuted("Shared#test"));
        assertEquals("team opt-in file", reloaded.get("Shared#test").reason());
        assertEquals(2, reloaded.get("Shared#test").recoverAfterPasses());
    }
}
