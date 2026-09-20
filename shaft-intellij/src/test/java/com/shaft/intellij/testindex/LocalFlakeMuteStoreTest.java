package com.shaft.intellij.testindex;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocalFlakeMuteStoreTest {

    @TempDir
    Path temp;

    @Test
    void muteRequiresReasonAndRecoversAfterConsecutivePasses() {
        LocalFlakeMuteStore store = new LocalFlakeMuteStore(temp.resolve("local-mutes.json"), 2);
        assertThrows(IllegalArgumentException.class, () -> store.mute("T#m", " ", 2));
        store.mute("T#m", "flake", 2);
        assertTrue(store.isMuted("T#m"));
        assertFalse(store.observe("T#m", true));
        assertTrue(store.observe("T#m", true));
        assertFalse(store.isMuted("T#m"));
    }

    @Test
    void failResetsStreak() {
        LocalFlakeMuteStore store = new LocalFlakeMuteStore(temp.resolve("local-mutes.json"), 3);
        store.mute("T#m", "flake", 3);
        store.observe("T#m", true);
        store.observe("T#m", true);
        store.observe("T#m", false);
        assertEquals(0, store.get("T#m").consecutivePasses());
    }

    @Test
    void reloadPreservesEntries() {
        Path file = temp.resolve("mutes.json");
        new LocalFlakeMuteStore(file).mute("Shared#test", "shared", 3);
        LocalFlakeMuteStore reloaded = new LocalFlakeMuteStore(file);
        assertTrue(reloaded.isMuted("Shared#test"));
        assertEquals("shared", reloaded.get("Shared#test").reason());
    }
}
