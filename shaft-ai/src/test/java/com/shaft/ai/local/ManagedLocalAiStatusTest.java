package com.shaft.ai.local;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class ManagedLocalAiStatusTest {
    @TempDir
    Path temp;

    @Test
    void inspectionIsTypedActionableAndReadOnly() {
        Path cache = temp.resolve("absent-cache");
        assertEquals(ManagedLocalAiStatus.State.DISABLED,
                ManagedLocalAiStatus.inspect(cache, false, "Windows 11", "amd64", "windows-msvc", "", null).state());
        assertEquals(ManagedLocalAiStatus.State.UNSUPPORTED,
                ManagedLocalAiStatus.inspect(cache, true, "Plan 9", "mips", "unknown", "", null).state());
        ManagedLocalAiStatus missing = ManagedLocalAiStatus.inspect(cache, true, "Windows 11", "amd64",
                "windows-msvc", "", "runtime-b10400-windows-x86_64");
        assertEquals(ManagedLocalAiStatus.State.NOT_PROVISIONED, missing.state());
        assertFalse(Files.exists(cache));
    }

    @Test
    void anotherOwnedInstallationDoesNotMakeMissingSelectionCorrupt() throws Exception {
        Path cache = temp.resolve("mixed-cache");
        Path stage = cache.resolve("staging/runtime.extract-test");
        Files.createDirectories(stage);
        Files.writeString(stage.resolve("server"), "binary");
        Files.writeString(stage.resolve(".shaft-ready"), "");
        ManagedLocalAiCache.withLock(cache, java.time.Duration.ofSeconds(1),
                () -> ManagedLocalAiCache.adopt(cache, "installed", stage));

        ManagedLocalAiStatus status = ManagedLocalAiStatus.inspect(cache, true, "Windows 11", "amd64",
                "windows-msvc", "", "missing");
        assertEquals(ManagedLocalAiStatus.State.NOT_PROVISIONED, status.state());
    }
}
