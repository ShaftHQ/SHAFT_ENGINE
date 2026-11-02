package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LighthouseRuntimeTest {
    @Test
    void copiesPrefixAndNormalizesWorkingDirectory(@TempDir Path temp) {
        Path work = temp.resolve("nested").resolve("cwd");
        LighthouseRuntime runtime = new LighthouseRuntime(List.of("node", "lighthouse"), work);
        assertEquals(List.of("node", "lighthouse"), runtime.commandPrefix());
        assertEquals(work.toAbsolutePath().normalize(), runtime.workingDirectory());
        assertThrows(UnsupportedOperationException.class, () -> runtime.commandPrefix().add("x"));
    }

    @Test
    void rejectsEmptyOrBlankCommandPrefix(@TempDir Path temp) {
        assertThrows(IllegalArgumentException.class,
                () -> new LighthouseRuntime(List.of(), temp));
        assertThrows(IllegalArgumentException.class,
                () -> new LighthouseRuntime(List.of("node", " "), temp));
        assertThrows(NullPointerException.class,
                () -> new LighthouseRuntime(null, temp));
        assertThrows(NullPointerException.class,
                () -> new LighthouseRuntime(List.of("node"), null));
    }

    @Test
    void requireReadyFailsWhenManagedRuntimeMissing(@TempDir Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        SetupOptions options = SetupOptions.defaults(SetupProfile.LIGHTHOUSE, new ShaftCachePaths(
                cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts")));
        Exception thrown = assertThrows(Exception.class, () -> LighthouseRuntime.requireReady(options));
        assertTrue(thrown.getMessage().contains("Managed Lighthouse is not ready"), thrown.getMessage());
        assertThrows(NullPointerException.class, () -> LighthouseRuntime.requireReady(null));
    }
}
