package com.shaft.mcp;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpEmulatorMemoryProbeTest {
    private static final long GIB_KB = 1024L * 1024L;

    @Test
    void warnsOnlyAboveTheThreshold() {
        var warnings = McpEmulatorMemoryProbe.warnings(Map.of(10L, 2 * GIB_KB, 11L, 30 * GIB_KB), 6);

        assertEquals(1, warnings.size());
        assertTrue(warnings.getFirst().contains("process 11"), warnings.getFirst());
        assertTrue(warnings.getFirst().contains("30 GiB"), warnings.getFirst());
    }

    @Test
    void readsResidentMemoryFromProcStatus(@TempDir Path directory) throws Exception {
        Path status = directory.resolve("status");
        Files.writeString(status, "Name:\tqemu-system-x86\nVmRSS:\t  31457280 kB\n");

        assertEquals(31457280L, McpEmulatorMemoryProbe.residentKb(status));
        assertEquals(0L, McpEmulatorMemoryProbe.residentKb(directory.resolve("missing")));
    }
}
