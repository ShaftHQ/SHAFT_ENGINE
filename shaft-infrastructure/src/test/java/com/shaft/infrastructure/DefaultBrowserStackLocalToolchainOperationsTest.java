package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DefaultBrowserStackLocalToolchainOperationsTest {
    @Test
    void startRefusesAnUnownedBinary(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), false);

        IOException failure = assertThrows(java.io.IOException.class,
                () -> operations.startTunnel(temp.resolve("BrowserStackLocal"), "key", paths.state().resolve("x.log")));
        assertTrue(failure.getMessage().contains("unowned"));
    }

    @Test
    void stopRefusesAnUnownedBinary(@TempDir Path temp) {
        ShaftCachePaths paths = paths(temp);
        DefaultBrowserStackLocalToolchainOperations operations = new DefaultBrowserStackLocalToolchainOperations(
                paths, plan(), false);

        IOException failure = assertThrows(java.io.IOException.class,
                () -> operations.stopProcess(1, temp.resolve("BrowserStackLocal")));
        assertTrue(failure.getMessage().contains("unowned"));
    }

    @Test
    void startCommandNeverUsesDaemonMode(@TempDir Path temp) {
        List<String> command = DefaultBrowserStackLocalToolchainOperations.startCommand(
                temp.resolve("BrowserStackLocal"), "secret-key");
        assertFalse(command.contains("--daemon"));
        assertFalse(command.contains("stop"));
        assertEquals(List.of(temp.resolve("BrowserStackLocal").toString(), "--key", "secret-key"), command);
    }

    private static SetupPlan plan() {
        return BrowserStackLocalSetupPlanner.plan(SetupPlatform.LINUX, SetupArchitecture.X64, SetupMode.MANAGED);
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache");
        Path data = temp.resolve("data");
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }
}
