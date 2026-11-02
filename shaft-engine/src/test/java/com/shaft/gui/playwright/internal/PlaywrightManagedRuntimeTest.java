package com.shaft.gui.playwright.internal;

import com.shaft.infrastructure.SetupMode;
import com.shaft.infrastructure.SetupOptions;
import com.shaft.infrastructure.SetupProfile;
import com.shaft.infrastructure.ShaftCachePaths;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

class PlaywrightManagedRuntimeTest {
    @Test
    void explicitRemoteChannelOrBrowserPathWinsWithoutManagedLookup() throws Exception {
        Path temp = Files.createTempDirectory("playwright-managed-precedence-");
        try {
            SetupOptions managed = SetupOptions.defaults(SetupProfile.PLAYWRIGHT, paths(temp))
                    .withMode(SetupMode.MANAGED);

            Assert.assertTrue(PlaywrightManagedRuntime.environment(managed, "connect", "", Map.of()).isEmpty());
            Assert.assertTrue(PlaywrightManagedRuntime.environment(managed, "local", "chrome", Map.of()).isEmpty());
            Assert.assertTrue(PlaywrightManagedRuntime.environment(managed, "local", "",
                    Map.of("PLAYWRIGHT_BROWSERS_PATH", temp.resolve("external").toString())).isEmpty());
            Assert.assertTrue(Files.notExists(managed.paths().cacheRoot()));
            Assert.assertTrue(Files.notExists(managed.paths().dataRoot()));
        } finally {
            deleteTree(temp);
        }
    }

    @Test
    void localManagedModeRequiresACompatibleReceiptWithoutInstalling() throws Exception {
        Path temp = Files.createTempDirectory("playwright-managed-missing-");
        try {
            SetupOptions managed = SetupOptions.defaults(SetupProfile.PLAYWRIGHT, paths(temp))
                    .withMode(SetupMode.MANAGED);

            IllegalStateException failure = Assert.expectThrows(IllegalStateException.class,
                    () -> PlaywrightManagedRuntime.environment(managed, "local", "", Map.of()));

            Assert.assertTrue(failure.getMessage().contains("setup plan"), failure.getMessage());
            Assert.assertTrue(failure.getMessage().contains("PLAYWRIGHT"), failure.getMessage());
            Assert.assertTrue(Files.notExists(managed.paths().cacheRoot()));
            Assert.assertTrue(Files.notExists(managed.paths().dataRoot()));
        } finally {
            deleteTree(temp);
        }
    }

    @Test
    void externalModeLeavesTheDefaultPlaywrightResolutionUntouched() throws Exception {
        Path temp = Files.createTempDirectory("playwright-external-");
        try {
            SetupOptions external = SetupOptions.defaults(SetupProfile.PLAYWRIGHT, paths(temp));

            Assert.assertTrue(PlaywrightManagedRuntime.environment(external, "local", "", Map.of()).isEmpty());
            Assert.assertTrue(Files.notExists(external.paths().cacheRoot()));
            Assert.assertTrue(Files.notExists(external.paths().dataRoot()));
        } finally {
            deleteTree(temp);
        }
    }

    @Test
    void readyManagedRootIsAppliedOnlyToTheChildEnvironment() throws Exception {
        Path temp = Files.createTempDirectory("playwright-managed-ready-");
        String before = System.getProperty("PLAYWRIGHT_BROWSERS_PATH");
        try {
            SetupOptions managed = SetupOptions.defaults(SetupProfile.PLAYWRIGHT, paths(temp))
                    .withMode(SetupMode.MANAGED);
            Path verifiedRoot = temp.resolve("verified-browsers").toAbsolutePath();

            Map<String, String> child = PlaywrightManagedRuntime.environment(managed, "local", "", Map.of(),
                    ignored -> verifiedRoot);

            Assert.assertEquals(child, Map.of("PLAYWRIGHT_BROWSERS_PATH", verifiedRoot.toString()));
            Assert.assertEquals(System.getProperty("PLAYWRIGHT_BROWSERS_PATH"), before);
            Assert.assertFalse(System.getenv().containsKey("SHAFT_PLAYWRIGHT_TEST_MUTATION"));
        } finally {
            if (before == null) System.clearProperty("PLAYWRIGHT_BROWSERS_PATH");
            else System.setProperty("PLAYWRIGHT_BROWSERS_PATH", before);
            deleteTree(temp);
        }
    }

    private static ShaftCachePaths paths(Path temp) {
        Path cache = temp.resolve("cache").toAbsolutePath();
        Path data = temp.resolve("data").toAbsolutePath();
        return new ShaftCachePaths(cache, data, cache.resolve("downloads"), data.resolve("tools"),
                data.resolve("state"), data.resolve("receipts"));
    }

    private static void deleteTree(Path root) throws Exception {
        if (Files.notExists(root)) return;
        try (var entries = Files.walk(root)) {
            for (Path path : entries.sorted(java.util.Comparator.reverseOrder()).toList()) Files.deleteIfExists(path);
        }
    }
}
