package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Isolated;
import org.junit.jupiter.api.parallel.ResourceAccessMode;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.junit.jupiter.api.parallel.Resources;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Isolated("Temporarily overrides JVM-wide path properties for trusted-anchor regressions")
class VerifiedArtifactStoreTest {
    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void fetchRejectsLinkedDownloadsRootBeforeCacheReadOrMutation(boolean cacheHit, @TempDir Path temp)
            throws Exception {
        Path storage = Files.createDirectory(temp.resolve("storage"));
        Path downloads = temp.resolve("downloads");
        try {
            Files.createSymbolicLink(downloads, storage);
        } catch (UnsupportedOperationException | IOException unsupported) {
            Assumptions.abort("Symbolic links unavailable: " + unsupported.getMessage());
        }
        Path source = temp.resolve("artifact.tgz");
        Files.writeString(source, "verified fixture");
        String checksum = "sha256:" + VerifiedArtifactStore.digest(source);
        SetupAction action = new SetupAction(SetupTarget.APPIUM_SERVER, SetupActionKind.INSTALL,
                "fixture", source.toUri(), checksum, false, Set.of());
        Path cached = storage.resolve(checksum.substring("sha256:".length()) + "-artifact.tgz");
        if (cacheHit) Files.writeString(cached, "verified fixture");

        IOException failure = assertThrows(IOException.class,
                () -> new VerifiedArtifactStore(downloads).fetch(action));

        assertTrue(failure.getMessage().contains("symbolic links"));
        assertEquals(cacheHit, Files.exists(cached));
        if (cacheHit) assertEquals("verified fixture", Files.readString(cached));
        try (var entries = Files.list(storage)) {
            assertEquals(cacheHit ? 1 : 0, entries.count());
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"java.io.tmpdir", "user.home"})
    @ResourceLock(value = Resources.SYSTEM_PROPERTIES, mode = ResourceAccessMode.READ_WRITE)
    void platformLinkAboveTrustedRootIsAccepted(String property, @TempDir Path temp) throws Exception {
        Path platformStorage = Files.createDirectory(temp.resolve("platform-storage"));
        Path platformAlias = temp.resolve("platform-alias");
        try {
            Files.createSymbolicLink(platformAlias, platformStorage);
        } catch (UnsupportedOperationException | IOException unsupported) {
            Assumptions.abort("Symbolic links unavailable: " + unsupported.getMessage());
        }
        Path trustedTemporaryRoot = platformAlias.resolve("owned-temp");
        Files.createDirectories(trustedTemporaryRoot);
        String previousValue = System.getProperty(property);
        try {
            System.setProperty(property, trustedTemporaryRoot.toString());

            assertDoesNotThrow(() -> VerifiedArtifactStore.requireUnlinkedAncestors(
                    trustedTemporaryRoot.resolve("junit/downloads")));
        } finally {
            restoreProperty(property, previousValue);
        }
    }

    @ParameterizedTest
    @ValueSource(strings = {"java.io.tmpdir", "user.home"})
    @ResourceLock(value = Resources.SYSTEM_PROPERTIES, mode = ResourceAccessMode.READ_WRITE)
    void linkedTrustedRootIsRejected(String property, @TempDir Path temp) throws Exception {
        Path platformStorage = Files.createDirectory(temp.resolve("platform-storage"));
        Path linkedTrustedRoot = temp.resolve("linked-root");
        try {
            Files.createSymbolicLink(linkedTrustedRoot, platformStorage);
        } catch (UnsupportedOperationException | IOException unsupported) {
            Assumptions.abort("Symbolic links unavailable: " + unsupported.getMessage());
        }
        String previousValue = System.getProperty(property);
        try {
            System.setProperty(property, linkedTrustedRoot.toString());

            IOException failure = assertThrows(IOException.class,
                    () -> VerifiedArtifactStore.requireUnlinkedAncestors(linkedTrustedRoot.resolve("downloads")));

            assertTrue(failure.getMessage().contains("symbolic links"));
        } finally {
            restoreProperty(property, previousValue);
        }
    }

    @Test
    @ResourceLock(value = Resources.SYSTEM_PROPERTIES, mode = ResourceAccessMode.READ_WRITE)
    void windowsJunctionAtTrustedRootIsRejected(@TempDir Path temp) throws Exception {
        Assumptions.assumeTrue(System.getProperty("os.name", "").toLowerCase().contains("windows"),
                "Windows directory junctions only");
        Path platformStorage = Files.createDirectory(temp.resolve("junction-target"));
        Path junction = temp.resolve("junction-root");
        String commandProcessor = System.getenv().getOrDefault("ComSpec", "C:\\Windows\\System32\\cmd.exe");
        Process process = new ProcessBuilder(commandProcessor, "/d", "/c", "mklink", "/J",
                junction.toString(), platformStorage.toString()).redirectErrorStream(true).start();
        String output = new String(process.getInputStream().readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);
        if (process.waitFor() != 0) {
            Assumptions.abort("Directory junctions unavailable: " + output.trim());
        }
        String previousValue = System.getProperty("java.io.tmpdir");
        try {
            System.setProperty("java.io.tmpdir", junction.toString());

            IOException failure = assertThrows(IOException.class,
                    () -> VerifiedArtifactStore.requireUnlinkedAncestors(junction.resolve("downloads")));

            assertTrue(failure.getMessage().contains("reparse/special"));
        } finally {
            restoreProperty("java.io.tmpdir", previousValue);
        }
    }

    private static void restoreProperty(String property, String previousValue) {
        if (previousValue == null) {
            System.clearProperty(property);
        } else {
            System.setProperty(property, previousValue);
        }
    }

    @Test
    void androidCommandLineToolsHaveATargetSpecificBoundWithoutWeakeningOtherArtifacts() {
        assertEquals(256L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.ANDROID_SDK));
        assertEquals(128L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.NODE));
    }

    @Test
    void playwrightBrowsersHaveATargetSpecificBoundWithoutWeakeningSharedArtifacts() {
        assertEquals(512L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.PLAYWRIGHT_CHROMIUM));
        assertEquals(512L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.PLAYWRIGHT_FIREFOX));
        assertEquals(512L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.PLAYWRIGHT_WEBKIT));
        assertEquals(128L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.FFMPEG));
    }
    @Test
    void oversizedArtifactIsRejectedAndTemporaryFileIsRemoved(@TempDir Path temp) throws Exception {
        Path source = temp.resolve("oversized.bin");
        try (var output = Files.newOutputStream(source)) {
            output.write(new byte[(int) VerifiedArtifactStore.MAX_ARTIFACT_BYTES]);
            output.write(1);
        }
        Path downloads = temp.resolve("downloads");
        SetupAction action = new SetupAction(SetupTarget.OCR_TESSDATA, SetupActionKind.INSTALL,
                "oversized", source.toUri(), "sha256:" + "0".repeat(64), false, Set.of());

        IOException failure = assertThrows(IOException.class,
                () -> new VerifiedArtifactStore(downloads).fetch(action));

        assertTrue(failure.getMessage().contains("safety limit"));
        try (var entries = Files.list(downloads)) {
            assertFalse(entries.findAny().isPresent());
        }
    }

    @Test
    void failedReplacementAndRollbackPreserveQuarantineForNextRetry(@TempDir Path temp) throws Exception {
        Path destination = temp.resolve("model");
        Path replacement = temp.resolve("replacement");
        Path quarantine = temp.resolve("model.quarantine");
        Files.writeString(destination, "original");
        Files.writeString(replacement, "new");
        int[] moves = {0};

        assertThrows(IOException.class, () -> VerifiedArtifactStore.replaceWithRollback(
                replacement, destination, quarantine, (source, target) -> {
                    if (++moves[0] >= 2) throw new IOException("injected move failure");
                    VerifiedArtifactStore.move(source, target);
                }));

        assertFalse(Files.exists(destination));
        assertEquals("original", Files.readString(quarantine));
        assertThrows(IOException.class, () -> VerifiedArtifactStore.replaceWithRollback(
                replacement, destination, quarantine, (source, target) -> {
                    if (source.equals(replacement)) throw new IOException("replacement still unavailable");
                    VerifiedArtifactStore.move(source, target);
                }));
        assertEquals("original", Files.readString(destination));
        assertFalse(Files.exists(quarantine));

        VerifiedArtifactStore.replaceWithRollback(replacement, destination, quarantine);
        assertEquals("new", Files.readString(destination));
        assertFalse(Files.exists(quarantine));
    }

    @Test
    void destinationAndQuarantineFailClosedWithoutChangingEither(@TempDir Path temp) throws Exception {
        Path destination = temp.resolve("model");
        Path replacement = temp.resolve("replacement");
        Path quarantine = temp.resolve("model.quarantine");
        Files.writeString(destination, "current");
        Files.writeString(replacement, "new");
        Files.writeString(quarantine, "recovery");

        assertThrows(IOException.class,
                () -> VerifiedArtifactStore.replaceWithRollback(replacement, destination, quarantine));

        assertEquals("current", Files.readString(destination));
        assertEquals("recovery", Files.readString(quarantine));
        assertEquals("new", Files.readString(replacement));
    }

    @Test
    void successfulDirectoryReplacementRemovesNonEmptyQuarantine(@TempDir Path temp) throws Exception {
        Path destination = Files.createDirectory(temp.resolve("bundle"));
        Files.writeString(destination.resolve("old.txt"), "old");
        Path replacement = Files.createDirectory(temp.resolve("replacement"));
        Files.writeString(replacement.resolve("new.txt"), "new");
        Path quarantine = temp.resolve("bundle.quarantine");

        VerifiedArtifactStore.replaceWithRollback(replacement, destination, quarantine);

        assertEquals("new", Files.readString(destination.resolve("new.txt")));
        assertFalse(Files.exists(destination.resolve("old.txt")));
        assertFalse(Files.exists(quarantine));
    }

    @Test
    void cleanupFailureAfterCommitDoesNotPoisonTheCanonicalQuarantine(@TempDir Path temp) throws Exception {
        Path destination = Files.createDirectory(temp.resolve("bundle"));
        Files.writeString(destination.resolve("old.txt"), "old");
        Path replacement = Files.createDirectory(temp.resolve("replacement"));
        Files.writeString(replacement.resolve("new.txt"), "new");
        Path quarantine = temp.resolve("bundle.quarantine");
        Path obsolete = temp.resolve("bundle.quarantine.obsolete");

        assertDoesNotThrow(() -> VerifiedArtifactStore.replaceWithRollback(
                replacement, destination, quarantine, VerifiedArtifactStore::move,
                ignored -> { throw new IOException("injected cleanup failure"); }));

        assertEquals("new", Files.readString(destination.resolve("new.txt")));
        assertFalse(Files.exists(quarantine));
        assertTrue(Files.isDirectory(obsolete));

        Path next = Files.createDirectory(temp.resolve("next"));
        Files.writeString(next.resolve("latest.txt"), "latest");
        VerifiedArtifactStore.replaceWithRollback(next, destination, quarantine);
        assertEquals("latest", Files.readString(destination.resolve("latest.txt")));
        assertFalse(Files.exists(quarantine));
        assertFalse(Files.exists(obsolete));
    }
}
