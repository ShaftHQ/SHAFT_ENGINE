package com.shaft.infrastructure;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

class VerifiedArtifactStoreTest {
    @Test
    void androidCommandLineToolsHaveATargetSpecificBoundWithoutWeakeningOtherArtifacts() {
        assertEquals(256L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.ANDROID_SDK));
        assertEquals(128L * 1024 * 1024,
                VerifiedArtifactStore.maximumArtifactBytes(SetupTarget.NODE));
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
}
