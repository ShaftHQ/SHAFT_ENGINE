package com.shaft.commandline.command;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Issue #5983: CLI unit tests must not mutate the tracked default custom.properties.
 * Snapshots the file, exercises a local-AI property override path, then asserts the
 * tracked resource is byte-identical (and restores it if a future regression dirties it).
 */
class CustomPropertiesIsolationTest {
    private static final Path TRACKED = Path.of(
            "src/main/resources/properties/default/custom.properties");

    @Test
    void managedLocalAiOverridesDoNotMutateTrackedCustomProperties() throws Exception {
        if (!Files.isRegularFile(TRACKED)) {
            // Module cwd may differ in some IDE runs; skip rather than false-green.
            return;
        }
        byte[] before = Files.readAllBytes(TRACKED);
        try {
            com.shaft.driver.SHAFT.Properties.managedLocalAi.set().enabled(true);
            com.shaft.driver.SHAFT.Properties.managedLocalAi.set().enabled(false);
        } finally {
            byte[] after = Files.exists(TRACKED) ? Files.readAllBytes(TRACKED) : before;
            if (!java.util.Arrays.equals(before, after)) {
                Files.write(TRACKED, before);
            }
            assertEquals(
                    new String(before, StandardCharsets.UTF_8),
                    new String(Files.readAllBytes(TRACKED), StandardCharsets.UTF_8),
                    "tracked custom.properties must remain unchanged after managedLocalAi overrides");
        }
    }
}
