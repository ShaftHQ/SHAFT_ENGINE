package com.shaft.fuzz;

import com.shaft.gui.internal.aria.AriaSnapshotHelper;
import org.yaml.snakeyaml.error.YAMLException;

import java.nio.charset.StandardCharsets;

/**
 * Jazzer target (#7012): {@code AriaSnapshotHelper.parse} may reject malformed YAML with
 * {@link YAMLException} or {@link IllegalArgumentException}, and nothing else.
 */
public final class AriaSnapshotFuzzer {
    private AriaSnapshotFuzzer() {
    }

    public static void fuzzerTestOneInput(byte[] data) {
        try {
            AriaSnapshotHelper.parse(new String(data, StandardCharsets.UTF_8));
        } catch (YAMLException | IllegalArgumentException expected) {
            // documented rejection
        }
    }
}
