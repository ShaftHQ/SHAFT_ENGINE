package com.shaft.cli.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Expands single-string command chaining ({@code &&}, {@code ;}) into separate command tokens.
 */
public final class ShellCommandNormalizer {
    private ShellCommandNormalizer() {
    }

    public static List<String> expandSingleCommandChaining(List<String> commands) {
        if (commands == null || commands.isEmpty()) {
            return Collections.emptyList();
        }
        if (commands.size() != 1) {
            return new ArrayList<>(commands);
        }
        String single = commands.getFirst();
        if (single.contains(" && ")) {
            return List.of(single.split(" \\&\\& "));
        }
        if (single.contains(" ; ")) {
            return List.of(single.split(" ; "));
        }
        return new ArrayList<>(commands);
    }
}
