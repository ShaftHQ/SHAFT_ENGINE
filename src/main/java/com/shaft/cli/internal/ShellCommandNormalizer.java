package com.shaft.cli.internal;

import java.util.Arrays;
import java.util.List;

/**
 * Normalizes a single-string command list that contains {@code &&} or {@code ;} chains,
 * matching legacy {@code TerminalActions} behavior for local execution paths.
 */
public final class ShellCommandNormalizer {
    private ShellCommandNormalizer() {
    }

    public static List<String> expandSingleCommandChaining(List<String> commands) {
        if (commands.size() == 1) {
            String first = commands.getFirst();
            if (first.contains(" && ")) {
                return Arrays.asList(first.split(" && "));
            }
            if (first.contains(" ; ")) {
                return Arrays.asList(first.split(" ; "));
            }
        }
        return commands;
    }
}
