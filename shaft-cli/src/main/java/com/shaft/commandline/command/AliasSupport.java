package com.shaft.commandline.command;

import java.io.PrintWriter;
import java.util.Locale;
import java.util.Map;

/**
 * Shared helper for the curated alias commands, which are pure {@code action -> tool name} mappings
 * over the generic {@code call} path.
 */
final class AliasSupport {

    private AliasSupport() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves an action token to its tool name, or reports the valid actions and returns {@code null}.
     *
     * @param actions the action-to-tool map
     * @param action  the requested action token
     * @param label   the command label for the error message (e.g. {@code browser})
     * @param err     the error writer
     * @return the tool name, or {@code null} if the action is unknown
     */
    static String resolve(Map<String, String> actions, String action, String label, PrintWriter err) {
        String tool = action == null ? null : actions.get(action.toLowerCase(Locale.ROOT));
        if (tool == null) {
            err.println("Unknown " + label + " action: " + action + ". Valid actions: "
                    + String.join(", ", actions.keySet()));
            err.flush();
        }
        return tool;
    }
}
