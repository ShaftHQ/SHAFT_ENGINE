package com.shaft.cli.internal;

import java.util.Iterator;
import java.util.List;

/**
 * Builds a single remote shell line from command parts (with optional docker exec wrapping),
 * matching legacy {@code TerminalActions} behavior.
 */
public final class RemoteCommandBundler {
    private RemoteCommandBundler() {
    }

    public static String buildLongCommand(List<String> commands, boolean dockerized,
                                          String dockerName, String dockerUsername, int dockerCommandTimeoutSeconds) {
        StringBuilder command = new StringBuilder();
        for (Iterator<String> i = commands.iterator(); i.hasNext(); ) {
            if (command.isEmpty()) {
                command.append(i.next());
            } else {
                command.append(" && ").append(i.next());
            }
        }
        if (dockerized) {
            command.insert(0, "docker exec -u " + dockerUsername + " -i " + dockerName + " timeout "
                    + dockerCommandTimeoutSeconds + " sh -c '");
            command.append("'");
        }
        return command.toString();
    }
}
