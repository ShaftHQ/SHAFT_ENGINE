package com.shaft.pilot.agent;

import java.util.Locale;

/**
 * Supported local agent frontends for SHAFT Autobot routing.
 */
public enum LocalAgentClient {
    CODEX("codex", "Codex CLI"),
    CLAUDE_CODE("claude", "Claude Code"),
    COPILOT_CLI("copilot", "Copilot CLI");

    private final String executableName;
    private final String displayName;

    LocalAgentClient(String executableName, String displayName) {
        this.executableName = executableName;
        this.displayName = displayName;
    }

    /**
     * Returns the executable name used by default command discovery.
     *
     * @return executable name
     */
    public String executableName() {
        return executableName;
    }

    /**
     * Returns a human-readable client name.
     *
     * @return display name
     */
    public String displayName() {
        return displayName;
    }

    /**
     * Parses user input into a local agent client.
     *
     * @param value client id, enum name, or display name
     * @return matching client
     */
    public static LocalAgentClient from(String value) {
        String normalized = normalize(value);
        for (LocalAgentClient client : values()) {
            if (normalize(client.name()).equals(normalized)
                    || normalize(client.executableName).equals(normalized)
                    || normalize(client.displayName).equals(normalized)) {
                return client;
            }
        }
        throw new IllegalArgumentException("Unsupported local agent client: " + value);
    }

    private static String normalize(String value) {
        return value == null ? "" : value.toLowerCase(Locale.ROOT).replace("-", "_").replace(" ", "_").trim();
    }
}
