package com.shaft.pilot.agent;

import java.util.Locale;

/**
 * SHAFT Autobot interaction mode.
 */
public enum LocalAgentMode {
    ASK,
    PLAN,
    AGENT;

    /**
     * Parses user input into an Autobot mode.
     *
     * @param value mode name
     * @return matching mode
     */
    public static LocalAgentMode from(String value) {
        String normalized = value == null ? "" : value.trim().toUpperCase(Locale.ROOT).replace('-', '_');
        return LocalAgentMode.valueOf(normalized);
    }
}
