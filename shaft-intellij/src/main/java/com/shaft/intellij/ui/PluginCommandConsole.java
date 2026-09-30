package com.shaft.intellij.ui;

/** In-plugin console that receives the exact command the user is asked to run. */
public final class PluginCommandConsole {
    private String placed = "";

    public String place(String command) {
        placed = command == null ? "" : command;
        return placed;
    }

    public String placed() {
        return placed;
    }
}
