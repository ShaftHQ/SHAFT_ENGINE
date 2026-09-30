package com.shaft.intellij.ui;

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
