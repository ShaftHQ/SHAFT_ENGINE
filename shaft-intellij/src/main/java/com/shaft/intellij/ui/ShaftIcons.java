package com.shaft.intellij.ui;

import com.intellij.openapi.util.IconLoader;

import javax.swing.Icon;

public final class ShaftIcons {
    public static final Icon ADD = load("add");
    public static final Icon CANCEL = load("cancel");
    public static final Icon CHECK = load("check");
    public static final Icon CLEAR = load("clear");
    public static final Icon CODE = load("code");
    public static final Icon COPY = load("copy");
    public static final Icon DOWNLOAD = load("download");
    public static final Icon EDIT = load("edit");
    public static final Icon GITHUB = load("github");
    public static final Icon HELP = load("help");
    public static final Icon RERUN = load("rerun");
    public static final Icon RESET = load("reset");
    public static final Icon SEARCH = load("search");
    public static final Icon SEND = load("send");
    public static final Icon SETTINGS = load("settings");
    public static final Icon VIEW = load("view");

    private ShaftIcons() {
        throw new IllegalStateException("Utility class");
    }

    private static Icon load(String name) {
        return IconLoader.getIcon("/icons/actions/" + name + ".svg", ShaftIcons.class);
    }
}
