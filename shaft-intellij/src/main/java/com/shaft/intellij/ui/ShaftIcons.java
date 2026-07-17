package com.shaft.intellij.ui;

import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;

import javax.swing.Icon;
import java.awt.Component;
import java.awt.Graphics;

public final class ShaftIcons {
    public static final Icon ADD = load("add");
    public static final Icon CANCEL = load("cancel");
    public static final Icon CHECK = load("check");
    public static final Icon CLEAR = load("clear");
    public static final Icon CODE = load("code");
    public static final Icon COPY = load("copy");
    public static final Icon DEBUG = load("debug");
    public static final Icon DELETE = load("delete");
    public static final Icon DOWNLOAD = load("download");
    public static final Icon EDIT = load("edit");
    public static final Icon GITHUB = load("github");
    public static final Icon HELP = load("help");
    public static final Icon MOVE_DOWN = load("move_down");
    public static final Icon MOVE_UP = load("move_up");
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
        Icon light = IconLoader.getIcon("/icons/actions/" + name + ".svg", ShaftIcons.class);
        Icon dark = IconLoader.getIcon("/icons/actions/" + name + "_dark.svg", ShaftIcons.class);
        return new ThemeIcon(light, dark);
    }

    private record ThemeIcon(Icon light, Icon dark) implements Icon {
        @Override
        public void paintIcon(Component component, Graphics graphics, int x, int y) {
            icon().paintIcon(component, graphics, x, y);
        }

        @Override
        public int getIconWidth() {
            return icon().getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return icon().getIconHeight();
        }

        private Icon icon() {
            return JBColor.isBright() ? light : dark;
        }
    }
}
