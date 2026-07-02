package com.shaft.intellij.ui;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.JBColor;

import javax.swing.Icon;
import java.awt.Component;
import java.awt.Graphics;

public final class ShaftIcons {
    public static final Icon ADD = AllIcons.General.Add;
    public static final Icon CANCEL = AllIcons.Actions.Cancel;
    public static final Icon CHECK = AllIcons.General.GreenCheckmark;
    public static final Icon CLEAR = AllIcons.Actions.GC;
    public static final Icon CODE = AllIcons.Actions.ShowCode;
    public static final Icon COPY = AllIcons.Actions.Copy;
    public static final Icon DOWNLOAD = AllIcons.Actions.Download;
    public static final Icon EDIT = AllIcons.Actions.Edit;
    public static final Icon GITHUB = load("github");
    public static final Icon HELP = AllIcons.Actions.Help;
    public static final Icon RERUN = AllIcons.Actions.Rerun;
    public static final Icon RESET = AllIcons.Actions.Rollback;
    public static final Icon SEARCH = AllIcons.Actions.Search;
    public static final Icon SEND = AllIcons.Actions.Execute;
    public static final Icon SETTINGS = AllIcons.General.Settings;
    public static final Icon VIEW = AllIcons.Actions.Show;

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
