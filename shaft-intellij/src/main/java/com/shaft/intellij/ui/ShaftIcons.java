package com.shaft.intellij.ui;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.IconLoader;

import javax.swing.Icon;

/**
 * Action icons for SHAFT UI (Carl F03 / F04 / W1.2).
 *
 * <p>Generic metaphors come from {@link AllIcons}. The plugin-owned attach glyph
 * loads via {@link IconLoader}, which resolves {@code _dark} variants without a manual
 * {@code ThemeIcon} switch. A GitHub glyph is not shipped until a control wires it.
 */
public final class ShaftIcons {
    public static final Icon ADD = AllIcons.General.Add;
    /** Plugin-owned: AllIcons has no stable AttachFile on platform 243. */
    public static final Icon ATTACH = IconLoader.getIcon("/icons/actions/attach.svg", ShaftIcons.class);
    public static final Icon CANCEL = AllIcons.Actions.Cancel;
    public static final Icon CHECK = AllIcons.General.InspectionsOK;
    public static final Icon CLEAR = AllIcons.Actions.GC;
    public static final Icon CODE = AllIcons.FileTypes.Java;
    public static final Icon COPY = AllIcons.Actions.Copy;
    public static final Icon DEBUG = AllIcons.Actions.StartDebugger;
    public static final Icon DELETE = AllIcons.General.Remove;
    public static final Icon DOWNLOAD = AllIcons.Actions.Download;
    public static final Icon EDIT = AllIcons.Actions.Edit;
    public static final Icon HELP = AllIcons.General.ContextHelp;
    public static final Icon MOVE_DOWN = AllIcons.Actions.MoveDown;
    public static final Icon MOVE_UP = AllIcons.Actions.MoveUp;
    public static final Icon RERUN = AllIcons.Actions.Restart;
    public static final Icon RESET = AllIcons.Actions.Rollback;
    public static final Icon SEARCH = AllIcons.Actions.Find;
    public static final Icon SEND = AllIcons.Actions.Execute;
    public static final Icon SETTINGS = AllIcons.General.Settings;
    public static final Icon VIEW = AllIcons.Actions.Preview;

    private ShaftIcons() {
        throw new IllegalStateException("Utility class");
    }
}
