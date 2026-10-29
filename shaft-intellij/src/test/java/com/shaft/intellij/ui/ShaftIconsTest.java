package com.shaft.intellij.ui;

import com.intellij.icons.AllIcons;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * W1.2 / Carl F03–F04: generic actions resolve through {@link AllIcons}; ThemeIcon is gone.
 */
class ShaftIconsTest {

    @Test
    void genericActionsReuseAllIcons() {
        assertAll(
                () -> assertSame(AllIcons.General.Add, ShaftIcons.ADD),
                () -> assertSame(AllIcons.Actions.Cancel, ShaftIcons.CANCEL),
                () -> assertSame(AllIcons.General.InspectionsOK, ShaftIcons.CHECK),
                () -> assertSame(AllIcons.Actions.GC, ShaftIcons.CLEAR),
                () -> assertSame(AllIcons.FileTypes.Java, ShaftIcons.CODE),
                () -> assertSame(AllIcons.Actions.Copy, ShaftIcons.COPY),
                () -> assertSame(AllIcons.Actions.StartDebugger, ShaftIcons.DEBUG),
                () -> assertSame(AllIcons.General.Remove, ShaftIcons.DELETE),
                () -> assertSame(AllIcons.Actions.Download, ShaftIcons.DOWNLOAD),
                () -> assertSame(AllIcons.Actions.Edit, ShaftIcons.EDIT),
                () -> assertSame(AllIcons.General.ContextHelp, ShaftIcons.HELP),
                () -> assertSame(AllIcons.Actions.MoveDown, ShaftIcons.MOVE_DOWN),
                () -> assertSame(AllIcons.Actions.MoveUp, ShaftIcons.MOVE_UP),
                () -> assertSame(AllIcons.Actions.Restart, ShaftIcons.RERUN),
                () -> assertSame(AllIcons.Actions.Rollback, ShaftIcons.RESET),
                () -> assertSame(AllIcons.Actions.Find, ShaftIcons.SEARCH),
                () -> assertSame(AllIcons.Actions.Execute, ShaftIcons.SEND),
                () -> assertSame(AllIcons.General.Settings, ShaftIcons.SETTINGS),
                () -> assertSame(AllIcons.Actions.Preview, ShaftIcons.VIEW));
    }

    @Test
    void pluginOwnedGithubIconLoadsWithoutThemeIconHelper() {
        assertNotNull(ShaftIcons.ATTACH);
        assertNotNull(ShaftIcons.GITHUB);
        // ThemeIcon record must not survive W1.2.
        assertTrue(
                java.util.Arrays.stream(ShaftIcons.class.getDeclaredClasses())
                        .noneMatch(c -> "ThemeIcon".equals(c.getSimpleName())),
                "ThemeIcon must be deleted once AllIcons / IconLoader handle dark variants");
    }
}
