package com.shaft.intellij.ui;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.IconLoader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import javax.swing.Icon;
import javax.swing.JButton;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * W1.2 / Carl F03–F04: generic actions resolve through {@link AllIcons}; ThemeIcon is gone.
 * Plugin-owned ATTACH must paint visible pixels under headless IconLoader.activate().
 */
class ShaftIconsTest {

    @BeforeAll
    static void activateIconLoader() {
        IconLoader.activate();
    }

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
    void pluginOwnedAttachIconPaintsWithoutThemeIconHelper() {
        assertNotNull(ShaftIcons.ATTACH);
        assertTrue(ShaftIcons.ATTACH.getIconWidth() >= 16, "ATTACH width");
        assertTrue(ShaftIcons.ATTACH.getIconHeight() >= 16, "ATTACH height");
        assertTrue(
                paintsVisiblePixels(ShaftIcons.ATTACH),
                "ATTACH IconLoader SVG must paint visible pixels under headless IconLoader.activate()");
        // ThemeIcon record must not survive W1.2.
        assertTrue(
                java.util.Arrays.stream(ShaftIcons.class.getDeclaredClasses())
                        .noneMatch(c -> "ThemeIcon".equals(c.getSimpleName())),
                "ThemeIcon must be deleted once AllIcons / IconLoader handle dark variants");
    }

    /** Minimal paint probe matching ShaftPanelSetupTest.paintsVisiblePixels for IconLoader SVGs. */
    private static boolean paintsVisiblePixels(Icon icon) {
        BufferedImage image = new BufferedImage(
                Math.max(1, icon.getIconWidth()),
                Math.max(1, icon.getIconHeight()),
                BufferedImage.TYPE_INT_ARGB);
        Graphics2D graphics = image.createGraphics();
        try {
            JButton target = new JButton();
            target.setForeground(java.awt.Color.BLACK);
            graphics.setColor(java.awt.Color.BLACK);
            icon.paintIcon(target, graphics, 0, 0);
        } finally {
            graphics.dispose();
        }
        for (int y = 0; y < image.getHeight(); y++) {
            for (int x = 0; x < image.getWidth(); x++) {
                if ((image.getRGB(x, y) >>> 24) > 0) {
                    return true;
                }
            }
        }
        return false;
    }
}
