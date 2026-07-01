package com.shaft.intellij.ui;

import com.intellij.ui.JBColor;
import com.intellij.util.ui.JBUI;

import javax.swing.JButton;
import javax.swing.Icon;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionListener;

/**
 * Shared icon-centric button styling for SHAFT IntelliJ tool windows.
 */
public final class ShaftIconButtons {
    public static final int SIZE = 32;
    private static final JBColor ENABLED_BACKGROUND = new JBColor(new Color(0xF4F6F8), new Color(0x4E5963));
    private static final JBColor DISABLED_BACKGROUND = new JBColor(new Color(0xEAEDF1), new Color(0x3F444A));

    private ShaftIconButtons() {
        throw new IllegalStateException("Utility class");
    }

    public static JButton create(String tooltip, String accessibleName, Icon icon, ActionListener action) {
        JButton button = new JButton();
        apply(button, tooltip, accessibleName, icon);
        button.addActionListener(action);
        return button;
    }

    public static void apply(JButton button, Icon icon) {
        String text = button.getText();
        String accessibleName = button.getAccessibleContext().getAccessibleName();
        apply(button, text, accessibleName == null || accessibleName.isBlank() ? text : accessibleName, icon);
    }

    public static void apply(JButton button, String tooltip, String accessibleName, Icon icon) {
        Dimension size = JBUI.size(SIZE, SIZE);
        button.setText("");
        button.setIcon(icon);
        button.setToolTipText(tooltip);
        button.getAccessibleContext().setAccessibleName(accessibleName);
        button.setPreferredSize(size);
        button.setMinimumSize(size);
        button.setMaximumSize(size);
        button.setMargin(JBUI.emptyInsets());
        button.setOpaque(true);
        button.setContentAreaFilled(true);
        button.setBackground(backgroundFor(button));
        button.addPropertyChangeListener("enabled", event -> button.setBackground(backgroundFor(button)));
    }

    private static JBColor backgroundFor(JButton button) {
        return button.isEnabled() ? ENABLED_BACKGROUND : DISABLED_BACKGROUND;
    }
}
