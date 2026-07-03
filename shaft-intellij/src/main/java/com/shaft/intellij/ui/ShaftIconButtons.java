package com.shaft.intellij.ui;

import com.intellij.util.ui.JBUI;

import javax.swing.JButton;
import javax.swing.Icon;
import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionListener;

/**
 * Shared icon-centric button styling for SHAFT IntelliJ tool windows.
 */
public final class ShaftIconButtons {
    public static final int SIZE = 32;
    private static final float DISABLED_ICON_OPACITY = 0.72F;

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
        button.setDisabledIcon(new OpacityIcon(icon, DISABLED_ICON_OPACITY));
        button.setToolTipText(tooltip);
        button.getAccessibleContext().setAccessibleName(accessibleName);
        button.setPreferredSize(size);
        button.setMinimumSize(size);
        button.setMaximumSize(size);
        button.setMargin(JBUI.emptyInsets());
        button.setOpaque(false);
        button.setContentAreaFilled(false);
        button.setBorder(JBUI.Borders.empty());
        button.setBorderPainted(false);
        button.setFocusPainted(false);
    }

    public static void widen(JButton button, int width) {
        Dimension size = JBUI.size(width, SIZE);
        button.setPreferredSize(size);
        button.setMinimumSize(size);
        button.setMaximumSize(size);
    }

    private record OpacityIcon(Icon delegate, float opacity) implements Icon {
        @Override
        public void paintIcon(Component component, Graphics graphics, int x, int y) {
            Graphics2D copy = (Graphics2D) graphics.create();
            try {
                copy.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity));
                delegate.paintIcon(component, copy, x, y);
            } finally {
                copy.dispose();
            }
        }

        @Override
        public int getIconWidth() {
            return delegate.getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return delegate.getIconHeight();
        }
    }
}
