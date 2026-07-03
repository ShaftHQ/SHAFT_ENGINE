package com.shaft.intellij.ui;

import com.intellij.util.ui.JBUI;

import javax.swing.BorderFactory;
import javax.swing.ButtonModel;
import javax.swing.JButton;
import javax.swing.Icon;
import javax.swing.UIManager;
import javax.swing.event.ChangeListener;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
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
    private static final String STATE_LISTENER_PROPERTY = ShaftIconButtons.class.getName() + ".stateListener";
    private static final Cursor HAND_CURSOR = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
    private static final Cursor DEFAULT_CURSOR = Cursor.getDefaultCursor();

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
        button.setMargin(JBUI.insets(3));
        button.setOpaque(true);
        button.setContentAreaFilled(true);
        button.setBorderPainted(true);
        button.setFocusPainted(true);
        button.setRolloverEnabled(true);
        installStateFeedback(button);
    }

    public static void widen(JButton button, int width) {
        Dimension size = JBUI.size(width, SIZE);
        button.setPreferredSize(size);
        button.setMinimumSize(size);
        button.setMaximumSize(size);
    }

    private static void installStateFeedback(JButton button) {
        Object existing = button.getClientProperty(STATE_LISTENER_PROPERTY);
        if (existing instanceof ChangeListener listener) {
            button.getModel().removeChangeListener(listener);
        }
        ChangeListener listener = event -> updateStateFeedback(button);
        button.getModel().addChangeListener(listener);
        button.putClientProperty(STATE_LISTENER_PROPERTY, listener);
        updateStateFeedback(button);
    }

    private static void updateStateFeedback(JButton button) {
        ButtonModel model = button.getModel();
        Color base = color("Button.background", color("Panel.background", Color.WHITE));
        Color foreground = color("Button.foreground", Color.BLACK);
        Color hover = color("ActionButton.hoverBackground", mix(base, foreground, 0.12D));
        Color pressed = color("ActionButton.pressedBackground", mix(base, foreground, 0.20D));
        Color disabled = color("Button.disabledBackground", mix(base, foreground, 0.06D));
        Color background;
        if (!model.isEnabled()) {
            background = visuallyDistinct(disabled, base, foreground, 0.06D);
            button.setCursor(DEFAULT_CURSOR);
        } else if (model.isPressed() && model.isArmed()) {
            background = visuallyDistinct(pressed, base, foreground, 0.20D);
            button.setCursor(HAND_CURSOR);
        } else if (model.isRollover()) {
            background = visuallyDistinct(hover, base, foreground, 0.12D);
            button.setCursor(HAND_CURSOR);
        } else {
            background = base;
            button.setCursor(HAND_CURSOR);
        }
        Color border = model.isEnabled()
                ? mix(background, foreground, model.isRollover() || model.isPressed() ? 0.24D : 0.14D)
                : mix(background, foreground, 0.08D);
        button.setBackground(background);
        button.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(border),
                JBUI.Borders.empty(2)));
    }

    private static Color visuallyDistinct(Color candidate, Color base, Color foreground, double weight) {
        return sameRgb(candidate, base) ? mix(base, foreground, weight) : candidate;
    }

    private static boolean sameRgb(Color first, Color second) {
        return first != null
                && second != null
                && first.getRed() == second.getRed()
                && first.getGreen() == second.getGreen()
                && first.getBlue() == second.getBlue();
    }

    private static Color color(String key, Color fallback) {
        Color value = UIManager.getColor(key);
        return value == null ? fallback : value;
    }

    private static Color mix(Color base, Color overlay, double overlayWeight) {
        double bounded = Math.max(0.0D, Math.min(1.0D, overlayWeight));
        double baseWeight = 1.0D - bounded;
        return new Color(
                channel(base.getRed(), overlay.getRed(), baseWeight, bounded),
                channel(base.getGreen(), overlay.getGreen(), baseWeight, bounded),
                channel(base.getBlue(), overlay.getBlue(), baseWeight, bounded));
    }

    private static int channel(int base, int overlay, double baseWeight, double overlayWeight) {
        return Math.max(0, Math.min(255, (int) Math.round(base * baseWeight + overlay * overlayWeight)));
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
