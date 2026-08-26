package com.shaft.intellij.ui;

import com.intellij.ui.AnimatedIcon;
import com.intellij.util.ui.JBUI;

import javax.swing.BorderFactory;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.ChangeListener;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.GraphicsEnvironment;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;

/** Shared interaction feedback for every labeled SHAFT tool-window button. */
public final class ShaftButtonInteractions {
    private static final String INSTALLED = ShaftButtonInteractions.class.getName() + ".installed";
    private static final String IDLE_TEXT = ShaftButtonInteractions.class.getName() + ".idleText";
    private static final String IDLE_ICON = ShaftButtonInteractions.class.getName() + ".idleIcon";
    private static final String IDLE_ENABLED = ShaftButtonInteractions.class.getName() + ".idleEnabled";
    private static final String ACTIVE_TIMER = ShaftButtonInteractions.class.getName() + ".activeTimer";
    private static final Cursor HAND = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
    private static final Cursor DEFAULT = Cursor.getDefaultCursor();

    private ShaftButtonInteractions() {
        throw new IllegalStateException("Utility class");
    }

    public static JButton create() {
        return apply(new JButton());
    }

    public static JButton create(String text) {
        return apply(new JButton(text));
    }

    public static JButton apply(JButton button) {
        if (Boolean.TRUE.equals(button.getClientProperty(INSTALLED))) {
            return button;
        }
        button.putClientProperty(INSTALLED, true);
        button.setRolloverEnabled(true);
        button.setFocusPainted(true);
        button.setContentAreaFilled(true);
        button.setOpaque(true);
        button.setCursor(button.isEnabled() ? HAND : DEFAULT);
        ChangeListener stateListener = ignored -> animateToState(button);
        button.getModel().addChangeListener(stateListener);
        button.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(FocusEvent event) {
                animateToState(button);
            }

            @Override
            public void focusLost(FocusEvent event) {
                animateToState(button);
            }
        });
        applyState(button, target(button));
        return button;
    }

    public static void setBusy(JButton button, boolean busy) {
        apply(button);
        if (busy) {
            if (button.getClientProperty(IDLE_TEXT) == null) {
                button.putClientProperty(IDLE_TEXT, button.getText());
                button.putClientProperty(IDLE_ICON, button.getIcon());
                button.putClientProperty(IDLE_ENABLED, button.isEnabled());
            }
            button.setEnabled(false);
            button.setIcon(AnimatedIcon.Default.INSTANCE);
        } else {
            Object text = button.getClientProperty(IDLE_TEXT);
            Object icon = button.getClientProperty(IDLE_ICON);
            if (text instanceof String idleText) {
                button.setText(idleText);
            }
            if (icon == null || icon instanceof Icon) {
                button.setIcon((Icon) icon);
            }
            button.putClientProperty(IDLE_TEXT, null);
            button.putClientProperty(IDLE_ICON, null);
            button.setEnabled(!Boolean.FALSE.equals(button.getClientProperty(IDLE_ENABLED)));
            button.putClientProperty(IDLE_ENABLED, null);
        }
    }

    private static void animateToState(JButton button) {
        Color start = button.getBackground();
        Target target = target(button);
        button.setCursor(button.isEnabled() ? HAND : DEFAULT);
        if (button.getClientProperty(ACTIVE_TIMER) instanceof Timer activeTimer) {
            activeTimer.stop();
        }
        if (GraphicsEnvironment.isHeadless() || UIManager.getBoolean("Component.disableAnimations")) {
            applyState(button, target);
            return;
        }
        int duration = button.getModel().isPressed() ? 80 : 120;
        long started = System.nanoTime();
        Timer timer = new Timer(16, null);
        timer.addActionListener(event -> {
            double elapsed = (System.nanoTime() - started) / 1_000_000.0D;
            double progress = Math.min(1.0D, elapsed / duration);
            button.setBackground(blend(start, target.background(), easeOut(progress)));
            applyBorder(button, target.border());
            button.repaint();
            if (progress >= 1.0D) {
                timer.stop();
                button.putClientProperty(ACTIVE_TIMER, null);
            }
        });
        timer.setRepeats(true);
        button.putClientProperty(ACTIVE_TIMER, timer);
        timer.start();
    }

    private static Target target(JButton button) {
        ButtonModel model = button.getModel();
        Color base = color("Button.background", color("Panel.background", new Color(0xF6F8FA)));
        Color primary = ShaftDesignTokens.primary();
        Color background = base;
        double borderWeight = button.hasFocus() ? 1.0D : 0.35D;
        if (!button.isEnabled()) {
            background = blend(base, color("Button.disabledForeground", Color.GRAY), 0.08D);
            borderWeight = 0.18D;
        } else if (model.isPressed() && model.isArmed()) {
            background = blend(base, primary, 0.24D);
            borderWeight = 0.85D;
        } else if (model.isRollover()) {
            background = blend(base, primary, 0.14D);
            borderWeight = 0.65D;
        }
        return new Target(background, blend(base, primary, borderWeight));
    }

    private static void applyState(JButton button, Target target) {
        button.setBackground(target.background());
        applyBorder(button, target.border());
        button.repaint();
    }

    private static void applyBorder(JButton button, Color border) {
        button.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(border), JBUI.Borders.empty(2, 8)));
    }

    private static Color color(String key, Color fallback) {
        Color color = UIManager.getColor(key);
        return color == null ? fallback : color;
    }

    private static Color blend(Color first, Color second, double weight) {
        double bounded = Math.max(0, Math.min(1, weight));
        return new Color(
                channel(first.getRed(), second.getRed(), bounded),
                channel(first.getGreen(), second.getGreen(), bounded),
                channel(first.getBlue(), second.getBlue(), bounded));
    }

    private static int channel(int first, int second, double weight) {
        return (int) Math.round(first * (1 - weight) + second * weight);
    }

    private static double easeOut(double value) {
        return 1 - Math.pow(1 - value, 3);
    }

    private record Target(Color background, Color border) { }
}
