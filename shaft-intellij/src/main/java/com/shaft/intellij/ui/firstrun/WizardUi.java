package com.shaft.intellij.ui.firstrun;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JSlider;
import javax.swing.JTextArea;
import java.awt.BorderLayout;

/** Swing helpers. Call sites pass bundle text so labels stay out of source literals. */
final class WizardUi {
    private WizardUi() {
    }

    static JLabel label(String text, String accessibleName) {
        JLabel label = new JLabel();
        label.setText(text);
        name(label, accessibleName);
        return label;
    }

    static JLabel iconLabel(Icon icon, String text, String accessibleName) {
        JLabel label = label(text, accessibleName);
        label.setIcon(icon);
        return label;
    }

    static JButton button(String text, String accessibleName, Runnable action) {
        JButton button = new JButton();
        button.setText(text);
        name(button, accessibleName);
        button.setDefaultCapable(false);
        button.addActionListener(event -> action.run());
        return button;
    }

    static JPasswordField password(String accessibleName) {
        JPasswordField field = new JPasswordField();
        name(field, accessibleName);
        return field;
    }

    static JTextArea wrappingText(String accessibleName) {
        JTextArea area = new JTextArea();
        area.setLineWrap(true);
        area.setWrapStyleWord(true);
        area.setColumns(24);
        area.setRows(5);
        area.setEditable(false);
        name(area, accessibleName);
        return area;
    }

    static JSlider slider(String accessibleName) {
        JSlider slider = new JSlider(0, 5, 0);
        name(slider, accessibleName);
        slider.setMajorTickSpacing(1);
        slider.setPaintTicks(true);
        return slider;
    }

    static JCheckBox check(String text, String accessibleName) {
        JCheckBox box = new JCheckBox();
        box.setText(text);
        name(box, accessibleName);
        return box;
    }

    static JComboBox<String> combo(String accessibleName) {
        JComboBox<String> combo = new JComboBox<>();
        name(combo, accessibleName);
        return combo;
    }

    static JPanel column() {
        JPanel panel = new JPanel(new BorderLayout(0, 6));
        return panel;
    }

    static void name(JComponent component, String accessibleName) {
        component.getAccessibleContext().setAccessibleName(accessibleName);
    }

    static void describe(JComponent component, String description) {
        component.getAccessibleContext().setAccessibleDescription(description);
    }
}
