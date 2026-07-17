package com.shaft.intellij.testrunner;

import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.BorderLayout;
import java.awt.GridLayout;

/**
 * Swing editor for {@link ShaftRunConfigurationOverrides} (issue #3659), shared as-is by both the
 * JUnit and TestNG SHAFT run-configuration extensions' "SHAFT" tab. Carries no {@code
 * JUnitConfiguration}/{@code TestNGConfiguration} imports so it does not pull either optional SDK
 * dependency into the other's class-loading path (mirrors the isolation already enforced by
 * {@code io.github.shafthq.shaft-withJUnit.xml}/{@code shaft-withTestNG.xml}).
 * <p>
 * Style matches {@code com.shaft.intellij.ui.GuidedWorkflowPanel}: plain {@link JComboBox} and
 * {@link JBCheckBox}/{@link JBTextField} components, not a bespoke look.
 */
final class ShaftRunConfigurationOverridesPanel extends JPanel {
    /** Selecting this combo item means "inherit from custom.properties" (blank/null browser). */
    static final String INHERIT_BROWSER = "(inherit from custom.properties)";
    private static final String[] BROWSERS = {INHERIT_BROWSER, "chrome", "firefox", "edge", "safari"};

    private final JBCheckBox enabledCheckBox =
            new JBCheckBox("Override SHAFT execution properties for this run");
    private final JComboBox<String> browserComboBox = new JComboBox<>(BROWSERS);
    private final JBCheckBox headlessCheckBox = new JBCheckBox("Headless");
    private final JBTextField extraVmArgsField = new JBTextField();

    ShaftRunConfigurationOverridesPanel() {
        super(new BorderLayout(4, 8));
        setBorder(JBUI.Borders.empty(8));

        enabledCheckBox.getAccessibleContext().setAccessibleName("Override SHAFT execution properties for this run");
        browserComboBox.getAccessibleContext().setAccessibleName("SHAFT browser override");
        headlessCheckBox.getAccessibleContext().setAccessibleName("SHAFT headless override");
        extraVmArgsField.getAccessibleContext().setAccessibleName("SHAFT extra VM argument overrides");
        extraVmArgsField.setToolTipText(
                "Extra -D system property overrides applied on top of the browser/headless selections above, "
                        + "e.g. -DexecutionAddress=... -DmobileWebDriverName=...");

        JPanel fields = new JPanel(new GridLayout(0, 1, 4, 4));
        fields.add(row("Browser", browserComboBox));
        fields.add(headlessCheckBox);
        fields.add(row("Extra VM args", extraVmArgsField));

        enabledCheckBox.addItemListener(event -> updateFieldsEnabled());
        updateFieldsEnabled();

        add(enabledCheckBox, BorderLayout.NORTH);
        add(fields, BorderLayout.CENTER);
    }

    private static JPanel row(String labelText, JComponent component) {
        JPanel row = new JPanel(new BorderLayout(4, 4));
        JLabel label = new JLabel(labelText);
        label.setLabelFor(component);
        row.add(label, BorderLayout.WEST);
        row.add(component, BorderLayout.CENTER);
        return row;
    }

    private void updateFieldsEnabled() {
        boolean enabled = enabledCheckBox.isSelected();
        browserComboBox.setEnabled(enabled);
        headlessCheckBox.setEnabled(enabled);
        extraVmArgsField.setEnabled(enabled);
    }

    /** Returns the panel's current state; {@code isEnabled() == false} when the checkbox is unchecked. */
    ShaftRunConfigurationOverrides getState() {
        ShaftRunConfigurationOverrides state = new ShaftRunConfigurationOverrides();
        state.setEnabled(enabledCheckBox.isSelected());
        Object selectedBrowser = browserComboBox.getSelectedItem();
        String browser = selectedBrowser == null ? "" : selectedBrowser.toString();
        state.setBrowser(INHERIT_BROWSER.equals(browser) ? "" : browser);
        state.setHeadless(headlessCheckBox.isSelected());
        state.setExtraVmArgs(extraVmArgsField.getText());
        return state;
    }

    /** Renders {@code state} into the fields; {@code null} renders as the untouched/inherit default. */
    void setState(ShaftRunConfigurationOverrides state) {
        enabledCheckBox.setSelected(state != null && state.isEnabled());
        String browser = state == null || state.getBrowser() == null ? "" : state.getBrowser();
        browserComboBox.setSelectedItem(browser.isBlank() ? INHERIT_BROWSER : browser);
        headlessCheckBox.setSelected(state != null && state.isHeadless());
        extraVmArgsField.setText(state == null || state.getExtraVmArgs() == null ? "" : state.getExtraVmArgs());
        updateFieldsEnabled();
    }
}
