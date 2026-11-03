package com.shaft.intellij.testrunner;

import com.intellij.ui.components.JBCheckBox;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBUI;

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import java.awt.BorderLayout;
import java.awt.GridLayout;

/**
 * Swing editor for {@link ShaftRunConfigurationOverrides} including lean target picker (W2.7).
 */
final class ShaftRunConfigurationOverridesPanel extends JPanel {
    static final String INHERIT_BROWSER = ShaftRunOverridesLabels.PROJECT_DEFAULT;
    private static final String[] BROWSERS = {INHERIT_BROWSER, "chrome", "firefox", "edge", "safari"};

    private final JBCheckBox enabledCheckBox =
            new JBCheckBox("Override SHAFT execution properties for this run");
    private final JComboBox<String> browserComboBox = new JComboBox<>(BROWSERS);
    private final JBCheckBox headlessCheckBox = new JBCheckBox("Headless");
    private final JComboBox<ShaftTargetSelection.Platform> platformCombo =
            new JComboBox<>(ShaftTargetSelection.Platform.values());
    private final JComboBox<ShaftTargetSelection.Execution> executionCombo =
            new JComboBox<>(ShaftTargetSelection.Execution.values());
    private final JBTextField deviceOsField = new JBTextField();
    private final JBTextField appAliasField = new JBTextField();
    private final JSpinner retrySpinner = new JSpinner(new SpinnerNumberModel(0, 0, 10, 1));
    private final JBTextField extraVmArgsField = new JBTextField();
    private final JLabel validationLabel = new JLabel(" ");

    ShaftRunConfigurationOverridesPanel() {
        super(new BorderLayout(4, 8));
        setBorder(JBUI.Borders.empty(8));

        enabledCheckBox.getAccessibleContext().setAccessibleName("Override SHAFT execution properties for this run");
        browserComboBox.getAccessibleContext().setAccessibleName("SHAFT browser override");
        headlessCheckBox.getAccessibleContext().setAccessibleName("SHAFT headless override");
        platformCombo.getAccessibleContext().setAccessibleName("SHAFT platform target");
        executionCombo.getAccessibleContext().setAccessibleName("SHAFT execution target");
        deviceOsField.getAccessibleContext().setAccessibleName("SHAFT device or OS");
        appAliasField.getAccessibleContext().setAccessibleName("SHAFT app alias");
        retrySpinner.getAccessibleContext().setAccessibleName("SHAFT retry count");
        extraVmArgsField.getAccessibleContext().setAccessibleName("SHAFT extra VM argument overrides");
        extraVmArgsField.setToolTipText(
                "Extra -D system property overrides applied on top of the picker, "
                        + "e.g. -DexecutionAddress=... -DmobileWebDriverName=...");
        deviceOsField.setToolTipText("Device/OS for mobile (ANDROID, iOS, or a cloud device name). Leave blank for Web.");
        appAliasField.setToolTipText("Cloud app alias from the registry. Required for cloud Android/iOS.");

        JPanel fields = new JPanel(new GridLayout(0, 1, 4, 4));
        fields.add(row("Browser", browserComboBox));
        fields.add(headlessCheckBox);
        fields.add(row("Platform", platformCombo));
        fields.add(row("Execution", executionCombo));
        fields.add(row("Device / OS", deviceOsField));
        fields.add(row("App alias", appAliasField));
        fields.add(row("Retries", retrySpinner));
        fields.add(row("Extra VM args", extraVmArgsField));
        fields.add(validationLabel);

        enabledCheckBox.addItemListener(event -> updateFieldsEnabled());
        platformCombo.addItemListener(event -> refreshValidation());
        executionCombo.addItemListener(event -> refreshValidation());
        deviceOsField.getDocument().addDocumentListener(simpleDocListener(this::refreshValidation));
        appAliasField.getDocument().addDocumentListener(simpleDocListener(this::refreshValidation));
        updateFieldsEnabled();
        refreshValidation();

        JLabel hint = new JLabel("Unset fields use " + ShaftRunOverridesLabels.projectDefaultLabel()
                + ". Invalid Platform × Execution × App combos are blocked before launch.");
        hint.setBorder(JBUI.Borders.emptyBottom(4));
        JPanel north = new JPanel(new BorderLayout());
        north.add(enabledCheckBox, BorderLayout.NORTH);
        north.add(hint, BorderLayout.SOUTH);
        add(north, BorderLayout.NORTH);
        add(fields, BorderLayout.CENTER);
    }

    private static javax.swing.event.DocumentListener simpleDocListener(Runnable action) {
        return new javax.swing.event.DocumentListener() {
            @Override public void insertUpdate(javax.swing.event.DocumentEvent e) { action.run(); }
            @Override public void removeUpdate(javax.swing.event.DocumentEvent e) { action.run(); }
            @Override public void changedUpdate(javax.swing.event.DocumentEvent e) { action.run(); }
        };
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
        platformCombo.setEnabled(enabled);
        executionCombo.setEnabled(enabled);
        deviceOsField.setEnabled(enabled);
        appAliasField.setEnabled(enabled);
        retrySpinner.setEnabled(enabled);
        extraVmArgsField.setEnabled(enabled);
        refreshValidation();
    }

    private void refreshValidation() {
        ShaftTargetSelection target = readTarget();
        var errors = target.validationErrors();
        validationLabel.setText(errors.isEmpty() ? " " : String.join(" ", errors));
    }

    private ShaftTargetSelection readTarget() {
        ShaftTargetSelection target = new ShaftTargetSelection();
        Object platform = platformCombo.getSelectedItem();
        Object execution = executionCombo.getSelectedItem();
        target.setPlatform(platform instanceof ShaftTargetSelection.Platform p ? p : ShaftTargetSelection.Platform.INHERIT_PLATFORM);
        target.setExecution(execution instanceof ShaftTargetSelection.Execution e ? e : ShaftTargetSelection.Execution.INHERIT_EXECUTION);
        target.setDeviceOs(deviceOsField.getText());
        target.setAppAlias(appAliasField.getText());
        target.setRetryCount((Integer) retrySpinner.getValue());
        return target;
    }

    ShaftRunConfigurationOverrides getState() {
        ShaftRunConfigurationOverrides state = new ShaftRunConfigurationOverrides();
        state.setEnabled(enabledCheckBox.isSelected());
        Object selectedBrowser = browserComboBox.getSelectedItem();
        String browser = selectedBrowser == null ? "" : selectedBrowser.toString();
        state.setBrowser(INHERIT_BROWSER.equals(browser) ? "" : browser);
        state.setHeadless(headlessCheckBox.isSelected());
        state.setExtraVmArgs(extraVmArgsField.getText());
        state.setTarget(readTarget());
        return state;
    }

    void setState(ShaftRunConfigurationOverrides state) {
        enabledCheckBox.setSelected(state != null && state.isEnabled());
        String browser = state == null || state.getBrowser() == null ? "" : state.getBrowser();
        browserComboBox.setSelectedItem(browser.isBlank() ? INHERIT_BROWSER : browser);
        headlessCheckBox.setSelected(state != null && state.isHeadless());
        extraVmArgsField.setText(state == null || state.getExtraVmArgs() == null ? "" : state.getExtraVmArgs());
        ShaftTargetSelection target = state == null ? new ShaftTargetSelection() : state.getTarget();
        platformCombo.setSelectedItem(target.getPlatform());
        executionCombo.setSelectedItem(target.getExecution());
        deviceOsField.setText(target.getDeviceOs());
        appAliasField.setText(target.getAppAlias());
        retrySpinner.setValue(target.getRetryCount());
        updateFieldsEnabled();
    }
}
