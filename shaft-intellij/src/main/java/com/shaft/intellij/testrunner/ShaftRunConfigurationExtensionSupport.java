package com.shaft.intellij.testrunner;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.JavaParameters;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.util.Key;
import com.shaft.intellij.project.ShaftProjectDetector;
import org.jdom.Element;

import javax.swing.JComponent;

/**
 * Shared logic for {@link ShaftJUnitRunConfigurationExtension} and
 * {@link ShaftTestNgRunConfigurationExtension} (issue #3659): one implementation of the "SHAFT"
 * run-configuration-editor tab, reused across both optional SDK dependencies.
 * <p>
 * {@code RunConfigurationExtension} is declared as {@code RunConfigurationExtensionBase<}{@link
 * RunConfigurationBase}{@code <?>>} -- fixed, not re-parameterized per concrete extension (verified
 * against the bytecode of both classes and confirmed by the bundled {@code
 * CoverageJavaRunConfigurationExtension}, which is registered the same raw-typed way for every
 * config type and narrows only through {@code isApplicableFor}). So every method here takes the
 * raw/wildcard {@link RunConfigurationBase}; the JUnit/TestNG extensions narrow via
 * {@code instanceof} in their own {@code isApplicableFor}.
 * <p>
 * Per-configuration override state rides on {@link RunConfigurationBase}'s inherited <em>copyable</em>
 * user data (it extends {@code UserDataHolderBase}) -- the same mechanism {@code
 * JavaCoverageEnabledConfiguration}/{@code CoverageEnabledConfiguration.getOrCreate} uses via
 * {@code putCopyableUserData}/{@code getCopyableUserData}, confirmed by reading that class's real
 * bytecode. Copyable user data (unlike plain user data) survives "Copy Configuration", matching
 * what a user duplicating a SHAFT run configuration would expect.
 */
final class ShaftRunConfigurationExtensionSupport {
    static final String SERIALIZATION_ID = "shaft";
    static final String EDITOR_TITLE = "SHAFT";

    private static final Key<ShaftRunConfigurationOverrides> OVERRIDES_KEY =
            Key.create("com.shaft.intellij.testrunner.SHAFT_RUN_CONFIGURATION_OVERRIDES");

    private static final String ENABLED_ATTRIBUTE = "enabled";
    private static final String BROWSER_ATTRIBUTE = "browser";
    private static final String HEADLESS_ATTRIBUTE = "headless";
    private static final String EXTRA_VM_ARGS_ATTRIBUTE = "extraVmArgs";

    // custom.properties keys these overrides patch (shaft-engine/src/main/resources/properties/custom.properties).
    private static final String BROWSER_PROPERTY = "targetBrowserName";
    private static final String HEADLESS_PROPERTY = "headlessExecution";

    private ShaftRunConfigurationExtensionSupport() {
        throw new IllegalStateException("Utility class");
    }

    static boolean isApplicableFor(RunConfigurationBase<?> configuration) {
        return configuration != null && ShaftProjectDetector.isShaftProject(configuration.getProject());
    }

    @SuppressWarnings("rawtypes")
    static SettingsEditor createEditor() {
        return new Editor();
    }

    static void readExternal(RunConfigurationBase<?> configuration, Element element) {
        String enabledValue = element.getAttributeValue(ENABLED_ATTRIBUTE);
        if (enabledValue == null) {
            // No SHAFT element written for this configuration: untouched, inherits custom.properties.
            configuration.putCopyableUserData(OVERRIDES_KEY, null);
            return;
        }
        ShaftRunConfigurationOverrides overrides = new ShaftRunConfigurationOverrides();
        overrides.setEnabled(Boolean.parseBoolean(enabledValue));
        overrides.setBrowser(element.getAttributeValue(BROWSER_ATTRIBUTE, ""));
        overrides.setHeadless(Boolean.parseBoolean(element.getAttributeValue(HEADLESS_ATTRIBUTE, "false")));
        overrides.setExtraVmArgs(element.getAttributeValue(EXTRA_VM_ARGS_ATTRIBUTE, ""));
        configuration.putCopyableUserData(OVERRIDES_KEY, overrides);
    }

    static void writeExternal(RunConfigurationBase<?> configuration, Element element) {
        ShaftRunConfigurationOverrides overrides = configuration.getCopyableUserData(OVERRIDES_KEY);
        if (overrides == null || !overrides.isEnabled()) {
            // Default: nothing written, matching every other run configuration that never opted in.
            return;
        }
        element.setAttribute(ENABLED_ATTRIBUTE, String.valueOf(overrides.isEnabled()));
        element.setAttribute(BROWSER_ATTRIBUTE, nullToEmpty(overrides.getBrowser()));
        element.setAttribute(HEADLESS_ATTRIBUTE, String.valueOf(overrides.isHeadless()));
        element.setAttribute(EXTRA_VM_ARGS_ATTRIBUTE, nullToEmpty(overrides.getExtraVmArgs()));
    }

    /** Shared apply routine for {@code patchCommandLine} (raw process command-line arguments). */
    static void applyOverrides(RunConfigurationBase<?> configuration, GeneralCommandLine commandLine) {
        ShaftRunConfigurationOverrides overrides = configuration.getCopyableUserData(OVERRIDES_KEY);
        if (overrides == null || !overrides.isEnabled()) {
            return;
        }
        if (!isBlank(overrides.getBrowser())) {
            commandLine.addParameter(systemProperty(BROWSER_PROPERTY, overrides.getBrowser()));
        }
        commandLine.addParameter(systemProperty(HEADLESS_PROPERTY, String.valueOf(overrides.isHeadless())));
        if (!isBlank(overrides.getExtraVmArgs())) {
            for (String token : ParametersList.parse(overrides.getExtraVmArgs())) {
                commandLine.addParameter(token);
            }
        }
    }

    /** Shared apply routine for {@code updateJavaParameters} (the JVM's {@code -D} parameters list). */
    static void applyOverrides(RunConfigurationBase<?> configuration, JavaParameters javaParameters) {
        ShaftRunConfigurationOverrides overrides = configuration.getCopyableUserData(OVERRIDES_KEY);
        if (overrides == null || !overrides.isEnabled()) {
            return;
        }
        ParametersList vmParametersList = javaParameters.getVMParametersList();
        if (!isBlank(overrides.getBrowser())) {
            vmParametersList.addProperty(BROWSER_PROPERTY, overrides.getBrowser());
        }
        vmParametersList.addProperty(HEADLESS_PROPERTY, String.valueOf(overrides.isHeadless()));
        if (!isBlank(overrides.getExtraVmArgs())) {
            vmParametersList.addParametersString(overrides.getExtraVmArgs());
        }
    }

    private static String systemProperty(String key, String value) {
        return "-D" + key + "=" + value;
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }

    private static String nullToEmpty(String value) {
        return value == null ? "" : value;
    }

    /**
     * Backs both extensions' {@code createEditor}. A private nested class (rather than a sixth
     * source file) since it is pure wiring around {@link ShaftRunConfigurationOverridesPanel} with
     * no JUnit/TestNG-specific imports of its own.
     */
    @SuppressWarnings("rawtypes")
    private static final class Editor extends SettingsEditor<RunConfigurationBase> {
        private final ShaftRunConfigurationOverridesPanel panel = new ShaftRunConfigurationOverridesPanel();

        @Override
        protected void resetEditorFrom(RunConfigurationBase configuration) {
            panel.setState(configuration.getCopyableUserData(OVERRIDES_KEY));
        }

        @Override
        protected void applyEditorTo(RunConfigurationBase configuration) {
            ShaftRunConfigurationOverrides state = panel.getState();
            configuration.putCopyableUserData(OVERRIDES_KEY, state.isEnabled() ? state : null);
        }

        @Override
        protected JComponent createEditor() {
            return panel;
        }
    }
}
