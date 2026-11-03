package com.shaft.intellij.testrunner;

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.SimpleJavaParameters;
import org.jdom.Element;

/** Test-only bridge to the package-private run-configuration override store. */
public final class ShaftRunConfigurationExtensionSupportAccess {
    private ShaftRunConfigurationExtensionSupportAccess() {
    }

    public static void readExternal(RunConfigurationBase<?> configuration, Element element) {
        ShaftRunConfigurationExtensionSupport.readExternal(configuration, element);
    }

    public static void writeExternal(RunConfigurationBase<?> configuration, Element element) {
        ShaftRunConfigurationExtensionSupport.writeExternal(configuration, element);
    }

    public static void installOverrides(RunConfigurationBase<?> configuration, ShaftRunConfigurationOverrides overrides) {
        ShaftRunConfigurationExtensionSupport.installOverrides(configuration, overrides);
    }

    public static void applyOverrides(RunConfigurationBase<?> configuration, GeneralCommandLine commandLine) {
        ShaftRunConfigurationExtensionSupport.applyOverrides(configuration, commandLine);
    }

    public static void applyOverrides(RunConfigurationBase<?> configuration, SimpleJavaParameters javaParameters) {
        ShaftRunConfigurationExtensionSupport.applyOverrides(configuration, javaParameters);
    }

    public static boolean isApplicableFor(RunConfigurationBase<?> configuration) {
        return ShaftRunConfigurationExtensionSupport.isApplicableFor(configuration);
    }

    @SuppressWarnings("rawtypes")
    public static com.intellij.openapi.options.SettingsEditor createEditor() {
        return ShaftRunConfigurationExtensionSupport.createEditor();
    }
}
