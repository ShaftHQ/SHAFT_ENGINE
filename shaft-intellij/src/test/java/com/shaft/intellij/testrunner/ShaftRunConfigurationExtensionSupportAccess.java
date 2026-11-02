package com.shaft.intellij.testrunner;

import com.intellij.execution.configurations.RunConfigurationBase;
import org.jdom.Element;

/** Test-only bridge to the package-private run-configuration override store. */
public final class ShaftRunConfigurationExtensionSupportAccess {
    private ShaftRunConfigurationExtensionSupportAccess() {
    }

    /**
     * @param configuration run configuration
     * @param element       serialized SHAFT overrides
     */
    public static void readExternal(RunConfigurationBase<?> configuration, Element element) {
        ShaftRunConfigurationExtensionSupport.readExternal(configuration, element);
    }
}
