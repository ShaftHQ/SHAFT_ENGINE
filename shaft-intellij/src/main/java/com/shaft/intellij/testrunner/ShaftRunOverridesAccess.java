package com.shaft.intellij.testrunner;

import com.intellij.execution.configurations.RunConfigurationBase;

/**
 * Public entry point for installing per-run SHAFT overrides from outside this package
 * (Tests panel multi-browser / properties-profile launches).
 */
public final class ShaftRunOverridesAccess {
    private ShaftRunOverridesAccess() {
    }

    public static void install(RunConfigurationBase<?> configuration, ShaftRunConfigurationOverrides overrides) {
        ShaftRunConfigurationExtensionSupport.installOverrides(configuration, overrides);
    }
}
