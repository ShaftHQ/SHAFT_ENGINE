package com.shaft.intellij.testrunner;

/**
 * Plain per-run-configuration state for SHAFT execution property overrides (issue #3659): whether
 * an individual TestNG/JUnit run configuration overrides the project's {@code custom.properties}
 * {@code targetBrowserName}/{@code headlessExecution} settings, and any extra {@code -D} VM-arg
 * overrides. A pure state holder -- no SDK imports -- so {@link ShaftRunConfigurationOverridesPanel}
 * and {@link ShaftRunConfigurationExtensionSupport} (shared by both the JUnit and TestNG
 * extensions) can all depend on it without pulling in either optional SDK dependency.
 * <p>
 * Default state ({@code enabled == false}) means "untouched, inherits {@code custom.properties}"
 * -- the required default for every run configuration that never opts in.
 */
public final class ShaftRunConfigurationOverrides {
    private boolean enabled;
    private String browser;
    private boolean headless;
    private String extraVmArgs;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /** Blank/null means "inherit from custom.properties". */
    public String getBrowser() {
        return browser;
    }

    public void setBrowser(String browser) {
        this.browser = browser;
    }

    public boolean isHeadless() {
        return headless;
    }

    public void setHeadless(boolean headless) {
        this.headless = headless;
    }

    /** Raw, whitespace-delimited extra VM argument overrides (e.g. {@code -Dkey=value ...}). */
    public String getExtraVmArgs() {
        return extraVmArgs;
    }

    public void setExtraVmArgs(String extraVmArgs) {
        this.extraVmArgs = extraVmArgs;
    }
}
