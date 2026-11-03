package com.shaft.intellij.testrunner;

/**
 * Plain per-run-configuration state for SHAFT execution property overrides (issue #3659 / #7591):
 * whether an individual TestNG/JUnit run configuration overrides the project's {@code custom.properties}
 * settings, browser/headless, target picker (Platform/Execution/Device/App), and any extra {@code -D}
 * VM-arg overrides. A pure state holder -- no SDK imports.
 */
public final class ShaftRunConfigurationOverrides {
    private boolean enabled;
    private String browser;
    private boolean headless;
    private String extraVmArgs;
    private ShaftTargetSelection target = new ShaftTargetSelection();

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

    public ShaftTargetSelection getTarget() {
        if (target == null) {
            target = new ShaftTargetSelection();
        }
        return target;
    }

    public void setTarget(ShaftTargetSelection target) {
        this.target = target == null ? new ShaftTargetSelection() : target;
    }
}
