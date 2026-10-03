package com.shaft.intellij.settings;

/** Test-only bridge to the package-private upgrade scheduler. */
public final class ShaftPluginUpgradeActivityAccess {
    private ShaftPluginUpgradeActivityAccess() {
    }

    /**
     * @param check work to schedule
     * @return pending work
     */
    public static java.util.concurrent.Future<?> schedule(Runnable check) {
        return ShaftPluginUpgradeActivity.schedule(check);
    }
}
