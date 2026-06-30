package com.shaft.sikulix;

import com.shaft.driver.SHAFT;
import com.shaft.gui.element.SikuliActions;
import com.shaft.tools.io.ReportManager;
import org.sikuli.script.App;

/**
 * Driver wrapper for attaching SHAFT SikuliX actions to a desktop application window.
 */
@SuppressWarnings("unused")
public class SikuliDriver {
    private final App sikuliApp;

    /**
     * Attaches to an opened desktop application window by name or partial name.
     *
     * @param applicationName name or partial name of the target application window
     */
    public SikuliDriver(String applicationName) {
        sikuliApp = new App(applicationName);
        sikuliApp.waitForWindow(SHAFT.Properties.timeouts.browserNavigationTimeout());
        sikuliApp.focus();
        ReportManager.log("Opened SikuliX app: [" + sikuliApp.getName() + "].");
    }

    /**
     * Creates a SikuliX driver attached to an opened desktop application window.
     *
     * @param applicationName name or partial name of the target application window
     * @return a SikuliX driver instance
     */
    public static SikuliDriver getInstance(String applicationName) {
        return new SikuliDriver(applicationName);
    }

    /**
     * Closes the attached desktop application window.
     */
    public void quit() {
        if (sikuliApp != null) {
            ReportManager.log("Closing SikuliX app: [" + sikuliApp.getName() + "].");
            sikuliApp.close();
        }
    }

    /**
     * Creates a SHAFT SikuliX element action facade scoped to the attached application.
     *
     * @return SikuliX action facade
     */
    public SikuliActions element() {
        return new SikuliActions(sikuliApp);
    }

    /**
     * Returns the native SikuliX app handle.
     *
     * @return SikuliX app handle
     */
    public App getDriver() {
        return sikuliApp;
    }
}
