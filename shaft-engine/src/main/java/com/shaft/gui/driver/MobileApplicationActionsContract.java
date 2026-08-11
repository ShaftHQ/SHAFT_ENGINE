package com.shaft.gui.driver;

import java.nio.file.Path;
import java.time.Duration;

/** Mobile application lifecycle actions. */
public interface MobileApplicationActionsContract {
    /** Installs an app from a server-visible path or URL. */
    MobileApplicationActionsContract install(String appPathOrUrl);

    /** Installs an app from an existing local path. */
    MobileApplicationActionsContract install(Path appPath);

    /** Returns whether the app identified by package or bundle ID is installed. */
    boolean isInstalled(String appId);

    /** Returns the native lifecycle state for an app package or bundle ID. */
    MobileApplicationState state(String appId);

    /** Activates an installed app by package or bundle ID. */
    MobileApplicationActionsContract activate(String appId);

    /** Terminates an app and returns the native provider result. */
    boolean terminate(String appId);

    /** Removes an installed app and returns the native provider result. */
    boolean remove(String appId);

    /** Sends the current app to the background for the requested duration. */
    MobileApplicationActionsContract background(Duration duration);

    /** Launches the app configured for the current Windows Appium session. */
    MobileApplicationActionsContract launchConfiguredApp();

    /** Closes the app configured for the current Windows Appium session. */
    MobileApplicationActionsContract closeConfiguredApp();

    /** Returns the owning categorized mobile facade. */
    MobileActionsContract and();
}
