package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

/** Injectable host and mutation boundary for the desktop-mobile setup transaction. */
interface DesktopMobileToolchainOperations {
    void hostPreflight(List<SetupAction> actions) throws IOException;

    default void preStatePreflight(List<SetupAction> actions, boolean offline) throws IOException { }

    void lockedPreflight(List<SetupAction> actions, boolean offline) throws IOException;

    void install(SetupAction action) throws IOException;

    SetupStatus status(SetupAction action);
}
