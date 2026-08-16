package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

/** Injectable host and mutation boundary for the desktop-mobile setup transaction. */
interface DesktopMobileToolchainOperations {
    void hostPreflight(List<SetupAction> actions) throws IOException;

    default void preStatePreflight(List<SetupAction> actions, boolean offline) throws IOException {
        // Most providers have no safe read that must occur before the shared state directory exists.
    }

    void lockedPreflight(List<SetupAction> actions, boolean offline) throws IOException;

    void install(SetupAction action) throws IOException;

    SetupStatus status(SetupAction action);
}
