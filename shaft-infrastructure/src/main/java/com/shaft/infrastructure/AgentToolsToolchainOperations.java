package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

/** Injectable host probe and catalog write boundary for agent tools. */
interface AgentToolsToolchainOperations {
    void hostPreflight(List<SetupAction> actions) throws IOException;

    default void preStatePreflight(List<SetupAction> actions, boolean offline) throws IOException {
        java.util.Objects.requireNonNull(actions, "actions");
        if (offline) return;
    }

    void lockedPreflight(List<SetupAction> actions, boolean offline) throws IOException;

    void install(SetupAction action) throws IOException;

    SetupStatus status(SetupAction action);
}
