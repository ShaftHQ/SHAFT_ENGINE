package com.shaft.intellij.ui;

/**
 * Plain-language setup status for the tool window. One next action when a prerequisite is missing,
 * and a ready line when the agent can run.
 */
public final class SetupReadiness {
    private SetupReadiness() {
    }

    public static String message(boolean agentRuntimePresent, boolean credentialPresent, boolean projectReady) {
        if (!agentRuntimePresent) {
            return "Install the agent runtime, then press Check.";
        }
        if (!credentialPresent) {
            return "Add the agent credential in Settings, then press Check.";
        }
        if (!projectReady) {
            return "Open a project folder, then press Check.";
        }
        return "Agent ready.";
    }
}
