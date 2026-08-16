package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

/** Read-only host prerequisite boundary for desktop-mobile providers. */
interface DesktopMobileHostProbe {
    SetupStatus status(SetupAction action);

    default void requireReady(List<SetupAction> actions) throws IOException {
        for (SetupAction action : actions) {
            if (action.kind() != SetupActionKind.DIAGNOSE) continue;
            SetupStatus status = status(action);
            if (status.readiness() != SetupReadiness.READY) {
                throw new IOException(action.target() + " prerequisite is not ready: " + status.detail());
            }
        }
    }
}
