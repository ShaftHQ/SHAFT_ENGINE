package com.shaft.infrastructure;

import java.io.IOException;
import java.util.List;

/** Injectable mutation boundary used by the Android setup transaction coordinator. */
interface AndroidToolchainOperations {
    void preflight(List<SetupAction> actions, boolean offline) throws IOException;

    void install(SetupAction action) throws IOException;

    SetupStatus status(SetupAction action);
}
