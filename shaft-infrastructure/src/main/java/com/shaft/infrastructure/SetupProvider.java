package com.shaft.infrastructure;

import java.io.IOException;

/** Provider SPI used by Java, CLI, MCP, and IDE setup adapters. */
public interface SetupProvider {
    SetupProfile profile();

    SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture);

    SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture);

    SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException;

    default ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options)
            throws IOException {
        throw new UnsupportedOperationException("Profile " + profile() + " does not own a startable service.");
    }
}
