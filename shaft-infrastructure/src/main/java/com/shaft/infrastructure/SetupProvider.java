package com.shaft.infrastructure;

import java.io.IOException;
import java.util.function.Consumer;

/** Provider SPI used by Java, CLI, MCP, and IDE setup adapters. */
public interface SetupProvider {
    SetupProfile profile();

    SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture);

    default SetupPlan plan(SetupOptions options, SetupSelection selection,
                           SetupPlatform platform, SetupArchitecture architecture) {
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile " + profile() + " does not accept component selection.");
        }
        return plan(options, platform, architecture);
    }

    SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture);

    default SetupReport status(SetupOptions options, SetupSelection selection,
                               SetupPlatform platform, SetupArchitecture architecture) {
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile " + profile() + " does not accept component selection.");
        }
        return status(options, platform, architecture);
    }

    SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException;

    default SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                 Consumer<SetupProgress> progress) throws IOException {
        java.util.Objects.requireNonNull(progress, "progress");
        return install(plan, approval, options);
    }

    default ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options)
            throws IOException {
        throw new UnsupportedOperationException("Profile " + profile() + " does not own a startable service.");
    }
}
