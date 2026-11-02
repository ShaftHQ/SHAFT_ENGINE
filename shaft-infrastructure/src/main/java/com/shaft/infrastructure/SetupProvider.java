package com.shaft.infrastructure;

import java.io.IOException;
import java.util.function.Consumer;

/** Provider SPI used by Java, CLI, MCP, and IDE setup adapters. */
public interface SetupProvider {
    /**
     * Returns the setup profile this provider owns.
     */
    SetupProfile profile();

    /**
     * Plans the changes this provider needs for the options, platform and architecture.
     */
    SetupPlan plan(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture);

    /**
     * Plans the changes this provider needs for the selected components.
     */
    default SetupPlan plan(SetupOptions options, SetupSelection selection,
                           SetupPlatform platform, SetupArchitecture architecture) {
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile " + profile() + " does not accept component selection.");
        }
        return plan(options, platform, architecture);
    }

    /**
     * Plans the given operation for the selected components.
     */
    default SetupPlan plan(SetupOptions options, SetupSelection selection, SetupOperation operation,
                           SetupPlatform platform, SetupArchitecture architecture) {
        java.util.Objects.requireNonNull(operation, "operation");
        if (operation != SetupOperation.INSTALL) {
            throw new IllegalArgumentException("Profile " + profile() + " does not support " + operation + '.');
        }
        return plan(options, selection, platform, architecture);
    }

    /** Reconstructs the canonical selection embedded in an approved plan. */
    default SetupSelection selectionFromPlan(SetupPlan plan) {
        java.util.Objects.requireNonNull(plan, "plan");
        return SetupSelection.defaults();
    }

    /**
     * Reports the current state of this provider's setup.
     */
    SetupReport status(SetupOptions options, SetupPlatform platform, SetupArchitecture architecture);

    /**
     * Reports the current state of the selected components.
     */
    default SetupReport status(SetupOptions options, SetupSelection selection,
                               SetupPlatform platform, SetupArchitecture architecture) {
        if (!selection.components().isEmpty()) {
            throw new IllegalArgumentException("Profile " + profile() + " does not accept component selection.");
        }
        return status(options, platform, architecture);
    }

    /**
     * Installs an approved plan.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException;

    /**
     * Installs an approved plan, reporting progress as it runs.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    default SetupReceipt install(SetupPlan plan, SetupApproval approval, SetupOptions options,
                                 Consumer<SetupProgress> progress) throws IOException {
        java.util.Objects.requireNonNull(progress, "progress");
        return install(plan, approval, options);
    }

    /**
     * Installs an approved plan if needed and starts its services; providers without services only
     * install.
     *
     * @throws IOException when an artifact cannot be fetched or written
     */
    default ManagedEnvironment start(SetupPlan plan, SetupApproval approval, SetupOptions options)
            throws IOException {
        throw new UnsupportedOperationException("Profile " + profile() + " does not own a startable service.");
    }

    /**
     * Stops the exact SHAFT-owned service represented by an approved plan.
     *
     * @param plan exact provider plan
     * @param approval approval bound to {@code plan}
     * @param options execution policy and owned roots
     * @return {@code true} when an owned service was stopped
     * @throws IOException when an owned service cannot be stopped safely
     */
    default boolean stop(SetupPlan plan, SetupApproval approval, SetupOptions options) throws IOException {
        throw new UnsupportedOperationException("Profile " + profile() + " does not own a stoppable service.");
    }

    /**
     * Reads bounded logs owned by this provider without mutating the host.
     *
     * @param options execution policy and owned roots
     * @param selection exact profile selection
     * @param platform selected host platform
     * @param architecture selected host architecture
     * @return log text, or an empty string when no owned logs exist
     * @throws IOException when owned logs cannot be read safely
     */
    default String logs(SetupOptions options, SetupSelection selection, SetupPlatform platform,
                        SetupArchitecture architecture) throws IOException {
        throw new UnsupportedOperationException("Profile " + profile() + " does not expose owned logs.");
    }
}
