package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;

/** Read-only resolver for a receipt-bound managed Playwright browser installation. */
public final class PlaywrightRuntime {
    private PlaywrightRuntime() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Resolves the verified browser root without installing or repairing anything.
     *
     * @param options managed Playwright setup options
     * @return absolute SHAFT-owned Playwright browser root
     * @throws IllegalStateException when the exact managed receipt and payload are not ready
     */
    public static Path resolveBrowserRoot(SetupOptions options) {
        SetupOptions selected = Objects.requireNonNull(options, "options");
        if (selected.profile() != SetupProfile.PLAYWRIGHT || selected.effectiveMode() != SetupMode.MANAGED) {
            throw new IllegalArgumentException("Managed Playwright runtime resolution requires profile PLAYWRIGHT in MANAGED mode.");
        }
        SetupPlatform platform = SetupPlatform.current();
        SetupArchitecture architecture = SetupArchitecture.current();
        SetupReport report;
        try {
            report = InfrastructureSetupService.builtIn(platform, architecture).status(selected);
        } catch (IllegalArgumentException failure) {
            throw unavailable(failure.getMessage());
        }
        Path root = PlaywrightSetupService.browserRoot(selected.paths(), platform, architecture)
                .toAbsolutePath().normalize();
        try {
            VerifiedArtifactStore.requireUnlinkedAncestors(root);
        } catch (IOException failure) {
            throw unavailable(failure.getMessage());
        }
        if (report.readiness() != SetupReadiness.READY
                || !Files.isDirectory(root, java.nio.file.LinkOption.NOFOLLOW_LINKS)) {
            throw unavailable("managed browser payload is " + report.readiness());
        }
        return root;
    }

    private static IllegalStateException unavailable(String detail) {
        return new IllegalStateException("Managed PLAYWRIGHT browsers are not ready (" + detail
                + "). Run `shaft setup plan --profile PLAYWRIGHT --mode MANAGED`, review and install the exact plan, then verify it.");
    }
}
