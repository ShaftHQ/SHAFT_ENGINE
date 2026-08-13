package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/** Exact managed Node/Lighthouse command resolved from a verified SHAFT receipt. */
public record LighthouseRuntime(List<String> commandPrefix, Path workingDirectory) {
    public LighthouseRuntime {
        commandPrefix = List.copyOf(java.util.Objects.requireNonNull(commandPrefix, "commandPrefix"));
        if (commandPrefix.isEmpty() || commandPrefix.stream().anyMatch(value -> value == null || value.isBlank())) {
            throw new IllegalArgumentException("Lighthouse command prefix must not be empty.");
        }
        workingDirectory = java.util.Objects.requireNonNull(workingDirectory, "workingDirectory")
                .toAbsolutePath().normalize();
    }

    /**
     * Resolves the release-pinned managed runtime without installing or changing the filesystem.
     *
     * @param options setup policy and owned paths
     * @return exact managed Node/Lighthouse command
     * @throws IOException when the approved managed runtime is not ready
     */
    public static LighthouseRuntime requireReady(SetupOptions options) throws IOException {
        SetupOptions selected = java.util.Objects.requireNonNull(options, "options").withProfile(SetupProfile.LIGHTHOUSE);
        SetupPlatform platform = SetupPlatform.current();
        SetupArchitecture architecture = SetupArchitecture.current();
        LighthouseSetupService service = LighthouseSetupProvider.service(selected, platform, architecture);
        SetupProfileStatus status = service.status();
        if (status.readiness() != SetupReadiness.READY) {
            throw new IOException("Managed Lighthouse is not ready. Run `shaft-cli setup plan --profile LIGHTHOUSE "
                    + "--mode MANAGED --output <absolute-plan.json>`, review its digest, then install it.");
        }
        Path nodeRoot = selected.paths().tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
        Path node = platform == SetupPlatform.WINDOWS ? nodeRoot.resolve("node.exe") : nodeRoot.resolve("bin/node");
        Path lighthouse = selected.paths().tools().resolve("lighthouse")
                .resolve(LighthouseSetupPlanner.LIGHTHOUSE_VERSION)
                .resolve("node_modules/lighthouse/cli/index.js");
        return new LighthouseRuntime(List.of(node.toString(), lighthouse.toString()), lighthouse.getParent());
    }
}
