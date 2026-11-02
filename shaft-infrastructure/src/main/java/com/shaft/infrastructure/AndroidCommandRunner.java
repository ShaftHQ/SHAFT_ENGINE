package com.shaft.infrastructure;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Set;

/** Sanitized child-process seam for Appium, sdkmanager, avdmanager, adb, and Emulator. */
@FunctionalInterface
interface AndroidCommandRunner {
    ReportingSetupService.ProcessResult run(List<String> command, Path workingDirectory,
                                            Map<String, String> environment, Set<String> removedEnvironment,
                                            String standardInput, Path log, Duration timeout) throws IOException;

    static AndroidCommandRunner system(ShaftCachePaths paths, SetupPlatform platform,
                                       SetupArchitecture architecture) {
        Path nodeRoot = paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platform.name().toLowerCase() + '-' + architecture.artifactName());
        return (command, workingDirectory, environment, removedEnvironment, standardInput, log, timeout) ->
                ReportingSetupService.runProcess(command, log, timeout, paths.cacheRoot(), nodeRoot,
                        workingDirectory, environment, removedEnvironment, standardInput);
    }
}
