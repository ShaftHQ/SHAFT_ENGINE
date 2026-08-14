package com.shaft.infrastructure;

import java.nio.file.Path;

/** Exact SHAFT-owned paths used by the managed Android runtime. */
record AndroidRuntimeLayout(Path nodeExecutable, Path appiumEntryPoint, Path appiumHome,
                            Path sdkRoot, Path adb, Path emulator, Path avdHome, Path avdRoot,
                            Path emulatorLog, Path appiumLog, String avdName, String serial) {
    static AndroidRuntimeLayout resolve(ShaftCachePaths paths, SetupPlatform platform,
                                        SetupArchitecture architecture, AndroidSetupRequest request) {
        String platformKey = platform.name().toLowerCase() + '-' + architecture.artifactName();
        Path nodeRoot = paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platformKey);
        Path node = platform == SetupPlatform.WINDOWS ? nodeRoot.resolve("node.exe")
                : nodeRoot.resolve("bin/node");
        Path appium = paths.tools().resolve("appium").resolve(AndroidSetupPlanner.APPIUM_VERSION);
        Path sdk = paths.tools().resolve("android-sdk").resolve(AndroidSetupPlanner.COMMAND_LINE_TOOLS_VERSION
                + "-api" + request.apiLevel() + '-' + request.abi());
        Path avdHome = paths.tools().resolve("android-avd");
        return new AndroidRuntimeLayout(node, appium.resolve("node_modules/appium/index.js"), appium,
                sdk, executable(sdk.resolve("platform-tools"), "adb", platform),
                executable(sdk.resolve("emulator"), "emulator", platform), avdHome,
                avdHome.resolve(request.avdName() + ".avd"),
                paths.state().resolve("logs/android-emulator.log"),
                paths.state().resolve("logs/appium-server.log"), request.avdName(), "emulator-5554");
    }

    private static Path executable(Path directory, String name, SetupPlatform platform) {
        return directory.resolve(platform == SetupPlatform.WINDOWS ? name + ".exe" : name);
    }
}
