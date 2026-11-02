package com.shaft.infrastructure;

import java.nio.file.Path;

/** Exact SHAFT-owned paths used by the managed iOS/Windows Appium runtime. */
record DesktopMobileRuntimeLayout(Path nodeExecutable, Path appiumEntryPoint, Path appiumHome, Path appiumLog) {
    static DesktopMobileRuntimeLayout resolve(ShaftCachePaths paths, SetupPlan plan) {
        String platformKey = plan.platform().name().toLowerCase() + '-' + plan.architecture().artifactName();
        Path nodeRoot = paths.tools().resolve("node").resolve(ReportingSetupPlanner.NODE_VERSION)
                .resolve(platformKey);
        Path node = plan.platform() == SetupPlatform.WINDOWS ? nodeRoot.resolve("node.exe")
                : nodeRoot.resolve("bin/node");
        String profile = plan.profile() == SetupProfile.MOBILE_IOS ? "appium-ios" : "appium-windows";
        Path appium = paths.tools().resolve(profile).resolve(AndroidSetupPlanner.APPIUM_VERSION);
        return new DesktopMobileRuntimeLayout(node, appium.resolve("node_modules/appium/index.js"), appium,
                paths.state().resolve("logs/" + profile + ".log"));
    }
}
