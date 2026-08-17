package com.shaft.infrastructure;

import java.io.IOException;
import java.time.Duration;

/** Explicit CLI/API access to lease-safe iOS/Windows Appium stop and bounded logs. */
public final class DesktopMobileRuntimeManager {
    private DesktopMobileRuntimeManager() { }

    public static boolean stop(ShaftCachePaths paths, SetupPlan plan, Duration timeout) throws IOException {
        return service(paths, plan).stop(timeout);
    }

    public static String logs(ShaftCachePaths paths, SetupPlan plan) throws IOException {
        return service(paths, plan).logs();
    }

    private static DesktopMobileLifecycleService service(ShaftCachePaths paths, SetupPlan plan) {
        DesktopMobileToolchainOperations operations = new DefaultDesktopMobileToolchainOperations(paths, plan, true);
        return systemLifecycle(paths, plan, operations);
    }

    static DesktopMobileLifecycleService systemLifecycle(ShaftCachePaths paths, SetupPlan plan,
                                                         DesktopMobileToolchainOperations operations) {
        DesktopMobileDeviceController devices = new SystemDesktopMobileDeviceController(plan, paths);
        return new DesktopMobileLifecycleService(paths, plan, operations, new SystemAndroidRuntimeController(),
                devices, new SystemDesktopMobileRuntimeHealth(paths, plan, devices));
    }
}
