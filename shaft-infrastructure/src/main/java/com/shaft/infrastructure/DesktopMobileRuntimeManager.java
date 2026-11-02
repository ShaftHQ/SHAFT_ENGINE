package com.shaft.infrastructure;

import java.io.IOException;
import java.time.Duration;

/** Explicit CLI/API access to lease-safe iOS/Windows Appium stop and bounded logs. */
public final class DesktopMobileRuntimeManager {
    private DesktopMobileRuntimeManager() { }

    /**
     * Stops the SHAFT-owned mobile runtime started for the plan, waiting up to the timeout.
     *
     * @return whether a running runtime was stopped
     * @throws IOException when the runtime state cannot be read
     */
    public static boolean stop(ShaftCachePaths paths, SetupPlan plan, Duration timeout) throws IOException {
        return service(paths, plan).stop(timeout);
    }

    /**
     * Returns the logs of the SHAFT-owned mobile runtime started for the plan.
     *
     * @throws IOException when the logs cannot be read
     */
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
