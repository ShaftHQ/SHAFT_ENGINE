package com.shaft.infrastructure;

/** Testable construction boundary for iOS/Windows Appium runtime ownership. */
@FunctionalInterface
interface DesktopMobileLifecycleFactory {
    DesktopMobileLifecycleService create(ShaftCachePaths paths, SetupPlan plan,
                                         DesktopMobileToolchainOperations operations);
}
