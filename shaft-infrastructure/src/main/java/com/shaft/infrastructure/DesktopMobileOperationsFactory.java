package com.shaft.infrastructure;

/** Testable construction boundary for profile-owned desktop-mobile operations. */
@FunctionalInterface
interface DesktopMobileOperationsFactory {
    DesktopMobileToolchainOperations create(ShaftCachePaths paths, SetupPlan plan, boolean offline);
}
