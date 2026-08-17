package com.shaft.infrastructure;

final class BrowserStackLocalRuntimeManager {
    private BrowserStackLocalRuntimeManager() {
        throw new IllegalStateException("Utility class");
    }

    static BrowserStackLocalLifecycleService systemLifecycle(ShaftCachePaths paths, SetupPlan plan,
                                                             BrowserStackLocalToolchainOperations operations) {
        return new BrowserStackLocalLifecycleService(paths, plan, operations);
    }
}
