package com.shaft.infrastructure;

final class SeleniumGridRuntimeManager {
    private SeleniumGridRuntimeManager() {
        throw new IllegalStateException("Utility class");
    }

    static SeleniumGridLifecycleService systemLifecycle(ShaftCachePaths paths, SetupPlan plan,
                                                        SeleniumGridToolchainOperations operations) {
        return new SeleniumGridLifecycleService(paths, plan, operations);
    }
}
