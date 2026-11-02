package com.shaft.infrastructure;

final class HealeniumRuntimeManager {
    private HealeniumRuntimeManager() {
        throw new IllegalStateException("Utility class");
    }

    static HealeniumLifecycleService systemLifecycle(ShaftCachePaths paths, SetupPlan plan,
                                                     HealeniumToolchainOperations operations) {
        return new HealeniumLifecycleService(paths, plan, operations);
    }
}
