package com.shaft.infrastructure;

@FunctionalInterface
interface HealeniumLifecycleFactory {
    HealeniumLifecycleService create(ShaftCachePaths paths, SetupPlan plan,
                                     HealeniumToolchainOperations operations);
}
