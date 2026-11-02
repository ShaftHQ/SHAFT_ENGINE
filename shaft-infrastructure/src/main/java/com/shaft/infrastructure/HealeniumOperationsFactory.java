package com.shaft.infrastructure;

@FunctionalInterface
interface HealeniumOperationsFactory {
    HealeniumToolchainOperations create(ShaftCachePaths paths, SetupPlan plan, boolean offline);
}
