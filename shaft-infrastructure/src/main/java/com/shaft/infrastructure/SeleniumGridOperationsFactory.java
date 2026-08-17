package com.shaft.infrastructure;

@FunctionalInterface
interface SeleniumGridOperationsFactory {
    SeleniumGridToolchainOperations create(ShaftCachePaths paths, SetupPlan plan, boolean offline);
}
