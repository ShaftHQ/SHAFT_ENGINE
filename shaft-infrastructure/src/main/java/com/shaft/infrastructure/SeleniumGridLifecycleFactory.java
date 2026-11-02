package com.shaft.infrastructure;

@FunctionalInterface
interface SeleniumGridLifecycleFactory {
    SeleniumGridLifecycleService create(ShaftCachePaths paths, SetupPlan plan,
                                        SeleniumGridToolchainOperations operations);
}
