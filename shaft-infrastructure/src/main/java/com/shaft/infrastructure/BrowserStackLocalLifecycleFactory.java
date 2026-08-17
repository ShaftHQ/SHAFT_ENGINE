package com.shaft.infrastructure;

@FunctionalInterface
interface BrowserStackLocalLifecycleFactory {
    BrowserStackLocalLifecycleService create(ShaftCachePaths paths, SetupPlan plan,
                                             BrowserStackLocalToolchainOperations operations);
}
