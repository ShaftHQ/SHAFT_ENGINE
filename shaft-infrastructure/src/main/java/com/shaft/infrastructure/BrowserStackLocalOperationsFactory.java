package com.shaft.infrastructure;

@FunctionalInterface
interface BrowserStackLocalOperationsFactory {
    BrowserStackLocalToolchainOperations create(ShaftCachePaths paths, SetupPlan plan, boolean offline);
}
