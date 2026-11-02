package com.shaft.infrastructure;

@FunctionalInterface
interface ReportPortalOperationsFactory {
    ReportPortalToolchainOperations create(ShaftCachePaths paths, SetupPlan plan, boolean offline);
}
