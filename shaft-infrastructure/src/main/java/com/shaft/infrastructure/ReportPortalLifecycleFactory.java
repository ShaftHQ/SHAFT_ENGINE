package com.shaft.infrastructure;

@FunctionalInterface
interface ReportPortalLifecycleFactory {
    ReportPortalLifecycleService create(ShaftCachePaths paths, SetupPlan plan,
                                        ReportPortalToolchainOperations operations);
}
