package com.shaft.infrastructure;

final class ReportPortalRuntimeManager {
    private ReportPortalRuntimeManager() {
        throw new IllegalStateException("Utility class");
    }

    static ReportPortalLifecycleService systemLifecycle(ShaftCachePaths paths, SetupPlan plan,
                                                        ReportPortalToolchainOperations operations) {
        return new ReportPortalLifecycleService(paths, plan, operations);
    }
}
