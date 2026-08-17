package com.shaft.infrastructure;

record ReportPortalRuntimeLease(int schemaVersion, String planDigest, String project, String endpoint,
                                int uiPort, int refCount) {
    ReportPortalRuntimeLease {
        if (schemaVersion != 1) throw new IllegalArgumentException("Unsupported ReportPortal lease schema.");
        if (planDigest == null || planDigest.isBlank()) throw new IllegalArgumentException("Plan digest is required.");
        if (project == null || project.isBlank()) throw new IllegalArgumentException("Compose project is required.");
        if (endpoint == null || endpoint.isBlank()) throw new IllegalArgumentException("Endpoint is required.");
        if (refCount < 1) throw new IllegalArgumentException("Lease refcount must be positive.");
    }

    ReportPortalRuntimeLease withRefCount(int value) {
        return new ReportPortalRuntimeLease(schemaVersion, planDigest, project, endpoint, uiPort, value);
    }

    boolean sameIdentity(ReportPortalRuntimeLease other) {
        return other != null
                && planDigest.equals(other.planDigest)
                && sameRuntime(other);
    }

    boolean sameRuntime(ReportPortalRuntimeLease other) {
        return other != null
                && project.equals(other.project)
                && endpoint.equals(other.endpoint)
                && uiPort == other.uiPort;
    }
}
