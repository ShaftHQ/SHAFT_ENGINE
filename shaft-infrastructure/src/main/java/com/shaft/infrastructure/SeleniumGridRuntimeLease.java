package com.shaft.infrastructure;

record SeleniumGridRuntimeLease(int schemaVersion, String planDigest, String project, String endpoint,
                                int port, int chrome, int edge, int firefox, int refCount) {
    SeleniumGridRuntimeLease {
        if (schemaVersion != 1) throw new IllegalArgumentException("Unsupported Grid lease schema.");
        if (planDigest == null || planDigest.isBlank()) throw new IllegalArgumentException("Plan digest is required.");
        if (project == null || project.isBlank()) throw new IllegalArgumentException("Compose project is required.");
        if (endpoint == null || endpoint.isBlank()) throw new IllegalArgumentException("Endpoint is required.");
        if (refCount < 1) throw new IllegalArgumentException("Lease refcount must be positive.");
    }

    SeleniumGridRuntimeLease withRefCount(int value) {
        return new SeleniumGridRuntimeLease(schemaVersion, planDigest, project, endpoint, port, chrome, edge,
                firefox, value);
    }

    boolean sameIdentity(SeleniumGridRuntimeLease other) {
        return other != null
                && planDigest.equals(other.planDigest)
                && sameRuntime(other);
    }

    boolean sameRuntime(SeleniumGridRuntimeLease other) {
        return other != null
                && project.equals(other.project)
                && endpoint.equals(other.endpoint)
                && port == other.port
                && chrome == other.chrome
                && edge == other.edge
                && firefox == other.firefox;
    }
}
