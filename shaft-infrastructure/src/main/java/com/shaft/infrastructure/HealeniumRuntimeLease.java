package com.shaft.infrastructure;

record HealeniumRuntimeLease(int schemaVersion, String planDigest, String project, String endpoint,
                             int backendPort, int imitatePort, int refCount) {
    HealeniumRuntimeLease {
        if (schemaVersion != 1) throw new IllegalArgumentException("Unsupported Healenium lease schema.");
        if (planDigest == null || planDigest.isBlank()) throw new IllegalArgumentException("Plan digest is required.");
        if (project == null || project.isBlank()) throw new IllegalArgumentException("Compose project is required.");
        if (endpoint == null || endpoint.isBlank()) throw new IllegalArgumentException("Endpoint is required.");
        if (refCount < 1) throw new IllegalArgumentException("Lease refcount must be positive.");
    }

    HealeniumRuntimeLease withRefCount(int value) {
        return new HealeniumRuntimeLease(schemaVersion, planDigest, project, endpoint, backendPort, imitatePort,
                value);
    }

    boolean sameIdentity(HealeniumRuntimeLease other) {
        return other != null
                && planDigest.equals(other.planDigest)
                && sameRuntime(other);
    }

    boolean sameRuntime(HealeniumRuntimeLease other) {
        return other != null
                && project.equals(other.project)
                && endpoint.equals(other.endpoint)
                && backendPort == other.backendPort
                && imitatePort == other.imitatePort;
    }
}
