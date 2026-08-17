package com.shaft.infrastructure;

record BrowserStackLocalRuntimeLease(int schemaVersion, String planDigest, long pid, String binary,
                                     int refCount) {
    BrowserStackLocalRuntimeLease {
        if (schemaVersion != 1) throw new IllegalArgumentException("Unsupported BrowserStack Local lease schema.");
        if (planDigest == null || planDigest.isBlank()) throw new IllegalArgumentException("Plan digest is required.");
        if (pid < 1) throw new IllegalArgumentException("Owned process id must be positive.");
        if (binary == null || binary.isBlank()) throw new IllegalArgumentException("Binary path is required.");
        if (refCount < 1) throw new IllegalArgumentException("Lease refcount must be positive.");
    }

    BrowserStackLocalRuntimeLease withRefCount(int value) {
        return new BrowserStackLocalRuntimeLease(schemaVersion, planDigest, pid, binary, value);
    }

    boolean sameIdentity(BrowserStackLocalRuntimeLease other) {
        return other != null
                && planDigest.equals(other.planDigest)
                && sameRuntime(other);
    }

    boolean sameRuntime(BrowserStackLocalRuntimeLease other) {
        return other != null
                && pid == other.pid
                && binary.equals(other.binary);
    }
}
