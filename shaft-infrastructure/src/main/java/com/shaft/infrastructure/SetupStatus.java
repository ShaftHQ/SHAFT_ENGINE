package com.shaft.infrastructure;

/** Read-only readiness result for one independently versioned target. */
public record SetupStatus(SetupTarget target, SetupReadiness readiness, String detectedVersion, String detail) {
    public SetupStatus {
        java.util.Objects.requireNonNull(target, "target");
        java.util.Objects.requireNonNull(readiness, "readiness");
        detectedVersion = detectedVersion == null ? "" : detectedVersion;
        detail = detail == null ? "" : detail;
    }
}
