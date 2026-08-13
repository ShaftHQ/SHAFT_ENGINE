package com.shaft.infrastructure;

import java.util.List;

/** Aggregate readiness returned by a profile provider. */
public record SetupProfileStatus(int schemaVersion, SetupProfile profile, SetupReadiness readiness,
                                 List<SetupStatus> targets) {
    public SetupProfileStatus {
        if (schemaVersion != 1) throw new IllegalArgumentException("Unsupported status schema: " + schemaVersion);
        java.util.Objects.requireNonNull(profile, "profile");
        java.util.Objects.requireNonNull(readiness, "readiness");
        targets = List.copyOf(java.util.Objects.requireNonNull(targets, "targets"));
    }
}
