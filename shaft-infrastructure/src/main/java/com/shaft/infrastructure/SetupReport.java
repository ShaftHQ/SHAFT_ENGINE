package com.shaft.infrastructure;

import java.util.List;
import java.util.Objects;

/** Stable provider-neutral diagnosis and verification result. */
public record SetupReport(int schemaVersion, SetupProfile profile, SetupReadiness readiness,
                          List<SetupStatus> targets, List<String> diagnostics) {
    public SetupReport {
        if (schemaVersion != 1) throw new IllegalArgumentException("Unsupported report schema: " + schemaVersion);
        Objects.requireNonNull(profile, "profile");
        Objects.requireNonNull(readiness, "readiness");
        targets = List.copyOf(Objects.requireNonNull(targets, "targets"));
        diagnostics = List.copyOf(Objects.requireNonNull(diagnostics, "diagnostics"));
        if (targets.stream().anyMatch(Objects::isNull) || diagnostics.stream().anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("Report entries must not be null.");
        }
    }

    public static SetupReport from(SetupProfileStatus status) {
        Objects.requireNonNull(status, "status");
        return new SetupReport(1, status.profile(), status.readiness(), status.targets(), List.of());
    }
}
