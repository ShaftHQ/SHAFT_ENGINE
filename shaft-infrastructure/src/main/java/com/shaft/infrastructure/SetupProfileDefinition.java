package com.shaft.infrastructure;

import java.util.List;
import java.util.Objects;
import java.util.HashSet;

/** Stable catalog metadata for one setup profile. */
public record SetupProfileDefinition(SetupProfile profile, String displayName, List<SetupTarget> targets) {
    public SetupProfileDefinition {
        Objects.requireNonNull(profile, "profile");
        if (displayName == null || displayName.isBlank()) {
            throw new IllegalArgumentException("Setup profile display name must not be blank.");
        }
        targets = List.copyOf(Objects.requireNonNull(targets, "targets"));
        if (targets.isEmpty()) {
            throw new IllegalArgumentException("Setup profile must contain at least one target.");
        }
        if (new HashSet<>(targets).size() != targets.size()) {
            throw new IllegalArgumentException("Setup profile must not contain duplicate targets.");
        }
    }
}
