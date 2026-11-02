package com.shaft.infrastructure;

import java.util.Objects;
import java.util.Set;

/** Stable catalog metadata for one setup target. */
public record SetupTargetDefinition(
        SetupTarget target,
        String displayName,
        Set<SetupCapability> capabilities,
        String description) {

    public SetupTargetDefinition {
        Objects.requireNonNull(target, "target");
        capabilities = Set.copyOf(Objects.requireNonNull(capabilities, "capabilities"));
        if (capabilities.isEmpty()) {
            throw new IllegalArgumentException("Setup target must expose at least one capability.");
        }
        if (displayName == null || displayName.isBlank()) {
            throw new IllegalArgumentException("Setup target display name must not be blank.");
        }
        if (description == null || description.isBlank()) {
            throw new IllegalArgumentException("Setup target description must not be blank.");
        }
    }
}
