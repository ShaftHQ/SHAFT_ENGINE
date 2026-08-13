package com.shaft.infrastructure;

import java.net.URI;
import java.util.Objects;
import java.util.Set;

/** Fully bound setup operation used for review, digesting, and execution. */
public record SetupAction(SetupTarget target, SetupActionKind kind, String version, URI source,
                          String checksum, boolean privileged, Set<String> requiredLicenses) {
    public SetupAction {
        Objects.requireNonNull(target, "target");
        Objects.requireNonNull(kind, "kind");
        Objects.requireNonNull(source, "source");
        if (version == null || version.isBlank()) throw new IllegalArgumentException("Version must not be blank.");
        if (checksum == null || !checksum.matches("sha256:[0-9a-fA-F]{64}")) {
            throw new IllegalArgumentException("Checksum must be a SHA-256 digest.");
        }
        requiredLicenses = Set.copyOf(Objects.requireNonNull(requiredLicenses, "requiredLicenses"));
        if (requiredLicenses.stream().anyMatch(license -> license == null || license.isBlank())) {
            throw new IllegalArgumentException("License identifiers must not be blank.");
        }
    }
}
