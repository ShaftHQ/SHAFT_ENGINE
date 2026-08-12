package com.shaft.gui.driver;

import com.shaft.tools.io.trace.TraceArtifactReference;

import java.nio.file.Path;
import java.time.Instant;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Immutable descriptor of one safely published, bounded mobile evidence archive. */
public record MobileEvidenceBundle(
        Instant capturedAt,
        Path archive,
        String context,
        Map<String, String> applicationMetadata,
        Map<String, String> deviceMetadata,
        List<MobileLogMessage> logMessages,
        List<MobileLogError> logErrors,
        List<MobilePerformanceSample> performanceSamples,
        List<TraceArtifactReference> artifacts,
        Map<String, String> omissions) {
    private static final Set<String> APPLICATION_METADATA_KEYS = Set.of(
            "appPackage", "appActivity", "bundleId", "applicationState");
    private static final Set<String> DEVICE_METADATA_KEYS = Set.of(
            "platformName", "platformVersion", "automationName", "orientation", "windowSize");
    private static final Set<String> OMISSION_KEYS = Set.of(
            "screenshot", "source", "logs", "logErrors", "performance", "recording", "applicationState");
    private static final Set<String> OMISSION_CODES = Set.of(
            "unsupported", "not-started", "empty", "sensitive", "oversized", "provider-failed",
            "changed-during-capture", "no-retained-recording", "active", "missing", "changed");

    public MobileEvidenceBundle {
        capturedAt = Objects.requireNonNull(capturedAt, "capturedAt");
        archive = Objects.requireNonNull(archive, "archive").toAbsolutePath().normalize();
        context = Objects.requireNonNull(context, "context");
        if (context.isBlank()) {
            throw new IllegalArgumentException("context must not be blank");
        }
        applicationMetadata = immutableMap(applicationMetadata, APPLICATION_METADATA_KEYS, "application metadata");
        deviceMetadata = immutableMap(deviceMetadata, DEVICE_METADATA_KEYS, "device metadata");
        logMessages = immutableList(logMessages);
        logErrors = immutableList(logErrors);
        performanceSamples = immutableList(performanceSamples);
        artifacts = immutableArtifacts(artifacts);
        omissions = immutableMap(omissions, OMISSION_KEYS, "omission component");
        omissions.forEach((component, code) -> {
            if (!OMISSION_CODES.contains(code)) {
                throw new IllegalArgumentException("Unsupported omission code for " + component + ".");
            }
        });
    }

    @Override
    public String toString() {
        return "MobileEvidenceBundle["
                + "capturedAt=" + capturedAt
                + ", applicationMetadata=" + applicationMetadata.size()
                + ", deviceMetadata=" + deviceMetadata.size()
                + ", logMessages=" + logMessages.size()
                + ", logErrors=" + logErrors.size()
                + ", performanceSamples=" + performanceSamples.size()
                + ", artifacts=" + artifacts.size()
                + ", omissions=" + omissions.size()
                + "]";
    }

    private static Map<String, String> immutableMap(Map<String, String> values, Set<String> allowedKeys,
                                                     String label) {
        if (values == null) {
            return Map.of();
        }
        Map<String, String> snapshot = new LinkedHashMap<>(values);
        if (snapshot.isEmpty()) {
            return Map.of();
        }
        snapshot.forEach((key, value) -> {
            Objects.requireNonNull(key, "map key");
            Objects.requireNonNull(value, "map value");
            if (!allowedKeys.contains(key)) {
                throw new IllegalArgumentException("Unsupported " + label + " key.");
            }
        });
        return Collections.unmodifiableMap(snapshot);
    }

    private static <T> List<T> immutableList(List<T> values) {
        return values == null ? List.of() : List.copyOf(values);
    }

    private static List<TraceArtifactReference> immutableArtifacts(List<TraceArtifactReference> values) {
        List<TraceArtifactReference> copied = immutableList(values);
        Set<String> ids = new HashSet<>();
        for (TraceArtifactReference artifact : copied) {
            if (!ids.add(artifact.id())) {
                throw new IllegalArgumentException("artifact ids must be unique");
            }
        }
        return copied;
    }
}
