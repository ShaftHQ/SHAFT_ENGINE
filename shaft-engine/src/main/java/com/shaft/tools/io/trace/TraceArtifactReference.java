package com.shaft.tools.io.trace;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Stable reference from a trace session or event to an artifact stored in the trace archive.
 * The path is archive-relative; {@code omitted} distinguishes an omission marker from full content.
 */
public record TraceArtifactReference(String id, String kind, String path, String mimeType, boolean omitted,
                                     Map<String, String> metadata) {
    public TraceArtifactReference(String id, String kind, String path, String mimeType, boolean omitted,
                                  Map<String, String> metadata) {
        this.id = required(id, "id");
        this.kind = required(kind, "kind");
        this.path = required(path, "path");
        validateArchivePath(this.path);
        this.mimeType = mimeType == null ? "" : mimeType;
        this.omitted = omitted;
        this.metadata = metadata == null
                ? Map.of()
                : Collections.unmodifiableMap(new HashMap<>(Map.copyOf(metadata)));
    }

    @Override
    public Map<String, String> metadata() {
        return Map.copyOf(metadata);
    }

    private static String required(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
        return value;
    }

    private static void validateArchivePath(String path) {
        if (path.startsWith("/") || path.startsWith("\\") || path.contains("\\") || path.contains(":")
                || path.endsWith("/") || path.contains("//")) {
            throw new IllegalArgumentException("path must be a portable archive-relative path");
        }
        for (String segment : path.split("/")) {
            if (segment.isBlank() || segment.equals(".") || segment.equals("..")) {
                throw new IllegalArgumentException("path must not contain empty, current, or parent segments");
            }
        }
    }
}
