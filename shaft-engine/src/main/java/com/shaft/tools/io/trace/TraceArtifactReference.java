package com.shaft.tools.io.trace;

import java.util.Map;

/**
 * Stable reference from a trace session or event to an artifact stored in the trace archive.
 * The path is archive-relative; {@code omitted} distinguishes an omission marker from full content.
 */
public record TraceArtifactReference(String id, String kind, String path, String mimeType, boolean omitted,
                                     Map<String, String> metadata) {
    public TraceArtifactReference {
        id = required(id, "id");
        kind = required(kind, "kind");
        path = required(path, "path");
        validateArchivePath(path);
        mimeType = mimeType == null ? "" : mimeType;
        metadata = metadata == null ? Map.of() : Map.copyOf(metadata);
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
