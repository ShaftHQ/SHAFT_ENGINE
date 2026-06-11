package com.shaft.capture.model;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NullNode;

import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

final class ModelSupport {
    private ModelSupport() {
        throw new IllegalStateException("Utility class");
    }

    static String requireText(String value, String field) {
        String normalized = Objects.requireNonNullElse(value, "").trim();
        if (normalized.isEmpty()) {
            throw new IllegalArgumentException(field + " is required.");
        }
        return normalized;
    }

    static String text(String value) {
        return Objects.requireNonNullElse(value, "").trim();
    }

    static <T> List<T> list(List<T> values) {
        return values == null ? List.of() : List.copyOf(values);
    }

    static Map<String, String> strings(Map<String, String> values) {
        if (values == null || values.isEmpty()) {
            return Map.of();
        }
        Map<String, String> copy = new TreeMap<>();
        values.forEach((key, value) -> copy.put(text(key).toLowerCase(), Objects.requireNonNullElse(value, "")));
        return Map.copyOf(copy);
    }

    static Map<String, JsonNode> extensions(Map<String, JsonNode> values) {
        if (values == null || values.isEmpty()) {
            return Map.of();
        }
        Map<String, JsonNode> copy = new LinkedHashMap<>();
        new TreeMap<>(values).forEach((key, value) ->
                copy.put(requireText(key, "Extension name"),
                        value == null ? NullNode.getInstance() : value.deepCopy()));
        return Map.copyOf(copy);
    }

    static String relativePath(String value, String field) {
        String normalized = text(value).replace('\\', '/');
        if (normalized.isEmpty()) {
            return "";
        }
        try {
            Path path = Path.of(normalized);
            if (path.isAbsolute() || normalized.equals("..") || normalized.startsWith("../")
                    || normalized.contains("/../")) {
                throw new IllegalArgumentException(field + " must be a safe relative path.");
            }
        } catch (InvalidPathException exception) {
            throw new IllegalArgumentException(field + " must be a valid relative path.", exception);
        }
        return normalized;
    }
}
