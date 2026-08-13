package com.shaft.infrastructure;

import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.TreeSet;

/** Immutable profile component selection, normalized before it becomes plan actions. */
public record SetupSelection(List<String> components) {
    public SetupSelection {
        Objects.requireNonNull(components, "components");
        TreeSet<String> normalized = new TreeSet<>();
        for (String component : components) {
            if (component == null || !component.matches("[a-zA-Z0-9_]+")) {
                throw new IllegalArgumentException("Setup components must use letters, digits, or underscores.");
            }
            normalized.add(component.toLowerCase(Locale.ROOT));
        }
        components = List.copyOf(normalized);
    }

    public static SetupSelection defaults() {
        return new SetupSelection(List.of());
    }
}
