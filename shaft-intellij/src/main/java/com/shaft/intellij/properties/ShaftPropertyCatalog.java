package com.shaft.intellij.properties;

import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * SHAFT Engine property catalog (issue #6417). The plugin build generates it from the engine's
 * {@code @Key} interfaces ({@code generateShaftPropertyCatalog} in {@code build.gradle.kts}), so
 * completion, validation and quick documentation can never drift from the engine source.
 */
public final class ShaftPropertyCatalog {
    /** Classpath location of the generated catalog. */
    public static final String RESOURCE = "/META-INF/shaft-engine/property-catalog.tsv";
    /** User-guide page listing every property. */
    public static final String USER_GUIDE_URL = "https://shafthq.github.io/docs/reference/properties/PropertiesList";

    private static final Set<String> BOOLEAN_TYPES = Set.of("boolean", "Boolean");
    private static final Set<String> INTEGER_TYPES = Set.of("int", "Integer", "long", "Long");
    private static final Set<String> DECIMAL_TYPES = Set.of("double", "Double", "float", "Float");
    private static final Map<String, ShaftProperty> CATALOG = load();

    private ShaftPropertyCatalog() {
    }

    /** Every known property, in declaration order. */
    public static Collection<ShaftProperty> all() {
        return CATALOG.values();
    }

    /** The property for {@code key}, or {@code null} when SHAFT does not know it. */
    public static @Nullable ShaftProperty find(String key) {
        return CATALOG.get(key);
    }

    /**
     * The closest known key to a key SHAFT does not know, or {@code null} when nothing is close
     * enough to be a likely typo. Custom keys unrelated to SHAFT therefore stay unflagged.
     */
    public static @Nullable String suggest(String unknownKey) {
        String best = null;
        int bestDistance = Math.max(2, unknownKey.length() / 6) + 1;
        String lower = unknownKey.toLowerCase(Locale.ROOT);
        for (String key : CATALOG.keySet()) {
            int distance = distance(lower, key.toLowerCase(Locale.ROOT), bestDistance);
            if (distance < bestDistance) {
                best = key;
                bestDistance = distance;
            }
        }
        return best;
    }

    /** Why {@code value} is invalid for {@code property}, or {@code null} when it is acceptable. */
    public static @Nullable String valueProblem(ShaftProperty property, String value) {
        String trimmed = value.trim();
        if (trimmed.isEmpty() || trimmed.contains("${")) {
            return null;
        }
        if (BOOLEAN_TYPES.contains(property.type())
                && !"true".equalsIgnoreCase(trimmed) && !"false".equalsIgnoreCase(trimmed)) {
            return "Expected true or false";
        }
        if (INTEGER_TYPES.contains(property.type()) && !trimmed.matches("[+-]?\\d+")) {
            return "Expected a whole number";
        }
        if (DECIMAL_TYPES.contains(property.type()) && !trimmed.matches("[+-]?(\\d+\\.?\\d*|\\.\\d+)")) {
            return "Expected a number";
        }
        return null;
    }

    static Map<String, ShaftProperty> parse(List<String> lines) {
        Map<String, ShaftProperty> result = new LinkedHashMap<>();
        for (String line : lines) {
            String[] cells = line.split("\t", -1);
            if (cells.length == 5 && !cells[0].isBlank()) {
                result.putIfAbsent(cells[0], new ShaftProperty(cells[0], cells[1], cells[2], cells[3], cells[4]));
            }
        }
        return Collections.unmodifiableMap(result);
    }

    private static Map<String, ShaftProperty> load() {
        try (InputStream stream = ShaftPropertyCatalog.class.getResourceAsStream(RESOURCE)) {
            if (stream == null) {
                return Map.of();
            }
            return parse(new String(stream.readAllBytes(), StandardCharsets.UTF_8).lines().toList());
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** Levenshtein distance, stopping early once it reaches {@code limit}. */
    private static int distance(String left, String right, int limit) {
        if (Math.abs(left.length() - right.length()) >= limit) {
            return limit;
        }
        int[] previous = new int[right.length() + 1];
        int[] current = new int[right.length() + 1];
        for (int j = 0; j <= right.length(); j++) {
            previous[j] = j;
        }
        for (int i = 1; i <= left.length(); i++) {
            current[0] = i;
            int rowMinimum = i;
            for (int j = 1; j <= right.length(); j++) {
                int cost = left.charAt(i - 1) == right.charAt(j - 1) ? 0 : 1;
                current[j] = Math.min(Math.min(current[j - 1] + 1, previous[j] + 1), previous[j - 1] + cost);
                rowMinimum = Math.min(rowMinimum, current[j]);
            }
            if (rowMinimum >= limit) {
                return limit;
            }
            int[] swap = previous;
            previous = current;
            current = swap;
        }
        return previous[right.length()];
    }
}
