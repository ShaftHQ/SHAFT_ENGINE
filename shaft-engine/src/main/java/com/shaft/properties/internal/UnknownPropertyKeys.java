package com.shaft.properties.internal;

import org.aeonbits.owner.Config;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

/**
 * Finds likely misspelled SHAFT property keys in the property files (#7085).
 *
 * <p>Advisory only: a key is reported only when it is not a known SHAFT key and is close to one
 * (same letters ignoring case and separators, or a small edit distance). Any other unknown key is
 * treated as the user's own key and is never reported, so custom keys in {@code custom.properties}
 * stay silent. Pass-through prefixes (capabilities, BrowserStack, LambdaTest, mobile) are skipped.
 */
public final class UnknownPropertyKeys {
    /** Every SHAFT property interface; {@code UnknownPropertyKeysTest} keeps this list complete. */
    static final List<Class<?>> PROPERTY_INTERFACES = List.of(API.class, Allure.class, BrowserStack.class,
            Capture.class, Cucumber.class, Flags.class, Healenium.class, Healing.class, Infrastructure.class,
            Internal.class, Jira.class, LambdaTest.class, Log4j.class, ManagedLocalAi.class, Mobile.class,
            NaturalActions.class, Ocr.class, Paths.class, Pattern.class, Performance.class, Pilot.class,
            Platform.class, Playwright.class, Reporting.class, TestNG.class, Timeouts.class, Tinkey.class,
            Visuals.class, Web.class);
    /** Files that hold third-party keys, not SHAFT keys. */
    static final Set<String> SKIPPED_FILES = Set.of("log4j2.properties", "customWebdriverCapabilities.properties",
            "junit-platform.properties", "reportportal.properties");
    private static final List<String> PASS_THROUGH_PREFIXES = List.of("capabilities.", "browserStack.",
            "lambdaTest.", "mobile_", "appium:");
    private static final Set<String> KNOWN_KEYS = collectKnownKeys();

    private UnknownPropertyKeys() {
    }

    /** All {@code @Key} values declared by the SHAFT property interfaces. */
    static Set<String> knownKeys() {
        return KNOWN_KEYS;
    }

    private static Set<String> collectKnownKeys() {
        Set<String> keys = new TreeSet<>();
        for (Class<?> type : PROPERTY_INTERFACES) {
            for (Method method : type.getMethods()) {
                Config.Key key = method.getAnnotation(Config.Key.class);
                if (key != null) {
                    keys.add(key.value());
                }
            }
        }
        return java.util.Collections.unmodifiableSet(keys);
    }

    /**
     * Returns one warning per likely misspelled key, each with a did-you-mean suggestion.
     *
     * @param fileName the property file name, used to skip third-party files and in the message
     * @param keys     the keys read from that file
     * @return the warnings, empty when nothing looks misspelled
     */
    public static List<String> warnings(String fileName, Collection<String> keys) {
        List<String> warnings = new ArrayList<>();
        if (SKIPPED_FILES.contains(fileName)) {
            return warnings;
        }
        Set<String> known = knownKeys();
        for (String key : new TreeSet<>(keys)) {
            if (known.contains(key) || PASS_THROUGH_PREFIXES.stream().anyMatch(key::startsWith)) {
                continue;
            }
            suggestion(key, known).ifPresent(match -> warnings.add("Unknown SHAFT property key \"" + key + "\" in "
                    + fileName + "; did you mean \"" + match + "\"? The value is ignored by SHAFT."));
        }
        return warnings;
    }

    static Optional<String> suggestion(String key, Set<String> known) {
        String normalized = normalize(key);
        int allowed = Math.min(2, normalized.length() / 6);
        String best = null;
        int bestDistance = Integer.MAX_VALUE;
        for (String candidate : known) {
            int distance = distance(normalized, normalize(candidate), allowed);
            if (distance < bestDistance) {
                best = candidate;
                bestDistance = distance;
            }
        }
        return bestDistance <= allowed ? Optional.ofNullable(best) : Optional.empty();
    }

    private static String normalize(String key) {
        return key.replaceAll("[_.\\-]", "").toLowerCase(Locale.ROOT);
    }

    /** Levenshtein distance, cut off above {@code limit} (returns {@code limit + 1}). */
    private static int distance(String a, String b, int limit) {
        if (Math.abs(a.length() - b.length()) > limit) {
            return limit + 1;
        }
        int[] previous = new int[b.length() + 1];
        int[] current = new int[b.length() + 1];
        for (int j = 0; j <= b.length(); j++) {
            previous[j] = j;
        }
        for (int i = 1; i <= a.length(); i++) {
            current[0] = i;
            int rowMin = current[0];
            for (int j = 1; j <= b.length(); j++) {
                int cost = a.charAt(i - 1) == b.charAt(j - 1) ? 0 : 1;
                current[j] = Math.min(Math.min(current[j - 1] + 1, previous[j] + 1), previous[j - 1] + cost);
                rowMin = Math.min(rowMin, current[j]);
            }
            if (rowMin > limit) {
                return limit + 1;
            }
            int[] swap = previous;
            previous = current;
            current = swap;
        }
        return previous[b.length()];
    }
}
