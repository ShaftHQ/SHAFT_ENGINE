package com.shaft.tools.io.internal;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * Keeps secret values out of everything SHAFT writes to the execution report (#7173).
 *
 * <p>A properties file whose name contains {@code secret} (for example {@code secrets.properties})
 * is a secret file: its keys and values are registered when it is loaded, it is never attached, and
 * its keys never reach {@code environment.xml}. {@link #redact(String)} replaces registered values and
 * JWT-shaped tokens with {@value #MASK}; report sinks (Allure step names and parameters, status
 * details, log entries, text attachments) call it before anything is written.
 */
public final class ReportSecrets {
    /** The mask written in place of a secret value. */
    public static final String MASK = "********";
    private static final int MINIMUM_SECRET_LENGTH = 4;
    private static final Set<String> NON_SECRET_VALUES = Set.of("true", "false", "null");
    private static final Pattern JWT = Pattern.compile(
            "eyJ[A-Za-z0-9_-]{8,}\\.eyJ[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]*");
    private static final Pattern SENSITIVE_KEY = Pattern.compile(
            "(?i).*(password|passwd|secret|token|api[._-]?key|access[._-]?key|private[._-]?key|authorization"
                    + "|credential|session[._-]?id).*");
    private static final Set<String> SECRET_KEYS = ConcurrentHashMap.newKeySet();
    private static final Set<String> SECRET_VALUES = ConcurrentHashMap.newKeySet();
    private static volatile List<String> valuesLongestFirst = List.of();

    private ReportSecrets() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Returns whether a properties file holds secrets and must never be attached or published.
     *
     * @param fileName the file name, with or without the {@code .properties} extension
     * @return {@code true} when the file name contains {@code secret}, ignoring case
     */
    public static boolean isSecretFile(String fileName) {
        return fileName != null && fileName.toLowerCase(Locale.ROOT).contains("secret");
    }

    /**
     * Registers every entry of a secret properties file so its values are masked in the report.
     *
     * @param properties the entries loaded from a secret file
     */
    public static void registerAll(Map<?, ?> properties) {
        properties.forEach((key, value) -> {
            SECRET_KEYS.add(String.valueOf(key).trim());
            if (value != null) {
                registerValue(String.valueOf(value));
            }
        });
    }

    /**
     * Registers one literal value that must never appear in the report.
     *
     * @param value the secret value; blanks, booleans, and values shorter than four characters are ignored
     */
    public static void registerValue(String value) {
        if (value == null) {
            return;
        }
        String trimmed = value.trim();
        if (trimmed.length() < MINIMUM_SECRET_LENGTH || NON_SECRET_VALUES.contains(trimmed.toLowerCase(Locale.ROOT))) {
            return;
        }
        if (SECRET_VALUES.add(trimmed)) {
            valuesLongestFirst = SECRET_VALUES.stream()
                    .sorted(Comparator.comparingInt(String::length).reversed()).toList();
        }
    }

    /**
     * Returns whether a property key came from a secret file.
     *
     * @param key the property key
     * @return {@code true} for keys registered from a secret file
     */
    public static boolean isSecretKey(String key) {
        return key != null && SECRET_KEYS.contains(key.trim());
    }

    /**
     * Returns whether a property key names a credential, such as {@code password} or {@code accessKey}.
     *
     * @param key the property key
     * @return {@code true} when the key is registered or its name looks sensitive
     */
    public static boolean isSensitiveKey(String key) {
        return isSecretKey(key) || (key != null && SENSITIVE_KEY.matcher(key).matches());
    }

    /**
     * Returns the value to publish for a property: the mask for sensitive keys holding a non-boolean
     * value, otherwise the value with secrets redacted.
     *
     * @param key   the property key
     * @param value the property value
     * @return the value that is safe to publish; empty for a {@code null} value
     */
    public static String publishableValue(String key, String value) {
        if (value == null) {
            return "";
        }
        if (isSensitiveKey(key) && !value.isBlank() && !NON_SECRET_VALUES.contains(value.trim().toLowerCase(Locale.ROOT))) {
            return MASK;
        }
        return redact(value);
    }

    /**
     * Masks sensitive {@code key=value} lines of a properties file and redacts secret values.
     *
     * @param content the properties file content
     * @return the content that is safe to attach; empty for {@code null} content
     */
    public static String redactProperties(String content) {
        if (content == null) {
            return "";
        }
        StringBuilder redacted = new StringBuilder(content.length());
        for (String line : linesKeepingEnds(content)) {
            String trimmed = line.stripLeading();
            int separator = indexOfSeparator(trimmed);
            if (!trimmed.startsWith("#") && !trimmed.startsWith("!") && separator > 0) {
                String key = trimmed.substring(0, separator).trim();
                String value = trimmed.substring(separator + 1).strip();
                String published = publishableValue(key, value);
                if (!published.equals(value)) {
                    String lineEnd = line.endsWith("\r\n") ? "\r\n" : line.endsWith("\n") ? "\n" : "";
                    redacted.append(line, 0, line.length() - trimmed.length())
                            .append(trimmed, 0, separator + 1).append(published).append(lineEnd);
                    continue;
                }
            }
            redacted.append(redact(line));
        }
        return redacted.toString();
    }

    /**
     * Replaces registered secret values and JWT-shaped tokens with {@value #MASK}.
     *
     * @param text the text about to be written to the report
     * @return the text with every secret masked; {@code null} stays {@code null}
     */
    public static String redact(String text) {
        if (text == null || text.isEmpty()) {
            return text;
        }
        String redacted = text;
        for (String value : valuesLongestFirst) {
            if (redacted.contains(value)) {
                redacted = redacted.replace(value, MASK);
            }
        }
        if (redacted.contains("eyJ")) {
            redacted = JWT.matcher(redacted).replaceAll(MASK);
        }
        return redacted;
    }

    /**
     * Tells whether the text holds a registered secret value or a JWT-shaped token.
     *
     * @param text the text to check
     * @return {@code true} when {@link #redact(String)} would mask part of it
     */
    public static boolean containsSecret(String text) {
        return text != null && !text.isEmpty() && !redact(text).equals(text);
    }

    /** Forgets every registered key and value. Intended for tests and re-initialization. */
    public static void clear() {
        SECRET_KEYS.clear();
        SECRET_VALUES.clear();
        valuesLongestFirst = List.of();
    }

    private static List<String> linesKeepingEnds(String content) {
        List<String> lines = new java.util.ArrayList<>();
        int start = 0;
        while (start < content.length()) {
            int end = content.indexOf('\n', start);
            int next = end < 0 ? content.length() : end + 1;
            lines.add(content.substring(start, next));
            start = next;
        }
        return lines;
    }

    private static int indexOfSeparator(String line) {
        for (int i = 0; i < line.length(); i++) {
            char character = line.charAt(i);
            if (character == '\\') {
                i++;
            } else if (character == '=' || character == ':') {
                return i;
            }
        }
        return -1;
    }
}
