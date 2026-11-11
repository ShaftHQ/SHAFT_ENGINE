package com.shaft.tools.io;

import java.util.regex.Pattern;

/**
 * Whole-token classified failure ids. A result is classified when its message contains one default
 * token, or a caller-supplied extension, and the session actually started.
 *
 * <p>Default tokens: {@code DEF-<digits>}, {@code G<digits>} (not inside a hostname such as
 * {@code Zephyrus-G14-GA401}), {@code SHAFT#<digits>}, {@code BS-<tokens>}, {@code ADB-<tokens>}.
 * The published extension, included in the default pattern, is {@code FUNC-<token>} and
 * {@code Jnn-BUG-<digits>} (for example {@code FUNC-J01-BUG-03} and {@code J01-BUG-03}).
 * Pass another whole-token {@link Pattern} to {@link #accepts(String, Pattern)} to add a project token.
 *
 * <p>Never-started sessions are rejected even when they also name an id: the whole token
 * {@code BS-QUOTA}, and the text {@code SessionNotCreated}.
 *
 * <p>The same contract is packaged at {@code META-INF/shaft/classified-failure-ids.md}.
 */
public final class ClassifiedFailureId {
    /**
     * Whole-token pattern. {@code G<digits>} refuses a following letter, digit, or hyphen so a hostname
     * segment such as {@code G14-GA401} is not a locator id. Repeated id segments use {@code {1,}} and
     * {@code {0,}}, which match the same text as {@code +} and {@code *} without a nested quantifier.
     */
    public static final String DEFAULT_PATTERN = "(?<![A-Za-z0-9])(?:DEF-\\d+"
            + "|G\\d+(?![A-Za-z0-9-])"
            + "|SHAFT#\\d+"
            + "|BS-[A-Z0-9]{1,}(?:-[A-Z0-9]{1,}){0,}"
            + "|ADB-[A-Z0-9]{1,}(?:-[A-Z0-9]{1,}){0,}"
            + "|FUNC-[A-Z0-9]{1,}(?:-[A-Z0-9]{1,}){0,}"
            + "|J\\d{2}-BUG-\\d+)(?![A-Za-z0-9])";

    private static final Pattern DEFAULT = Pattern.compile(DEFAULT_PATTERN);
    private static final Pattern NEVER_STARTED = Pattern.compile(
            "(?<![A-Za-z0-9])BS-QUOTA(?![A-Za-z0-9])|SessionNotCreated");

    private ClassifiedFailureId() {
    }

    /**
     * Accepts a message that names a default classified id after the session has started.
     *
     * @param message failure, skip, or log text; {@code null} is unclassified
     * @return {@code true} when {@code message} names a default classified id and the session started
     */
    public static boolean accepts(String message) {
        return accepts(message, null);
    }

    /**
     * Accepts a message that names a default or caller-supplied classified id.
     *
     * @param message   failure, skip, or log text
     * @param extension extra whole-token pattern, or {@code null} to use only the default contract
     * @return {@code true} when a default or extension id matches and the session is not never-started
     */
    public static boolean accepts(String message, Pattern extension) {
        if (message == null || message.isBlank()) {
            return false;
        }
        if (NEVER_STARTED.matcher(message).find()) {
            return false;
        }
        if (DEFAULT.matcher(message).find()) {
            return true;
        }
        return extension != null && extension.matcher(message).find();
    }
}
