package com.shaft.tools.io.internal;

import java.util.Locale;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * Spots attachment calls whose arguments were swapped (#7185).
 *
 * <p>SHAFT's {@code SHAFT.Report.attach(type, name, content)} and Allure's
 * {@code Allure.attachment(name, type, content)} order the same three strings differently, so a swapped
 * call compiles and the report then shows the content as the attachment type. This guard names such
 * calls; it never blocks them, because attachments are best-effort evidence.
 */
public final class AttachmentTypeGuard {
    /** Longest type accepted as a MIME type, per RFC 6838 (127 characters per name part). */
    static final int MAX_TYPE_LENGTH = 127;
    private static final int QUOTED_LENGTH = 40;
    private static final Pattern MIME = Pattern.compile("^[a-zA-Z0-9][\\w.+-]*/[\\w.+-]+(\\s*;[^\\r\\n]*)?$");

    private AttachmentTypeGuard() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Tells whether a value is a MIME type such as {@code text/markdown}: {@code type/subtype}, one line,
     * at most 127 characters, optionally followed by parameters.
     *
     * @param type the attachment type
     * @return {@code true} for a MIME type
     */
    public static boolean isMimeType(String type) {
        return type != null && type.length() <= MAX_TYPE_LENGTH && MIME.matcher(type.trim()).matches();
    }

    /**
     * Describes what looks swapped in one attachment call, or returns an empty result when nothing does.
     * A type is treated as content when it is not a MIME type and is multi-line or longer than 127
     * characters; SHAFT keywords such as {@code Markdown} stay quiet. Text content that is exactly a MIME
     * type, while the type is not one, is reported as a swapped call too.
     *
     * @param name        the attachment name
     * @param type        the attachment type
     * @param textContent the attachment content when it is text, otherwise {@code null}
     * @return the warning to log, or empty
     */
    public static Optional<String> callTimeProblem(String name, String type, String textContent) {
        if (type == null || isMimeType(type)) {
            return Optional.empty();
        }
        if (type.length() > MAX_TYPE_LENGTH || type.contains("\n") || type.contains("\r")) {
            return Optional.of("Attachment '" + name + "' has content instead of a type ('" + abbreviate(type)
                    + "'); the arguments look swapped. Allure.attachment takes (name, type, content);"
                    + " SHAFT.Report.attach takes (type, name, content).");
        }
        if (textContent != null && isMimeType(textContent)) {
            return Optional.of("Attachment '" + name + "' has the MIME type '" + textContent.trim().toLowerCase(Locale.ROOT)
                    + "' as its content and '" + abbreviate(type) + "' as its type; the arguments look swapped.");
        }
        return Optional.empty();
    }

    /**
     * Returns the first line of a value, cut to 40 characters.
     *
     * @param value the value to quote
     * @return the short form
     */
    static String abbreviate(String value) {
        String first = value.strip().lines().findFirst().orElse("");
        return first.length() > QUOTED_LENGTH ? first.substring(0, QUOTED_LENGTH) + "..." : first;
    }
}
