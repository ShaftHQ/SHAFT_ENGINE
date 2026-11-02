package com.shaft.api.internal;

import com.shaft.driver.SHAFT;

import java.util.Locale;
import java.util.function.BooleanSupplier;
import java.util.function.IntSupplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Validated, per-request access to the SHAFT API property family: the response-status flag,
 * the three API timeouts, Swagger validation and OpenAPI coverage (#6326).
 *
 * <p>Every value is read when a request is performed, on the calling thread, so
 * {@code SHAFT.Properties.*.set()} calls made after {@code new SHAFT.API(...)} apply, and a
 * per-thread timeout override never leaks into another thread's requests. Booleans accept
 * {@code true}/{@code false} in any case; numbers accept surrounding whitespace. Anything else
 * fails with the property key, the rejected value and a {@code Fix:} line.</p>
 */
public final class ApiSettings {
    /** Largest timeout, in seconds, that still fits the HTTP client's millisecond {@code int}. */
    public static final int MAX_TIMEOUT_SECONDS = Integer.MAX_VALUE / 1000;
    public static final String AUTO_ASSERT_KEY = "automaticallyAssertResponseStatusCode";
    public static final String SWAGGER_ENABLED_KEY = "swagger.validation.enabled";
    public static final String SWAGGER_URL_KEY = "swagger.validation.url";
    public static final String COVERAGE_ENABLED_KEY = "openapi.coverage.report.enabled";
    public static final String COVERAGE_THRESHOLD_KEY = "openapi.coverage.threshold";
    private static final String COVERAGE_THRESHOLD_SETTER = "SHAFT.Properties.api.set().openApiCoverageThreshold(...)";
    private static final Pattern OWNER_CONVERSION = Pattern.compile("Cannot convert '(.*)' to \\w+", Pattern.DOTALL);

    private ApiSettings() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Whether a request without an explicit target status must return 2xx. An explicit
     * {@code setTargetStatusCode(n)} is asserted regardless of this flag.
     *
     * @return the validated {@code automaticallyAssertResponseStatusCode} value
     */
    public static boolean automaticallyAssertResponseStatusCode() {
        return readBoolean(AUTO_ASSERT_KEY, SHAFT.Properties.flags::automaticallyAssertResponseStatusCode,
                "SHAFT.Properties.flags.set().automaticallyAssertResponseStatusCode(...)");
    }

    /** @return the validated {@code swagger.validation.enabled} value */
    public static boolean swaggerValidationEnabled() {
        return readBoolean(SWAGGER_ENABLED_KEY, SHAFT.Properties.api::swaggerValidationEnabled,
                "SHAFT.Properties.api.set().swaggerValidationEnabled(...)");
    }

    /** @return the validated {@code openapi.coverage.report.enabled} value */
    public static boolean openApiCoverageReportEnabled() {
        return readBoolean(COVERAGE_ENABLED_KEY, SHAFT.Properties.api::openApiCoverageReportEnabled,
                "SHAFT.Properties.api.set().openApiCoverageReportEnabled(...)");
    }

    /** @return the validated {@code apiSocketTimeout} in seconds ({@code 0} = no timeout) */
    public static int apiSocketTimeoutSeconds() {
        return readTimeout("apiSocketTimeout", SHAFT.Properties.timeouts::apiSocketTimeout);
    }

    /** @return the validated {@code apiConnectionTimeout} in seconds ({@code 0} = no timeout) */
    public static int apiConnectionTimeoutSeconds() {
        return readTimeout("apiConnectionTimeout", SHAFT.Properties.timeouts::apiConnectionTimeout);
    }

    /** @return the validated {@code apiConnectionManagerTimeout} in seconds ({@code 0} = no timeout) */
    public static int apiConnectionManagerTimeoutSeconds() {
        return readTimeout("apiConnectionManagerTimeout", SHAFT.Properties.timeouts::apiConnectionManagerTimeout);
    }

    /** @return the validated {@code openapi.coverage.threshold} percentage */
    public static int openApiCoverageThreshold() {
        int threshold = readInt(COVERAGE_THRESHOLD_KEY, SHAFT.Properties.api::openApiCoverageThreshold,
                "a whole percentage from 0 to 100 (0 disables enforcement)", "80", COVERAGE_THRESHOLD_SETTER);
        return validateCoverageThreshold(threshold);
    }

    /**
     * Validates every API property a request depends on, before anything is sent.
     *
     * @throws IllegalArgumentException naming the first invalid property and how to fix it
     */
    public static void validateRequestSettings() {
        automaticallyAssertResponseStatusCode();
        apiSocketTimeoutSeconds();
        apiConnectionTimeoutSeconds();
        apiConnectionManagerTimeoutSeconds();
        if (swaggerValidationEnabled() && isBlank(SHAFT.Properties.api.swaggerValidationUrl())) {
            throw new IllegalArgumentException(missingSpecUrlMessage(SWAGGER_ENABLED_KEY));
        }
        if (openApiCoverageReportEnabled()) {
            if (isBlank(SHAFT.Properties.api.swaggerValidationUrl())) {
                throw new IllegalArgumentException(missingSpecUrlMessage(COVERAGE_ENABLED_KEY));
            }
            openApiCoverageThreshold();
        }
    }

    /**
     * Validates an explicit target status code.
     *
     * @param targetStatusCode {@code 0} for no explicit target, otherwise a three-digit status
     * @return the same value when valid
     * @throws IllegalArgumentException when the value is not 0 and not between 100 and 999
     */
    public static int validateTargetStatusCode(int targetStatusCode) {
        if (targetStatusCode != 0 && (targetStatusCode < 100 || targetStatusCode > 999)) {
            throw new IllegalArgumentException("Invalid target status code " + targetStatusCode
                    + "; expected a three-digit HTTP status from 100 to 999, or 0 for no explicit target."
                    + System.lineSeparator()
                    + "Fix: pass the status the server should return, for example setTargetStatusCode(200) or setTargetStatusCode(404).");
        }
        return targetStatusCode;
    }

    /**
     * Validates an OpenAPI coverage threshold.
     *
     * @param threshold required coverage percentage
     * @return the same value when valid
     * @throws IllegalArgumentException when the value is outside 0..100
     */
    public static int validateCoverageThreshold(int threshold) {
        if (threshold < 0 || threshold > 100) {
            throw new IllegalArgumentException(invalidValueMessage(COVERAGE_THRESHOLD_KEY, String.valueOf(threshold),
                    "a whole percentage from 0 to 100 (0 disables enforcement)", "80", COVERAGE_THRESHOLD_SETTER));
        }
        return threshold;
    }

    /**
     * Message for Swagger validation or OpenAPI coverage enabled without a spec URL.
     *
     * @param enabledKey the property that requires the spec URL
     * @return message with a fix-next line
     */
    public static String missingSpecUrlMessage(String enabledKey) {
        return enabledKey + "=true requires " + SWAGGER_URL_KEY + ", but it is missing or blank."
                + System.lineSeparator()
                + "Fix: set " + SWAGGER_URL_KEY + "=<OpenAPI URL, file path or inline definition>, or set "
                + enabledKey + "=false.";
    }

    static boolean readBoolean(String key, BooleanSupplier supplier, String setter) {
        try {
            return supplier.getAsBoolean();
        } catch (UnsupportedOperationException conversionFailure) {
            String raw = rawValue(conversionFailure);
            String trimmed = raw == null ? null : raw.trim().toLowerCase(Locale.ROOT);
            if ("true".equals(trimmed)) {
                return true;
            }
            if ("false".equals(trimmed)) {
                return false;
            }
            throw new IllegalArgumentException("Invalid value '" + raw + "' for SHAFT property \"" + key
                    + "\"; expected true or false (any case)." + System.lineSeparator()
                    + "Fix: set " + key + "=true or " + key + "=false in your properties file, as -D" + key
                    + "=..., or through " + setter + ".", conversionFailure);
        }
    }

    static int readTimeout(String key, IntSupplier supplier) {
        String expected = "a whole number of seconds from 0 (no timeout) to " + MAX_TIMEOUT_SECONDS;
        String setter = "SHAFT.Properties.timeouts.set()." + key + "(...)";
        int seconds = readInt(key, supplier, expected, "30", setter);
        if (seconds < 0 || seconds > MAX_TIMEOUT_SECONDS) {
            throw new IllegalArgumentException(invalidValueMessage(key, String.valueOf(seconds), expected, "30", setter));
        }
        return seconds;
    }

    static int readInt(String key, IntSupplier supplier, String expected, String example, String setter) {
        try {
            return supplier.getAsInt();
        } catch (UnsupportedOperationException conversionFailure) {
            String raw = rawValue(conversionFailure);
            if (raw != null) {
                try {
                    return Integer.parseInt(raw.trim());
                } catch (NumberFormatException ignored) {
                    // reported below with the key and the fix
                }
            }
            throw new IllegalArgumentException(invalidValueMessage(key, raw, expected, example, setter), conversionFailure);
        }
    }

    private static String invalidValueMessage(String key, String raw, String expected, String example, String setter) {
        return "Invalid value '" + raw + "' for SHAFT property \"" + key + "\"; expected " + expected + "."
                + System.lineSeparator()
                + "Fix: set " + key + "=" + example + " in your properties file, as -D" + key + "=" + example
                + ", or through " + setter + ".";
    }

    private static String rawValue(UnsupportedOperationException conversionFailure) {
        String message = conversionFailure.getMessage();
        if (message == null) {
            return null;
        }
        Matcher matcher = OWNER_CONVERSION.matcher(message);
        return matcher.find() ? matcher.group(1) : null;
    }

    private static boolean isBlank(String value) {
        return value == null || value.isBlank();
    }
}
