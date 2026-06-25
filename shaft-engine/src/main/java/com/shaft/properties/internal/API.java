package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for API (REST) testing in the SHAFT framework.
 * Covers Swagger/OpenAPI schema validation settings that are applied when performing
 * REST API requests via {@code SHAFT.API}.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.api.set().swaggerValidationEnabled(true).swaggerValidationUrl("https://petstore.swagger.io/v2/swagger.json");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/api.properties",
        "file:src/main/resources/properties/default/api.properties",
        "classpath:api.properties",
})
public interface API extends EngineProperties<API> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.apiOverride.set(ConfigFactory.create(API.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Whether automatic Swagger/OpenAPI response-schema validation is enabled for all API calls.
     * When enabled, each response is validated against the schema defined at {@link #swaggerValidationUrl()}.
     * <p>Property key: {@code swagger.validation.enabled} — default: {@code false}
     *
     * @return {@code true} to enable Swagger validation; {@code false} to skip it
     */
    @Key("swagger.validation.enabled")
    @DefaultValue("false")
    boolean swaggerValidationEnabled();

    /**
     * The URL of the Swagger/OpenAPI specification document used for response validation.
     * Only relevant when {@link #swaggerValidationEnabled()} returns {@code true}.
     * <p>Property key: {@code swagger.validation.url} — default: {@code ""}
     *
     * @return the Swagger spec URL, or an empty string if not configured
     */
    @Key("swagger.validation.url")
    @DefaultValue("")
    String swaggerValidationUrl();

    /**
     * Whether SHAFT should summarize OpenAPI operation coverage at the end of execution.
     * When enabled, API calls are matched against {@link #swaggerValidationUrl()} and grouped
     * by tag, path, and method.
     * <p>Property key: {@code openapi.coverage.report.enabled} - default: {@code false}
     *
     * @return {@code true} to enable OpenAPI coverage reporting; {@code false} to skip it
     */
    @Key("openapi.coverage.report.enabled")
    @DefaultValue("false")
    boolean openApiCoverageReportEnabled();

    /**
     * Minimum OpenAPI operation coverage percentage required for a passing run.
     * A value of {@code 0} disables threshold enforcement.
     * <p>Property key: {@code openapi.coverage.threshold} - default: {@code 0}
     *
     * @return required coverage percentage from {@code 0} to {@code 100}
     */
    @Key("openapi.coverage.threshold")
    @DefaultValue("0")
    int openApiCoverageThreshold();

    /**
     * Returns a fluent {@link SetProperty} builder for programmatically overriding API properties.
     *
     * <p>Example:
     * <pre>{@code
     * SHAFT.Properties.api.set().swaggerValidationEnabled(true);
     * }</pre>
     *
     * @return a new {@link SetProperty} instance
     */
    default SetProperty set() {
        return new SetProperty();
    }

    /**
     * Fluent builder that allows programmatic override of individual API configuration properties.
     * All setter methods return {@code this} to support method chaining.
     *
     * <p>Example:
     * <pre>{@code
     * SHAFT.Properties.api.set()
     *     .swaggerValidationEnabled(true)
     *     .swaggerValidationUrl("https://petstore.swagger.io/v2/swagger.json");
     * }</pre>
     */
    class SetProperty implements EngineProperties.SetProperty {

        /**
         * Creates a new {@code SetProperty} instance.
         */
        public SetProperty() {
        }

        /**
         * Overrides the {@code swagger.validation.enabled} property at runtime.
         *
         * @param value {@code true} to enable Swagger/OpenAPI response validation
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty swaggerValidationEnabled(boolean value) {
            setProperty("swagger.validation.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code swagger.validation.url} property at runtime.
         *
         * @param value the URL of the Swagger/OpenAPI specification document
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty swaggerValidationUrl(String value) {
            setProperty("swagger.validation.url", value);
            return this;
        }

        /**
         * Overrides the {@code openapi.coverage.report.enabled} property at runtime.
         *
         * @param value {@code true} to enable OpenAPI coverage reporting
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty openApiCoverageReportEnabled(boolean value) {
            setProperty("openapi.coverage.report.enabled", String.valueOf(value));
            return this;
        }

        /**
         * Overrides the {@code openapi.coverage.threshold} property at runtime.
         *
         * @param value required coverage percentage from {@code 0} to {@code 100}; {@code 0} disables enforcement
         * @return this {@link SetProperty} instance for chaining
         * @throws IllegalArgumentException if {@code value} is outside {@code 0..100}
         */
        public SetProperty openApiCoverageThreshold(int value) {
            if (value < 0 || value > 100) {
                throw new IllegalArgumentException("OpenAPI coverage threshold must be between 0 and 100.");
            }
            setProperty("openapi.coverage.threshold", String.valueOf(value));
            return this;
        }
    }
}
