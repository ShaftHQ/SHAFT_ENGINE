package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for performance testing settings in the SHAFT framework.
 * Controls Lighthouse audit thresholds and performance measurement parameters.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.performance.set().lighthouseExecution(true);
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/performance.properties",
        "file:src/main/resources/properties/default/performance.properties",
        "classpath:performance.properties",
})
public interface Performance extends EngineProperties<Performance> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.performanceOverride.set(ConfigFactory.create(Performance.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    @Key("lightHouseExecution")
    @DefaultValue("false")
    boolean isEnabled();

    @Key("lightHouseExecution.port")
    @DefaultValue("8888")
    int port();

    @Key("generatePerformanceReport")
    @DefaultValue("true")
        // Default is enabled
    boolean isEnablePerformanceReport();

    /**
     * Returns comma-separated per-endpoint API p95 performance budgets in milliseconds.
     *
     * <p>Use normalized endpoint names as report keys, for example
     * {@code users=500,orders/{id}=750}. Blank means no endpoint budgets are configured.
     *
     * @return configured API endpoint budgets, or a blank string
     */
    @Key("apiEndpointPerformanceBudgets")
    @DefaultValue("")
    String apiEndpointPerformanceBudgets();

    /**
     * Returns whether API p95 budget violations should fail the run instead of warning only.
     *
     * @return {@code true} when budget violations should fail the run
     */
    @Key("failOnApiPerformanceBudgetViolation")
    @DefaultValue("false")
    boolean failOnApiPerformanceBudgetViolation();

    /**
     * Returns comma-separated Playwright browser action p95 performance budgets in milliseconds.
     *
     * <p>Use metric names such as {@code playwright.element.click=750} or {@code *=1000}
     * to apply a default budget to every recorded browser action.
     *
     * @return configured browser action budgets, or a blank string
     */
    @Key("browserActionPerformanceBudgets")
    @DefaultValue("")
    String browserActionPerformanceBudgets();

    /**
     * Returns comma-separated Playwright page-load p95 performance budgets in milliseconds.
     *
     * <p>Use navigated URLs as keys, for example {@code https://example.com=3000}, or
     * {@code *=3000} to apply one default budget to every recorded page load.
     *
     * @return configured page-load budgets, or a blank string
     */
    @Key("pageLoadPerformanceBudgets")
    @DefaultValue("")
    String pageLoadPerformanceBudgets();

    /**
     * Returns whether browser p95 budget violations should fail the run instead of warning only.
     *
     * @return {@code true} when browser budget violations should fail the run
     */
    @Key("failOnBrowserPerformanceBudgetViolation")
    @DefaultValue("false")
    boolean failOnBrowserPerformanceBudgetViolation();

    default SetProperty set() {
        return new SetProperty();
    }

    @SuppressWarnings("unused")
    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty isEnabled(boolean value) {
            setProperty("lightHouseExecution", String.valueOf(value));
            return this;
        }

        public SetProperty port(int value) {
            setProperty("lightHouseExecution.port", String.valueOf(value));
            return this;
        }

        public SetProperty generatePerformanceReport(boolean value) {
            setProperty("generatePerformanceReport", String.valueOf(value));
            return this;
        }

        /**
         * Overrides comma-separated per-endpoint API p95 performance budgets in milliseconds.
         *
         * @param value budgets formatted as {@code endpoint=millis,endpoint=millis}
         * @return self-reference for fluent property overrides
         */
        public SetProperty apiEndpointPerformanceBudgets(String value) {
            setProperty("apiEndpointPerformanceBudgets", value);
            return this;
        }

        /**
         * Overrides whether API p95 budget violations should fail the run.
         *
         * @param value {@code true} to fail the run on budget violations
         * @return self-reference for fluent property overrides
         */
        public SetProperty failOnApiPerformanceBudgetViolation(boolean value) {
            setProperty("failOnApiPerformanceBudgetViolation", String.valueOf(value));
            return this;
        }

        /**
         * Overrides comma-separated Playwright browser action p95 performance budgets in milliseconds.
         *
         * @param value budgets formatted as {@code action=millis,action=millis}
         * @return self-reference for fluent property overrides
         */
        public SetProperty browserActionPerformanceBudgets(String value) {
            setProperty("browserActionPerformanceBudgets", value);
            return this;
        }

        /**
         * Overrides comma-separated Playwright page-load p95 performance budgets in milliseconds.
         *
         * @param value budgets formatted as {@code page=millis,page=millis}
         * @return self-reference for fluent property overrides
         */
        public SetProperty pageLoadPerformanceBudgets(String value) {
            setProperty("pageLoadPerformanceBudgets", value);
            return this;
        }

        /**
         * Overrides whether browser budget violations should fail the run.
         *
         * @param value {@code true} to fail the run on budget violations
         * @return self-reference for fluent property overrides
         */
        public SetProperty failOnBrowserPerformanceBudgetViolation(boolean value) {
            setProperty("failOnBrowserPerformanceBudgetViolation", String.valueOf(value));
            return this;
        }
    }
}
