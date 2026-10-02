package com.shaft.properties.internal;

import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

/**
 * Configuration properties interface for pattern settings in the SHAFT framework.
 * Controls test-data column name prefixes and Allure link patterns used during execution.
 *
 * <p>Use {@link #set()} to override values programmatically:
 * <pre>{@code
 * SHAFT.Properties.pattern.set().testDataColumnNamePrefix("Data");
 * }</pre>
 */
@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/pattern.properties", "file:src/main/resources/properties/default/pattern.properties", "classpath:pattern.properties",})
public interface Pattern extends EngineProperties<Pattern> {
    private static void setProperty(String key, String value) {
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.patternOverride.set(ConfigFactory.create(Pattern.class, ThreadLocalPropertiesManager.getOverrides()));
        EngineProperties.logPropertyUpdate(key, value);
    }

    /**
     * Prefix used when naming generated data-provider parameters in test reports.
     *
     * <p>Default: {@code Data}.
     *
     * @return the configured value of {@code testDataColumnNamePrefix}
     */
    @Key("testDataColumnNamePrefix")
    @DefaultValue("Data")
    String testDataColumnNamePrefix();

    /**
     * URL pattern used to turn {@literal @}Issue/{@literal @}Issues annotation values into clickable
     * links in the Allure report; use {} as the issue-ID placeholder.
     *
     * @return the configured value of {@code allure.link.issue.pattern}
     */
    @Key("allure.link.issue.pattern")
    @DefaultValue("")
    String allureLinkIssuePattern();

    /**
     * Starts a fluent, thread-local override of these properties for the current test thread.
     *
     * @return a new {@link SetProperty} builder
     */
    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        /**
         * Overrides the {@code testDataColumnNamePrefix} property at runtime. Prefix used when naming
         * generated data-provider parameters in test reports.
         *
         * @param value the new value of {@code testDataColumnNamePrefix}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty testDataColumnNamePrefix(String value) {
            setProperty("testDataColumnNamePrefix", value);
            return this;
        }

        /**
         * Overrides the {@code allure.link.issue.pattern} property at runtime. URL pattern used to turn
         * {@literal @}Issue/{@literal @}Issues annotation values into clickable links in the Allure
         * report; use {} as the issue-ID placeholder.
         *
         * @param value the new value of {@code allure.link.issue.pattern}
         * @return this {@link SetProperty} instance for chaining
         */
        public SetProperty allureLinkIssuePattern(String value) {
            setProperty("allure.link.issue.pattern", value);
            return this;
        }

    }

}
