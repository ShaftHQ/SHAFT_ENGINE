package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
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
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("testDataColumnNamePrefix")
    @DefaultValue("Data")
    String testDataColumnNamePrefix();

    @Key("allure.link.issue.pattern")
    @DefaultValue("")
    String allureLinkIssuePattern();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty testDataColumnNamePrefix(String value) {
            setProperty("testDataColumnNamePrefix", value);
            return this;
        }

        public SetProperty allureLinkIssuePattern(String value) {
            setProperty("allure.link.issue.pattern", value);
            return this;
        }

    }

}
