package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/pattern.properties", "file:src/main/resources/properties/default/pattern.properties", "classpath:pattern.properties",})
public interface Pattern extends EngineProperties<Pattern> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.pattern = ConfigFactory.create(Pattern.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
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
