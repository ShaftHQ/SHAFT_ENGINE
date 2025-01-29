package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings({"SameParameterValue", "unused"})
@Sources({"system:properties", "file:src/test/resources/allure.properties", "file:src/main/resources/properties/default/allure.properties", "classpath:allure.properties",})
public interface AllureResults extends EngineProperties<AllureResults> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.allureResults = ConfigFactory.create(AllureResults.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("allure.results.directory")
    @DefaultValue("allure-results")
    String allureResultsDirectory();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty allureResultsDirectory(String value) {
            setProperty("allure.results.directory", value);
            return this;
        }
    }

}
