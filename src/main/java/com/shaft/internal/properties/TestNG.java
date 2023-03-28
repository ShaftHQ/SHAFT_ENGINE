package com.shaft.internal.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/TestNG.properties", "file:src/main/resources/properties/default/TestNG.properties", "classpath:TestNG.properties",})
public interface TestNG extends EngineProperties {

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.testNG = ConfigFactory.create(TestNG.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("setParallel")
    @DefaultValue("NONE")
    String setParallel();

    @Key("setThreadCount")
    @DefaultValue("1")
    String setThreadCount();

    @Key("setVerbose")
    @DefaultValue("1")
    String setVerbose();

    @Key("setPreserveOrder")
    @DefaultValue("true")
    boolean setPreserveOrder();

    @Key("setGroupByInstances")
    @DefaultValue("true")
    boolean setGroupByInstances();

    @Key("setDataProviderThreadCount")
    @DefaultValue("1")
    String setDataProviderThreadCount();


    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void setParallel(String value) {
            setProperty("setParallel", value);
        }

        public void setThreadCount(String value) {
            setProperty("setThreadCount", value);
        }

        public void setVerbose(String value) {
            setProperty("setVerbose", value);
        }

        public void setPreserveOrder(boolean value) {
            setProperty("setPreserveOrder", String.valueOf(value));
        }

        public void setGroupByInstances(boolean value) {
            setProperty("setGroupByInstances", String.valueOf(value));
        }

        public void setDataProviderThreadCount(String value) {
            setProperty("setDataProviderThreadCount", value);
        }

    }
}
