package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/performance.properties",
        "file:src/main/resources/properties/default/performance.properties",
        "classpath:performance.properties",
})
public interface Performance extends EngineProperties<Performance> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.performance = ConfigFactory.create(Performance.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("lightHouseExecution")
    @DefaultValue("false")
    boolean isEnabled();

    @Key("lightHouseExecution.port")
    @DefaultValue("8888")
    int port();

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
    }
}
