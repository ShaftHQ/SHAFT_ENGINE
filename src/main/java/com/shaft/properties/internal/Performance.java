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
        ThreadLocalPropertiesManager.setProperty(key, value);
        Properties.performanceOverride.set(ConfigFactory.create(Performance.class, ThreadLocalPropertiesManager.getOverrides()));
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
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
