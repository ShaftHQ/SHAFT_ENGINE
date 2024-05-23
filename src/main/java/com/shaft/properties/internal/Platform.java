package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@Sources({"system:properties",
        "file:src/main/resources/properties/ExecutionPlatform.properties",
        "file:src/main/resources/properties/default/ExecutionPlatform.properties",
        "classpath:ExecutionPlatform.properties",
})
public interface Platform extends EngineProperties<Platform> {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.platform = ConfigFactory.create(Platform.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("SHAFT.CrossBrowserMode")
    @DefaultValue("off")
    String crossBrowserMode();

    @Key("executionAddress")
    @DefaultValue("local")
    String executionAddress();

    @Key("targetOperatingSystem")
    @DefaultValue("Linux")
    String targetPlatform();

    @Key("com.SHAFT.proxySettings")
    @DefaultValue("")
    String proxy();

    @Key("driverProxySettings")
    @DefaultValue("true")
    boolean driverProxySettings();

    @Key("jvmProxySettings")
    @DefaultValue("true")
    boolean jvmProxySettings();

    @Key("enableBiDi")
    @DefaultValue("true")
    boolean enableBiDi();

    default SetProperty set() {
        return new SetProperty();
    }

    @SuppressWarnings("unused")
    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty crossBrowserMode(String value) {
            setProperty("SHAFT.CrossBrowserMode", value);
            return this;
        }

        public SetProperty executionAddress(String value) {
            setProperty("executionAddress", value);
            return this;
        }

        /**
         * @param value io.github.shafthq.shaft.enums.OperatingSystems
         */
        public SetProperty targetPlatform(String value) {
            setProperty("targetOperatingSystem", value);
            return this;
        }

        public SetProperty proxySettings(String value) {
            setProperty("com.SHAFT.proxySettings", value);
            return this;
        }

        public SetProperty driverProxySettings(boolean value) {
            setProperty("driverProxySettings", String.valueOf(value));
            return this;
        }

        public SetProperty jvmProxySettings(boolean value) {
            setProperty("jvmProxySettings", String.valueOf(value));
            return this;
        }

        public SetProperty enableBiDi(boolean value) {
            setProperty("enableBiDi", String.valueOf(value));
            return this;
        }
    }
}
