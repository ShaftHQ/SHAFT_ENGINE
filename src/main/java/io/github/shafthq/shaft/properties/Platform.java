package io.github.shafthq.shaft.properties;

import com.shaft.tools.io.ReportManager;
import io.github.shafthq.shaft.enums.OperatingSystems;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@Sources({"system:properties",
        "file:src/main/resources/properties/ExecutionPlatform.properties",
        "file:src/main/resources/properties/default/ExecutionPlatform.properties",
        "classpath:ExecutionPlatform.properties",
})
public interface Platform extends EngineProperties {
    @Key("SHAFT.CrossBrowserMode")
    @DefaultValue("off")
    String crossBrowserMode();

    @Key("executionAddress")
    @DefaultValue("local")
    String executionAddress();

    @Key("targetOperatingSystem")
    @DefaultValue(OperatingSystems.LINUX)
    String targetOperatingSystem();

    @Key("com.SHAFT.proxySettings")
    @DefaultValue("")
    String proxySettings();

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.platform = ConfigFactory.create(Platform.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void crossBrowserMode(String value) {
            setProperty("SHAFT.CrossBrowserMode", value);
        }

        public void executionAddress(String value) {
            setProperty("executionAddress", value);
        }

        /**
         * @param value io.github.shafthq.shaft.enums.OperatingSystems
         */
        public void targetOperatingSystem(String value) {
            setProperty("targetOperatingSystem", value);
        }

        public void proxySettings(String value) {
            setProperty("com.SHAFT.proxySettings", value);
        }
    }
}
