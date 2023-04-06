package testPackage.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;

@Sources({"system:properties",
        "file:src/main/resources/properties/ExecutionPlatform.properties",
        "file:src/main/resources/properties/default/ExecutionPlatform.properties",
        "classpath:ExecutionPlatform.properties",
})
public interface Platform extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
//        Properties.platform = ConfigFactory.create(Platform.class, updatedProps);
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

    default SetProperty set() {
        return new SetProperty();
    }

    @SuppressWarnings("unused")
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
        public void targetPlatform(String value) {
            setProperty("targetOperatingSystem", value);
        }

        public void proxySettings(String value) {
            setProperty("com.SHAFT.proxySettings", value);
        }
    }
}
