package com.shaft.internal.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/internal.properties",
        "file:src/main/resources/properties/default/internal.properties",
        "classpath:internal.properties",
})
public interface Internal extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.internal = ConfigFactory.create(Internal.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("shaftEngineVersion")
    @DefaultValue("SHAFT Engine v7.1.20230321")
    String shaftEngineVersion();

    @Key("watermarkImagePath")
    @DefaultValue("https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/main/src/main/resources/images/shaft_white_bg.png")
    String watermarkImagePath();

    @Key("allureVersion")
    @DefaultValue("2.21.0")
    String allureVersion();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void shaftEngineVersion(String value) {
            setProperty("shaftEngineVersion", value);
        }

        public void watermarkImagePath(String value) {
            setProperty("watermarkImagePath", value);
        }

        public void allureVersion(String value) {
            setProperty("allureVersion", value);
        }

}

}
