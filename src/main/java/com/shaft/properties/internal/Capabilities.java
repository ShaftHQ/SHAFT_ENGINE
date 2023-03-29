package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/customWebdriverCapabilities.properties", "file:src/main/resources/properties/default/customWebdriverCapabilities.properties", "classpath:customWebdriverCapabilities.properties"})
public interface Capabilities extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.capabilities = ConfigFactory.create(Capabilities.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("capabilities.accessKey")
    @DefaultValue("")
    String capabilitiesAccessKey();

    @Key("capabilities.appiumVersion")
    @DefaultValue("")
    String capabilitiesAppiumVersion();

    @Key("capabilities.deviceQuery")
    @DefaultValue("")
    String capabilitiesDeviceQuery();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public void capabilitiesAccessKey(String value) {
            setProperty("capabilities.accessKey", value);
        }

        public void capabilitiesAppiumVersion(String value) {
            setProperty("capabilities.appiumVersion", value);
        }

        public void capabilitiesDeviceQuery(String value) {
            setProperty("capabilities.deviceQuery", value);
        }

    }

}
