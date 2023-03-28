package com.shaft.internal.properties;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/WebCapabilities.properties",
        "file:src/main/resources/properties/default/WebCapabilities.properties",
        "classpath:WebCapabilities.properties",
})
public interface Web extends EngineProperties {
    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.web = ConfigFactory.create(Web.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("targetBrowserName")
    @DefaultValue("chrome")
    String targetBrowserName();

    @Key("headlessExecution")
    @DefaultValue("false")
    boolean headlessExecution();

    @Key("isMobileEmulation")
    @DefaultValue("false")
    boolean isMobileEmulation();

    @Key("mobileEmulation.isCustomDevice")
    @DefaultValue("false")
    boolean mobileEmulation_isCustomDevice();

    @Key("mobileEmulation.deviceName")
    @DefaultValue("")
    String mobileEmulation_deviceName();

    @Key("mobileEmulation.width")
    @DefaultValue("")
    String mobileEmulation_width();

    @Key("mobileEmulation.height")
    @DefaultValue("")
    String mobileEmulation_height();

    @Key("mobileEmulation.pixelRatio")
    @DefaultValue("")
    String mobileEmulation_pixelRatio();

    @Key("mobileEmulation.userAgent")
    @DefaultValue("")
    String mobileEmulation_userAgent();

    @Key("baseURL")
    @DefaultValue("")
    String baseURL();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public void baseURL(String value) {
            setProperty("baseURL", value);
        }

        /**
         * @param value io.github.shafthq.shaft.enums.Browsers
         */
        public void targetBrowserName(String value) {
            setProperty("targetBrowserName", value);
        }

        public void headlessExecution(boolean value) {
            setProperty("headlessExecution", String.valueOf(value));
        }

        public void isMobileEmulation(boolean value) {
            setProperty("isMobileEmulation", String.valueOf(value));
        }

        public void mobileEmulation_isCustomDevice(boolean value) {
            setProperty("mobileEmulation.isCustomDevice", String.valueOf(value));
        }

        public void mobileEmulation_deviceName(String value) {
            setProperty("mobileEmulation.deviceName", value);
        }

        public void mobileEmulation_width(String value) {
            setProperty("mobileEmulation.width", value);
        }

        public void mobileEmulation_height(String value) {
            setProperty("mobileEmulation.height", value);
        }

        public void mobileEmulation_pixelRatio(String value) {
            setProperty("mobileEmulation.pixelRatio", value);
        }

        public void mobileEmulation_userAgent(String value) {
            setProperty("mobileEmulation.userAgent", value);
        }
    }

}
