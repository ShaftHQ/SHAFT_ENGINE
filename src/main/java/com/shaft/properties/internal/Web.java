package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties", "file:src/main/resources/properties/WebCapabilities.properties", "file:src/main/resources/properties/default/WebCapabilities.properties", "classpath:WebCapabilities.properties"})
public interface Web extends EngineProperties<Web> {
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

    @Key("forceBrowserDownload")
    @DefaultValue("false")
    boolean forceBrowserDownload();

    @Key("headlessExecution")
    @DefaultValue("false")
    boolean headlessExecution();

    @Key("incognitoMode")
    @DefaultValue("false")
    boolean incognitoMode();

    @Key("isMobileEmulation")
    @DefaultValue("false")
    boolean isMobileEmulation();

    @Key("mobileEmulation.isCustomDevice")
    @DefaultValue("false")
    boolean mobileEmulationIsCustomDevice();

    @Key("mobileEmulation.deviceName")
    @DefaultValue("")
    String mobileEmulationDeviceName();

    @Key("mobileEmulation.width")
    @DefaultValue("")
    int mobileEmulationWidth();

    @Key("mobileEmulation.height")
    @DefaultValue("")
    int mobileEmulationHeight();

    @Key("mobileEmulation.pixelRatio")
    @DefaultValue("1.0")
    double mobileEmulationPixelRatio();

    @Key("mobileEmulation.userAgent")
    @DefaultValue("")
    String mobileEmulationUserAgent();

    @Key("baseURL")
    @DefaultValue("")
    String baseURL();

    @Key("browserWindowWidth")
    @DefaultValue("1920")
    int browserWindowWidth();

    @Key("browserWindowHeight")
    @DefaultValue("1080")
    int browserWindowHeight();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {
        public SetProperty baseURL(String value) {
            setProperty("baseURL", value);
            return this;
        }

        /**
         * @param value io.github.shafthq.shaft.enums.Browsers
         */
        public SetProperty targetBrowserName(String value) {
            setProperty("targetBrowserName", value);
            return this;
        }

        public SetProperty forceBrowserDownload(boolean value) {
            setProperty("forceBrowserDownload", String.valueOf(value));
            return this;
        }

        public SetProperty headlessExecution(boolean value) {
            setProperty("headlessExecution", String.valueOf(value));
            return this;
        }

        public SetProperty incognitoMode(boolean value) {
            setProperty("incognitoMode", String.valueOf(value));
            return this;
        }

        public SetProperty isMobileEmulation(boolean value) {
            setProperty("isMobileEmulation", String.valueOf(value));
            return this;
        }

        public SetProperty mobileEmulationIsCustomDevice(boolean value) {
            setProperty("mobileEmulation.isCustomDevice", String.valueOf(value));
            return this;
        }

        public SetProperty mobileEmulationDeviceName(String value) {
            setProperty("mobileEmulation.deviceName", value);
            return this;
        }

        public SetProperty mobileEmulationWidth(int value) {
            setProperty("mobileEmulation.width", String.valueOf(value));
            return this;
        }

        public SetProperty mobileEmulationHeight(int value) {
            setProperty("mobileEmulation.height", String.valueOf(value));
            return this;
        }

        public SetProperty mobileEmulationPixelRatio(double value) {
            setProperty("mobileEmulation.pixelRatio", String.valueOf(value));
            return this;
        }

        public SetProperty mobileEmulationUserAgent(String value) {
            setProperty("mobileEmulation.userAgent", value);
            return this;
        }

        public SetProperty browserWindowWidth(int value) {
            setProperty("browserWindowWidth", String.valueOf(value));
            return this;
        }

        public SetProperty browserWindowHeight(int value) {
            setProperty("browserWindowHeight", String.valueOf(value));
            return this;
        }
    }

}
