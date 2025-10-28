package com.shaft.properties.internal;

import com.shaft.tools.io.ReportManager;
import io.appium.java_client.remote.AutomationName;
import org.aeonbits.owner.Config.Sources;
import org.aeonbits.owner.ConfigFactory;

@SuppressWarnings("unused")
@Sources({"system:properties",
        "file:src/main/resources/properties/MobileCapabilities.properties",
        "file:src/main/resources/properties/default/MobileCapabilities.properties",
        "classpath:MobileCapabilities.properties",
})
public interface Mobile extends EngineProperties<Mobile> {

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.mobile = ConfigFactory.create(Mobile.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    @Key("platformName")
    @DefaultValue("")
    String platformName();

    @Key("mobile_platformVersion")
    @DefaultValue("")
    String platformVersion();

    @Key("mobile_deviceName")
    @DefaultValue("")
    String deviceName();

    @Key("mobile_automationName")
    @DefaultValue(AutomationName.ANDROID_UIAUTOMATOR2)
    String automationName();

    @Key("mobile_udid")
    @DefaultValue("")
    String udid();

    @Key("browserName")
    @DefaultValue("")
    String browserName();

    @Key("MobileBrowserVersion")
    @DefaultValue("")
    String browserVersion();

    @Key("mobile_app")
    @DefaultValue("")
    String app();

    @Key("mobile_appPackage")
    @DefaultValue("")
    String appPackage();

    @Key("mobile_appActivity")
    @DefaultValue("")
    String appActivity();

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public SetProperty selfManagedAndroidSDKVersion(int value) {
            setProperty("selfManagedAndroidSDKVersion", String.valueOf(value));
            return this;
        }

        public SetProperty selfManaged(boolean value) {
            setProperty("selfManaged", String.valueOf(value));
            return this;
        }

        public SetProperty platformName(String value) {
            setProperty("platformName", value);
            return this;
        }

        public SetProperty platformVersion(String value) {
            setProperty("mobile_platformVersion", value);
            return this;
        }

        public SetProperty deviceName(String value) {
            setProperty("mobile_deviceName", value);
            return this;
        }

        /**
         * @param value io.appium.java_client.remote.AutomationName
         */
        public SetProperty automationName(String value) {
            setProperty("mobile_automationName", value);
            return this;
        }

        public SetProperty udid(String value) {
            setProperty("mobile_udid", value);
            return this;
        }

        public SetProperty browserName(String value) {
            setProperty("browserName", value);
            return this;
        }

        public SetProperty browserVersion(String value) {
            setProperty("MobileBrowserVersion", value);
            return this;
        }

        public SetProperty app(String value) {
            setProperty("mobile_app", value);
            return this;
        }

        public SetProperty appPackage(String value) {
            setProperty("mobile_appPackage", value);
            return this;
        }

        public SetProperty appActivity(String value) {
            setProperty("mobile_appActivity", value);
            return this;
        }
    }
}
