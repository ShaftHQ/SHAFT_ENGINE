package com.shaft.internal.properties;

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
public interface Mobile extends EngineProperties {
    @Key("selfManaged")
    @DefaultValue("false")
    boolean selfManaged();

    @Key("selfManagedAndroidSDKVersion")
    @DefaultValue("31")
    int selfManagedAndroidSDKVersion();

    @Key("mobile_platformName")
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

    @Key("mobile_browserName")
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

    private static void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.mobile = ConfigFactory.create(Mobile.class, updatedProps);
        // temporarily set the system property to support hybrid read/write mode
        System.setProperty(key, value);
        ReportManager.logDiscrete("Setting \"" + key + "\" property with \"" + value + "\".");
    }

    default SetProperty set() {
        return new SetProperty();
    }

    class SetProperty implements EngineProperties.SetProperty {

        public void selfManagedAndroidSDKVersion(int value) {
            setProperty("selfManagedAndroidSDKVersion", String.valueOf(value));
        }

        public void selfManaged(boolean value) {
            setProperty("selfManaged", String.valueOf(value));
        }

        public void platformName(String value) {
            setProperty("mobile_platformName", value);
        }

        public void platformVersion(String value) {
            setProperty("mobile_platformVersion", value);
        }

        public void deviceName(String value) {
            setProperty("mobile_deviceName", value);
        }

        /**
         * @param value io.appium.java_client.remote.AutomationName
         */
        public void automationName(String value) {
            setProperty("mobile_automationName", value);
        }

        public void udid(String value) {
            setProperty("mobile_udid", value);
        }

        public void browserName(String value) {
            setProperty("mobile_browserName", value);
        }

        public void browserVersion(String value) {
            setProperty("MobileBrowserVersion", value);
        }

        public void app(String value) {
            setProperty("mobile_app", value);
        }

        public void appPackage(String value) {
            setProperty("mobile_appPackage", value);
        }

        public void appActivity(String value) {
            setProperty("mobile_appActivity", value);
        }
    }
}
