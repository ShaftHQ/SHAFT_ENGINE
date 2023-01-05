package io.github.shafthq.shaft.properties;

import io.appium.java_client.remote.AutomationName;
import org.aeonbits.owner.Config;
import org.aeonbits.owner.ConfigFactory;

@Config.Sources({"system:properties",
        "file:src/main/resources/properties/MobileCapabilities.properties",
        "file:src/main/resources/properties/default/MobileCapabilities.properties",
        "classpath:MobileCapabilities.properties",
})
public interface Mobile extends EngineProperties {
    //TODO: implement
    @Key("mobile_platformVersion")
    @DefaultValue("")
    String platformVersion();

    @Key("mobile_deviceName")
    @DefaultValue("")
    String deviceName();

    @Key("mobile_automationName")
    @DefaultValue(AutomationName.ANDROID_UIAUTOMATOR2)
    AutomationName automationName();

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

    @Override
    default void setProperty(String key, String value) {
        var updatedProps = new java.util.Properties();
        updatedProps.setProperty(key, value);
        Properties.mobile = ConfigFactory.create(Mobile.class, updatedProps);
    }
}
